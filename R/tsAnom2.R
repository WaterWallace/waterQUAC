#' Detect anomalies in time series water quality or quantity data
#'
#' This function identifies anomalies in time series data based on:
#' - Values outside expected physical sensor bounds (`sensorMin`/`sensorMax`)
#' - Impossible values (e.g., negative values for non-negative sensors)
#' - Repeated or flatlined values using a rolling standard deviation
#' - Spikes using deviation from rolling median
#'
#' Anomalies are flagged in a "Quality" column (or similarly named column based on what's present in the data).
#'
#' @param df A data frame containing time series data. It must have a timestamp column of class `POSIXct` and a numeric value column (assumed to be the second column).
#' @param overwrite A vector of quality codes that CAN be overwritten by this function (e.g., manually applied flags that can be updated).
#' @param sensorMin Minimum valid value as reportable from the sensor.
#' @param sensorMax Maximum valid value as reportable from the sensor.
#' @param window Approximate number of hours to define a rolling window size for flatline and spike detection.
#' @param prec The precision threshold (standard deviation in the same units as input data. ie. set to 0 for absolute repetitive values, otherwise a small value to allow for some standard deviation in the window) to detect repeated values (flatlines). Values less than `prec` across the window are considered repeating.
#' @param diag Logical; if `TRUE`, diagnostic columns used in detection (e.g., rolling SD and median) will be appended to the output.
#'
#' @return A data frame with the original data and an updated or newly created "Quality" column with anomaly classifications: 'impossible', 'below_limits', 'above_limits', 'repeating_value', 'spike', or 'OK' where no QC flags have been triggered.
#'
#' @note The input data frame (`df`) must include the following columns:
#'
#' | Column Name | Type      | Description                                               |
#' |-------------|-----------|-----------------------------------------------------------|
#' | `ts`        | POSIXct   | Timestamp of each observation                             |
#' | `Value`     | numeric   | Measured value from the sensor                            |
#' | `Quality`   | character | Existing quality flag for the observation (optional)      |
#'
#' All other columns in the input data frame will be preserved in the output.
#' If `diag = TRUE`, additional diagnostic columns (e.g., rolling SD, median) will be appended.
#'
#' @importFrom stats sd median
#' @importFrom zoo rollapply
#' @importFrom tibble tibble
#' @importFrom dplyr mutate case_when bind_cols
#' @importFrom rlang sym
#'
#' @examples
#' library(dplyr)
#' library(zoo)
#' library(plotly)
#'
#' #example Total Suspended Solids dataframe
#' df <- waterQUAC::TSS_data
#'
#' # Apply anomaly detection
#' flagged <- ts_anom2(df = df,
#'                    overwrite = 1:4000,
#'                    sensorMin = 0,
#'                    sensorMax = 2000)
#'
#' # Plot flagged results
#'   plotly::plot_ly(data = flagged) |>
#'     plotly::add_markers(
#'       x = ~ts,
#'       y = ~Value,
#'       color = ~Quality,
#'       marker = list(size = 8)
#'     )
#'
#'
#' @export

SAVED_ts_anom2 <- function(df, overwrite = c(1:4000), sensorMin, sensorMax, window = 10,
                     prec = 0.0001, diag = FALSE, time_threshold_days = 2, log = FALSE)
            {

  impossible_removed <- df %>% dplyr::filter(.[[2]] <= 0)

  below_limits_removed <- df %>% dplyr::filter(.[[2]] < sensorMin)
  above_limits_removed <- df %>% dplyr::filter(.[[2]] > sensorMax)

  # Define the pattern to match variations of "quality"
  pattern <- "(?i)quality"

  # Check if any column name matches the pattern
  if (!any(grepl(pattern, colnames(df)))) {
    df$quality <- NA
  } else {
    q_name <- colnames(df)[grep(pattern, colnames(df))]
  }

  # Find the column name that is of class "posixct"
  posixct_column <- names(df)[sapply(df, function(x) any(class(x) == "POSIXct"))]
  sp <- tibble::tibble(ts = df[[posixct_column]], value = df[[2]])

  duplicates_removed <- removeTSDuplicates(sp, sp$value, output = 1)

  ######################################
  ## Clean data before despiking

  ## remove duplicates
  sp <- removeTSDuplicates(sp, sp$value, output = 0) %>%
    dplyr::filter(value > 0) %>%
    dplyr::filter(value > sensorMin) %>%
    dplyr::filter(value < sensorMax)


  if(log)
  {
    sp$value <- log(sp$value)
  }

#  if(invert)
#  {
#    invertoffset <- max(sp$value %>% na.omit) + 1
#    sp$value <- sp$value * -1 + invertoffset
#  }


  fullsp <- sp

  #multiplier <- ifelse(invert, 1,-1)

  ## Remove the point before a sudden drop
  #sp <- sp %>% mutate(dy = c(diff(as.numeric(ts)/60/60),0)) %>%
  #  mutate(dx = c(diff(value),0)) %>% mutate(dydx = dx/dy) %>%
  # dplyr::filter(dydx > multiplier*10*sd(na.omit(dydx))) %>% # only remove negative drops, 10 standard deviations from normal
  #  dplyr::select(-c(dy, dx, dydx))


  halflife_mins <- 60
  sp$dt <- c(0,diff(sp$ts, units = "mins"))
  sp$dynamic_alpha <- 1 - exp(-sp$dt / halflife_mins)


  # Made this flexible to actually storing the running_mean and running_var in the parquet in the platform
  # That way it won't need to loop from the beginning every time
  if(!("running_mean" %in% names(sp))){
    sp$running_mean <- c(sp$value[1], rep(NA,nrow(sp)-1))
    sp$z_score <- c(0, rep(NA,nrow(sp)-1))
    sp$running_var <- c((sp$value[1] * 0.1)^2, rep(NA,nrow(sp)-1))
  }


  z_threshold <- c(0.001,4)
  spdev <- 0
  weight <- 1
  #min(which(is.na(sp$running_mean)))
  for(row in which(is.na(sp$running_mean)))
  {
    last_running_mean <- sp$running_mean[(row-1)]
    last_running_var <- sp$running_var[(row-1)]

    dynamic_alpha <- sp$dynamic_alpha[row]

    running_mean <- (1 - dynamic_alpha * weight) * last_running_mean + dynamic_alpha * sp$value[row] * weight
    spdev <- (sp$value[row] - running_mean)^2 # squared deviance # move this line below to test back again
    this_var <- (1 - dynamic_alpha * weight) * last_running_var + dynamic_alpha * spdev * weight

    #residual <- sp$value[row] - running_mean # deviance


    # lower the sensitivity
    beta <- 0.2 # introduce some influence of current value to calculate z_score
    last_running_mean <- (1 -  beta * weight  ) * last_running_mean + beta * weight * sp$value[row]

    # z_score determines whether it's a spike or not, >4 or <0.001 is a spike
    z_score <- abs(sp$value[row] - last_running_mean) / max(sqrt(last_running_var), 0.01)
    sp$z_score[row] <- z_score

    #
    weight <- 1
    weight <- ifelse((z_score > z_threshold[2]),0.1,weight)
    weight <- ifelse((z_score < z_threshold[1]),0.5,weight)

    sp$running_mean[row] <- running_mean
    sp$running_var[row] <- this_var

  }





  # transform back for drift detection ( it doesn't like the negatives from log)
#  if(invert)
#  {
#    sp[[2]] <- (sp[[2]] - invertoffset) * -1
#  }

  if(log)
  {
    sp[[2]] <- exp(sp[[2]])
  }

  #plot(sp$z_score)

  # remove z_score spikes
  sp <- sp %>% dplyr::filter(z_score > z_threshold[1] & z_score < z_threshold[2])

  # if it's rising for 2 days
  drift <- sp %>% detect_sensor_drift("value", time_threshold_days = 2, overwrite = overwrite, type = "rising")

  spikesremoved <- fullsp[!(fullsp$ts %in% sp$ts),]
  drift_removed <- drift %>% dplyr::filter(quality == "sensor_drift")

  #cbind(
  #xts(sp$value, sp$ts),# %>% dygraph
  #xts(spikesremoved$value, spikesremoved$ts)) %>% dygraph

  # repeated spikes might be real, so exclude them
  #repeated <- removeTSDuplicates(fullsp, !(fullsp$ts %in% sp$ts))
  #spikesremoved <- spikesremoved[spikesremoved$ts %in% repeated$ts,]

  # Use `!!sym(q_name)` for dynamic column reference
  df <- df %>%
    mutate(
      !!sym(q_name) := ifelse(!is.na(.data[[q_name]]) & !(.data[[q_name]] %in% overwrite),
                              as.character(.data[[q_name]]),
                              NA_character_)
    )

  df <- df %>%
    mutate(
      !!sym(q_name) := case_when(
        .data[[posixct_column]] %in% impossible_removed[[1]] ~ 'impossible',
        .data[[posixct_column]] %in% below_limits_removed[[1]] ~ 'below_limits',
        .data[[posixct_column]] %in% above_limits_removed[[1]] ~ 'above_limits',
        .data[[posixct_column]] %in% duplicates_removed$ts ~ 'repeating_value',
        .data[[posixct_column]] %in% drift_removed$ts ~ 'sensor_drift',
        .data[[posixct_column]] %in% spikesremoved$ts ~ 'spike',
        TRUE ~ "OK"
      )
    )

  #df$impossible <- df[[posixct_column]] %in% impossible_removed[[1]]
  #df$below <- df[[posixct_column]] %in% below_limits_removed[[1]]
  #df$above <- df[[posixct_column]] %in% above_limits_removed[[1]]
  #df$dupe <- df[[posixct_column]] %in% duplicates_removed[[1]]
  #df$drift <- df[[posixct_column]] %in% drift_removed[[1]]
  #df$spike <- df[[posixct_column]] %in% spikesremoved[[1]]
  #View(df)

  #spikes <- df %>% dplyr::filter(Quality == "spike")
  #nospikes <- df %>% dplyr::filter(Quality == "OK")

  #cbind(
  #xts(spikes$Value, spikes$ts),
  #xts(nospikes$Value, nospikes$ts)) %>% dygraph

  #cbind(
  #  xts(sp$value, sp$ts),# %>% dygraph
  #  xts(spikesremoved$value, spikesremoved$ts)) %>% dygraph


  # infill "spikes" in drift points
  driftpoints <- lead(df[[sym(q_name)]],1) == "sensor_drift" & lag(df[[sym(q_name)]],1) == "sensor_drift"
  df[[sym(q_name)]][driftpoints] <- "sensor_drift"

  # Optionally include sp columns in output
  if (diag) {
    df <- bind_cols(df, sp[, -1])  # drop 'ts' column from sp to avoid duplication
  }

  #plot(df$Value)
  #if(invert)
  #{
  #  df[[2]] <- (df[[2]] - invertoffset) * -1
  #}

  #if(log)
  #{
  #  df[[2]] <- exp(df[[2]])
  #}

  return(df)

  #plot(df$Quality == "spike")
  #df %>% ggplot(aes(x = ts, y  = Value, colour = Quality)) +
  #  geom_point()




}

# standalone real time spike detection
spike_detection <- function(ts,
                            value,
                            last_t = 0,
                            last_running_mean = NULL,
                            last_running_var = NULL,
                            last_dev = NULL,
                            halflife_mins = 120,
                            z_threshold = c(0.001,4))
{

  if(is.null(last_running_mean)) last_running_mean <- value
  if(is.null(last_running_var)) last_running_var <- (0.1*value)^2
  if(is.null(last_dev)) last_dev <- (0.1*value)

  # to run this real time spike detection, the only values it needs are these 4 values
  #last_t <- sp$ts[(row-1)] # last time
  #last_running_mean <- sp$running_mean[(row-1)] # running mean
  #last_running_var <- sp$running_var[(row-1)] # variance
  #last_dev <- sp$dev[(row-1)] # squared deviance

  # exponentional decay
  dt <- difftime(ts, last_t, units = "mins") %>% as.numeric # change in time
  dynamic_alpha <- 1 - exp(-dt / halflife_mins) # exponentional decay factor

  # apply exponential decay, older values have less influence
  # daily values are almost entirely independent
  # 15 minute value are worth about 10% of the weighted mean
  running_mean <- (1 - dynamic_alpha ) * last_running_mean + dynamic_alpha * value
  this_var <- (1 - dynamic_alpha) * last_running_var + dynamic_alpha * last_dev
  this_dev <- ( value - running_mean )^2 # squared deviance lagged

  # z_score determines whether it's a spike or not, >4 or <0.001 is a spike
  # not strictly a z score, but calculated on a variance calculated from a lagged squared deviance
  # so it's the deviance from the mean, divided by the standard deviation (sqrt of variance)

  # e.g. for z_score: if deviance / stddev = 4 that's like deviance = 4 * stddev, just neater
  z_score <- abs( value - running_mean) / max(sqrt(this_var), 0.01)
  # update z_score each time

  # if z score is outside the thresholds
  # then don't update the running mean
  spike <- (z_score > z_threshold[2] | z_score < z_threshold[1] )
  weight <- ifelse(spike, 0.1, 1)

  running_mean <- (1 - weight) * last_running_mean + weight * running_mean
  this_var <- (1 - weight) * last_running_var + weight * this_var

  return(data.frame(ts = ts, value = value, mean = running_mean, var = this_var, dev = this_dev, z_score, spike))
}

ts_anom2 <- function(df, overwrite = c(1:4000), sensorMin, sensorMax, window = 10,
                     prec = 0.0001, diag = FALSE, time_threshold_days = 2, log = FALSE, invert = FALSE)
{
  # override these for now because they won't work in real time.
  # But performance should improve when they are used where required
  #log <- FALSE
  #invert <- FALSE

  impossible_removed <- df %>% dplyr::filter(.[[2]] <= 0)

  below_limits_removed <- df %>% dplyr::filter(.[[2]] < sensorMin)
  above_limits_removed <- df %>% dplyr::filter(.[[2]] > sensorMax)

  # Define the pattern to match variations of "quality"
  pattern <- "(?i)quality"

  # Check if any column name matches the pattern
  if (!any(grepl(pattern, colnames(df)))) {
    df$quality <- NA
  } else {
    q_name <- colnames(df)[grep(pattern, colnames(df))]
  }

  # Find the column name that is of class "posixct"
  posixct_column <- names(df)[sapply(df, function(x) any(class(x) == "POSIXct"))]
  sp <- tibble::tibble(ts = df[[posixct_column]], value = df[[2]])

  duplicates_removed <- removeTSDuplicates(sp, sp$value, output = 1)

  ######################################
  ## Clean data before despiking

  ## remove duplicates
  sp <- removeTSDuplicates(sp, sp$value, output = 0) %>%
    dplyr::filter(value > 0) %>%
    dplyr::filter(value > sensorMin) %>%
    dplyr::filter(value < sensorMax)

  message("finished dedeupe")

  fullsp <- sp

  if(invert)
  {
    invertoffset <- sensorMax + 1
    sp$value <- sp$value * -1 + invertoffset
  }

  if(log)
  {
    sp$value <- log(sp$value)
  }


  #plot(sp$value)

  #multiplier <- ifelse(invert, 1,-1)

  ## Remove the point before a sudden drop
  #sp <- sp %>% mutate(dy = c(diff(as.numeric(ts)/60/60),0)) %>%
  #  mutate(dx = c(diff(value),0)) %>% mutate(dydx = dx/dy) %>%
  # dplyr::filter(dydx > multiplier*10*sd(na.omit(dydx))) %>% # only remove negative drops, 10 standard deviations from normal
  #  dplyr::select(-c(dy, dx, dydx))

  last_t <- 0 # last time
  last_running_mean <- sp$mean[1] # running mean
  last_running_var <- sp$var[1] # variance
  last_dev <- sp$dev[1] # squared deviance

  despike_batch <- list()
  despiked <- list()

  #sp <- na.omit(df)
  for(row in 2:nrow(sp))
  {
    # loop through each point
    spike_detect <- spike_detection(ts = sp$ts[row], value = sp$value[row], last_t, last_running_mean, last_running_var, last_dev)
    last_t <- spike_detect$ts
    last_running_mean <- spike_detect$mean
    last_running_var <- spike_detect$var
    last_dev <- spike_detect$dev
    despiked[[length(despiked) + 1]] <- spike_detect
  }
  despike_batch[["last"]] <- spike_detect
  despike_batch[["data"]] <- rbindlist(despiked)


  message("finished loop")
  #sp$z_score %>% quantile(5/100)
  #cbind(
  #xts(sp$value, sp$ts),
  #xts(sp$running_mean, sp$ts),
  #xts(sp$running_mean+max(sqrt(this_var), 0.01)*4, sp$ts),
  #xts(sp$running_mean-max(sqrt(this_var), 0.01)*4, sp$ts)
#
#  ) %>% dygraph

  # transform back for drift detection ( it doesn't like the negatives from log)
  #if(log)
  #{
  #  sp[[2]] <- exp(sp[[2]])
  #}

  #if(invert)
  #{
  #  sp[[2]] <- (sp[[2]] - invertoffset) * -1
  #}


  #plot(sp$z_score)

  # remove z_score spikes
  #sp <- sp %>% dplyr::filter(z_score > z_threshold[1] & z_score < z_threshold[2])



  #return(sp)
  # if it's rising for 2 days
  drift <- despike_batch[["data"]] %>% dplyr::filter(spike == FALSE) %>%
    dplyr::select(ts, value) %>%
    detect_sensor_drift("value", time_threshold_days = 2, overwrite = overwrite, type = "rising")

  despiked_ts <- despike_batch[["data"]] %>%
    dplyr::filter(spike == FALSE) %>%
    pull(ts)

  spikesremoved <- fullsp[!(fullsp$ts %in% despiked_ts),]
  drift_removed <- drift %>% dplyr::filter(quality == "sensor_drift")

  tail(drift)
  despike_batch$last

  message("finished drift")
  #cbind(
  #xts(sp$value, sp$ts),# %>% dygraph
  #xts(spikesremoved$value, spikesremoved$ts)) %>% dygraph

  # repeated spikes might be real, so exclude them
  #repeated <- removeTSDuplicates(fullsp, !(fullsp$ts %in% sp$ts))
  #spikesremoved <- spikesremoved[spikesremoved$ts %in% repeated$ts,]

  # Use `!!sym(q_name)` for dynamic column reference
  df <- df %>%
    mutate(
      !!sym(q_name) := ifelse(!is.na(.data[[q_name]]) & !(.data[[q_name]] %in% overwrite),
                              as.character(.data[[q_name]]),
                              NA_character_)
    )

  df <- df %>%
    mutate(
      !!sym(q_name) := case_when(
        .data[[posixct_column]] %in% impossible_removed[[1]] ~ 'impossible',
        .data[[posixct_column]] %in% below_limits_removed[[1]] ~ 'below_limits',
        .data[[posixct_column]] %in% above_limits_removed[[1]] ~ 'above_limits',
        .data[[posixct_column]] %in% duplicates_removed$ts ~ 'repeating_value',
        .data[[posixct_column]] %in% drift_removed$ts ~ 'sensor_drift',
        .data[[posixct_column]] %in% spikesremoved$ts ~ 'spike',
        TRUE ~ "OK"
      )
    )

  #df$impossible <- df[[posixct_column]] %in% impossible_removed[[1]]
  #df$below <- df[[posixct_column]] %in% below_limits_removed[[1]]
  #df$above <- df[[posixct_column]] %in% above_limits_removed[[1]]
  #df$dupe <- df[[posixct_column]] %in% duplicates_removed[[1]]
  #df$drift <- df[[posixct_column]] %in% drift_removed[[1]]
  #df$spike <- df[[posixct_column]] %in% spikesremoved[[1]]
  #View(df)

  #spikes <- df %>% dplyr::filter(Quality == "spike")
  #nospikes <- df %>% dplyr::filter(Quality == "OK")

  #cbind(
  #xts(spikes$Value, spikes$ts),
  #xts(nospikes$Value, nospikes$ts)) %>% dygraph

  #cbind(
  #  xts(sp$value, sp$ts),# %>% dygraph
  #  xts(spikesremoved$value, spikesremoved$ts)) %>% dygraph


  # infill "spikes" in drift points
  driftpoints <- lead(df[[sym(q_name)]],1) == "sensor_drift" & lag(df[[sym(q_name)]],1) == "sensor_drift"
  df[[sym(q_name)]][driftpoints] <- "sensor_drift"

  # Optionally include sp columns in output
  if (diag) {
    df <- bind_cols(df, sp[, -1])  # drop 'ts' column from sp to avoid duplication
  }

  #plot(df$Value)
  #if(invert)
  #{
  #  df[[2]] <- (df[[2]] - invertoffset) * -1
  #}

  #if(log)
  #{
  #  df[[2]] <- exp(df[[2]])
  #}

  return(df)

  #plot(df$Quality == "spike")
  #df %>% ggplot(aes(x = ts, y  = Value, colour = Quality)) +
  #  geom_point()




}





plot(despike_batch$data$z_score, log = "y")
abline(h=4, col="blue")
abline(h=0.001, col="blue")

#View(despike_batch[["data"]])
#tail(despike_batch$data)




