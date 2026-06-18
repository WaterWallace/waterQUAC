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

ts_anom2 <- function(df, overwrite = c(1:4000), sensorMin, sensorMax, window = 10,
                     prec = 0.0001, diag = FALSE, time_threshold_days = 2)
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

  if(invert)
  {
    invertoffset <- max(sp$value %>% na.omit) + 1
    sp$value <- sp$value * -1 + invertoffset
  }


  fullsp <- sp

  multiplier <- ifelse(invert, 1,-1)

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
  #min(which(is.na(sp$running_mean)))
  for(row in which(is.na(sp$running_mean)))
  {
    last_running_mean <- sp$running_mean[(row-1)]
    last_running_var <- sp$running_var[(row-1)]

    dynamic_alpha <- sp$dynamic_alpha[row]

    running_mean <- (1 - dynamic_alpha) * last_running_mean + dynamic_alpha * sp$value[row]
    this_var <- (1 - dynamic_alpha) * last_running_var + dynamic_alpha *  spdev

    spdev <- (sp$value[row] - running_mean)^2 # squared deviance
    #residual <- sp$value[row] - running_mean # deviance


    # z_score determines whether it's a spike or not, >4 or <0.001 is a spike
    z_score <- abs(sp$value[row] - running_mean) / max(sqrt(this_var), 0.01)
    sp$z_score[row] <- z_score

    if(z_score > z_threshold[2] | z_score < z_threshold[1] ){
      sp$running_mean[row] <- last_running_mean
      sp$running_var[row] <- last_running_var
    }else{ # don't update mean and variance on a spike
      sp$running_mean[row] <- running_mean
      sp$running_var[row] <- this_var
    }
  }





  # transform back for drift detection ( it doesn't like the negatives from log)
  if(invert)
  {
    sp[[2]] <- (sp[[2]] - invertoffset) * -1
  }

  if(log)
  {
    sp[[2]] <- exp(sp[[2]])
  }


  # remove z_score spikes
  high <- sp %>% dplyr::filter(z_score > z_threshold[2])
  # remove z_score spikes
  low <- sp %>% dplyr::filter(z_score < z_threshold[1])

  # remove z_score spikes
  sp <- sp %>% dplyr::filter(z_score > z_threshold[1] & z_score < z_threshold[2])

  cbind(xts(sp$value, sp$ts),
        xts(high$value, high$ts),
        xts(low$value, low$ts)
  ) %>% dygraph

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

df <- waterQUAC::TSS_data
flagged <- ts_anom2(df = df,
                    overwrite = 1:4000,
                    sensorMin = 0,
                    sensorMax = 2000)

unique(processed$Quality)
flagged %>% ggplot(aes(x=ts, y=Value, colour=Quality)) +
  geom_point() +
  labs(title = "EC JRC (all QC)", y = "EC microsiemens")


anom_cols <- function(df, overwrite = c(1:4000), sensorMin, sensorMax, window = 10,
                     prec = 0.0001, diag = FALSE, time_threshold_days = 2)
{

  df$flag <- NA
  # function applies rules based filters, and adds columns to filter by


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
  value_col <- names(df)[2]

  sp <- df %>% dplyr::select(posixct_column, value_col)

  #sp <- tibble::tibble(ts = df[[posixct_column]], value = df[[2]])

  duplicates_removed <- removeTSDuplicates(sp, sp[[value_col]], output = 1)

  # apply rules
  df <- df %>% mutate(flag = ifelse(.[[2]] > sensorMax, "above_max", flag)) %>%
    mutate(flag = ifelse(.[[2]] < sensorMin, "below_min", flag)) %>%
    mutate(flag = ifelse(.[[2]] <= 0, "impossible", flag)) %>%
    mutate(flag = ifelse(.data[[posixct_column]] %in%  duplicates_removed[[posixct_column]], "dupe", flag)) %>%
    mutate(flag = ifelse(is.na(.data[[value_col]]), "Blank",flag ))

  fulldf <- df
  df <- df %>% dplyr::filter(is.na(flag))

  ######################################
  ## Clean data before despiking
  sp <- sp %>% dplyr::filter(ts %in% df[[posixct_column]])

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
    sp$running_mean <- c(sp[[value_col]][1], rep(NA,nrow(sp)-1))
    sp$z_score <- c(0, rep(NA,nrow(sp)-1))
    sp$running_var <- c((sp[[value_col]][1] * 0.1)^2, rep(NA,nrow(sp)-1))
  }


  z_threshold <- c(0.001,4)
  #min(which(is.na(sp$running_mean)))
  for(row in which(is.na(sp$running_mean)))
  {
    last_running_mean <- sp$running_mean[(row-1)]
    last_running_var <- sp$running_var[(row-1)]

    dynamic_alpha <- sp$dynamic_alpha[row]

    # z_score determines whether it's a spike or not, >4 or <0.001 is a spike
    z_score <- abs( sp[[value_col]][row] - last_running_mean) / max(sqrt(last_running_var), 0.01)
    sp$z_score[row] <- z_score

    running_mean <- (1 - dynamic_alpha) * last_running_mean + dynamic_alpha * sp[[value_col]][row]
    spdev <- ( sp[[value_col]][row] - running_mean)^2 # squared deviance
    this_var <- (1 - dynamic_alpha) * last_running_var + dynamic_alpha *  spdev

    #residual <- sp$value[row] - running_mean # deviance

    if(z_score > z_threshold[2] | z_score < z_threshold[1] ){
      sp$running_mean[row] <- last_running_mean
      sp$running_var[row] <- last_running_var
    }else{ # don't update mean and variance on a spike
      sp$running_mean[row] <- running_mean
      sp$running_var[row] <- this_var
    }
  }


  # remove z_score spikes
  high <- sp %>% dplyr::filter(z_score > z_threshold[2])
  # remove z_score spikes
  low <- sp %>% dplyr::filter(z_score < z_threshold[1])


  cbind(xts(sp[[value_col]], sp$ts),
        xts(high[[value_col]], high$ts),
        xts(low[[value_col]], low$ts)
        ) %>% dygraph



  #cbind(xts(sp$value, sp$ts),
  #      xts(high$value, high$ts),
  #      xts(low$value, low$ts)
  #      ) %>% dygraph

  # transform back for drift detection ( it doesn't like the negatives from log)
  if(invert)
  {
    sp[[2]] <- (sp[[2]] - invertoffset) * -1
  }

  if(log)
  {
    sp[[2]] <- exp(sp[[2]])
  }


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




