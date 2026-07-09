#' Real time anomaly detection for time series water quality or quantity data
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
#'
#' @return
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
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate case_when bind_cols
#' @importFrom rlang sym
#' @importFrom data.table rbindlist
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
drift_detect_rt <- function(ts, value, last_t, last_value,
                                   last_mean = NULL, cumulative_time = NULL,
                                   threshold_multiplier = 1.1, time_threshold_days = 2, halflife = 120,
                                   type = NULL)
{


  if(is.null(last_mean))  last_mean <- value

  # exponentional decay
  dt <- difftime(ts, last_t, units = "days") %>% as.numeric # change in time
  dynamic_alpha <- 1 - exp(-dt / 120) # exponentional decay factor, with a 30 day halflife

  #
  last_mean <- ( (1 - dynamic_alpha ) * last_mean + dynamic_alpha * value )
  threshold <- threshold_multiplier * last_mean

  # increment cumulative_time
  if(value > threshold)
  {
    dt <- min(1, dt) # prevent long gaps instantly being drift
    if(type == "rising")
    {
      if ( value > last_value ){
        cumulative_time <- cumulative_time + dt # cumulative time in days
      }
    }else{
      cumulative_time <- cumulative_time + dt # cumulative time in days
    }
  }else{
    cumulative_time <- 0
  }

  drift <- ( cumulative_time > time_threshold_days )

  data.frame(ts = ts, value = value, cumulative_time = cumulative_time, mean = last_mean, drift = drift)

}


# standalone real time spike detection
spike_detect_rt <- function(ts,
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

  this_dev <- abs( value - running_mean )#^2 #

  # normal (good)
  #this_var <- (1 - dynamic_alpha) * last_running_var + dynamic_alpha * ( last_dev )^2

  # weighted
  this_var <- (1 - dynamic_alpha) * last_running_var + dynamic_alpha * ( 0.9 * last_dev + 0.1 * this_dev )^2

  #if(this_dev < 0.001 | last_dev  < 0.001)
  #{
  #  this_var <- (1 - dynamic_alpha) * last_running_var + dynamic_alpha * ( last_dev )^2
  #}else{
  #  # experimental
  #  extended_dev <- last_dev * exp(-log(this_dev/last_dev) * dt / halflife_mins)
  #  extended_dev <- max(0.001, extended_dev)
  #  this_var <- (1 - dynamic_alpha) * last_running_var + dynamic_alpha * ( extended_dev )^2
  #}

  #if(is.infinite(this_var)) break
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

ts_anom_rt <- function(df, overwrite = c(1:4000), sensorMin, sensorMax,
                     time_threshold_days = 2, log = FALSE, invert = FALSE,
                     last_values = NULL)
{

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

  value_column <- names(df)[2]

  # default values if no last_values input.
  # If no last_values, then re-run from start.
  if (is.null(last_values)){
    last_t <- 0 # last time
    last_running_mean <- df[[value_column]][1] # running mean
    last_dev <- df[[value_column]][1]*0.1 # squared deviance
    last_running_var <- (last_dev)^2 # variance

    # defaults for drift
    last_value <- df[[value_column]][1] # To check if level rising
    last_drift_t <- 0
    last_drift_mean <- df[[value_column]][1]
    cumulative_time <- 0
  }else{
    last_t <- last_values$ts # last time
    last_running_mean <- last_values$mean # running mean
    last_running_var <- last_values$var # variance
    last_dev <- last_values$dev # squared deviance

    # defaults for drift
    last_value <- last_values$drift_value
    last_drift_t <- last_values$drift_ts
    last_drift_mean <- last_values$drift_mean
    cumulative_time <- last_values$drift_cumulative_time

    df <- df %>% dplyr::filter(ts > last_t)
    if( nrow(df) == 0 ) return (NULL) # blank, exit

  }

  # Apply rules
  impossible_removed <- df %>% dplyr::filter(.[[2]] <= 0)
  below_limits_removed <- df %>% dplyr::filter(.[[2]] < sensorMin)
  above_limits_removed <- df %>% dplyr::filter(.[[2]] > sensorMax)

  sp <- tibble::tibble(ts = df[[posixct_column]], value = df[[value_column]])
  #duplicates_removed <- removeTSDuplicates(sp, sp$value, output = 1)

  ######################################
  ## Clean data before despiking

  ## # skip remove Duplicates because the spike detection removes them
  sp <- sp %>% #removeTSDuplicates(sp, sp$value, output = 0) %>%
    dplyr::filter(value > 0) %>%
    dplyr::filter(value > sensorMin) %>%
    dplyr::filter(value < sensorMax)

  #fullsp <- sp

  if(invert)
  {
    invertoffset <- sensorMax + 1
    sp$value <- sp$value * -1 + invertoffset
  }

  if(log)
  {
    sp$value <- log(sp$value)
  }


  #despike_batch <- list()
  despiked <- list()
  dedrift <- list()

  #sp <- na.omit(df)
  for(row in 1:nrow(sp))
  {
    # loop through each point
    spike_detect <- spike_detect_rt(ts = sp$ts[row], value = sp$value[row], last_t,
                                    last_running_mean, last_running_var, last_dev)

    if(!spike_detect$spike){ # only test if drift if not a spike
      drift_detect <- drift_detect_rt (sp$ts[row], sp$value[row], last_drift_t, last_value,
                              last_drift_mean, cumulative_time,
                              threshold_multiplier = 2, time_threshold_days = time_threshold_days,
                              type = "rising")

      # update recursive inputs
      last_value <- sp$value[row]
      last_drift_t <- sp$ts[row]
      last_drift_mean <- drift_detect$mean
      cumulative_time <- drift_detect$cumulative_time

      # store drift detected
      if(drift_detect$drift)  dedrift[[length(dedrift) + 1]] <- drift_detect
      #dedrift[[length(dedrift) + 1]] <- drift_detect
    }

    # update recursive inputs
    last_t <- spike_detect$ts
    last_running_mean <- spike_detect$mean
    last_running_var <- spike_detect$var
    last_dev <- spike_detect$dev

    # store spike detected
    if(spike_detect$spike)  despiked[[length(despiked) + 1]] <- spike_detect
    #despiked[[length(despiked) + 1]] <- spike_detect

  }

  # timestamps of flagged data
  spikesremoved <-  rbindlist(despiked)
  drift_removed <- rbindlist(dedrift)

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

  # infill "spikes" in drift points
  driftpoints <- lead(df[[sym(q_name)]],1) == "sensor_drift" & lag(df[[sym(q_name)]],1) == "sensor_drift"
  df[[sym(q_name)]][driftpoints] <- "sensor_drift"

  # Optionally include sp columns in output
  #if (diag) {
  #  df <- bind_cols(df, sp[, -1])  # drop 'ts' column from sp to avoid duplication
  #}

  names(drift_detect) <- paste0("drift_",names(drift_detect))
  #bind_cols(spike_detect, drift_detect)

  list(
    last_values = bind_cols(spike_detect, drift_detect),
    data = df
  )

  #return(df)

  #plot(df$Quality == "spike")
  #df %>% ggplot(aes(x = ts, y  = Value, colour = Quality)) +
  #  geom_point()


}


df <- waterQUAC::TSS_data


# Full period of record (slow)
# Process for comparison
fulldata <- df %>% ts_anom_rt(overwrite = c(1:4000), sensorMin = 0.001, sensorMax = 2000, last_values = NULL)

# plot spikes/drift removed
fulldata$data %>% ggplot(aes(x = ts, y = Value, colour = Quality)) +
  geom_point()


# Process one month of data
# First Month
onemonth <- df %>% dplyr::filter(ts < (df$ts[1] + 30*60*60*24)) %>%
  ts_anom_rt(overwrite = c(1:4000), sensorMin = 0.001, sensorMax = 2000, last_values = NULL)


# First Month --- Input last_values returned from last run
print(onemonth$last_values)

# Now process the next month
secondmonth <- df %>% dplyr::filter(ts > onemonth$last_values$ts) %>% # don't really have to chop this, ts_anom_rt takes care of it
  dplyr::filter(ts < (df$ts[1] + 60*60*60*24)) %>%
  ts_anom_rt(overwrite = c(1:4000), sensorMin = 0.001, sensorMax = 2000,
             last_values = onemonth$last_values) # last_values was returned from the last run


# join second month onto the full series data
result <- fulldata$data %>%
  left_join(secondmonth$data %>% select(ts, Quality), by = "ts") %>% na.omit

# confirming all points the same
# sum of zero means there's no difference
sum(result$Quality.x != result$Quality.y)

# Second month --- Input last_values returned from last run
print(secondmonth$last_values)

# doing 10 months of data so slower
thirdblock <- df %>% dplyr::filter(ts > secondmonth$last_values$ts) %>% # don't really have to chop this, ts_anom_rt takes care of it
  dplyr::filter(ts < (df$ts[1] + 365*60*60*24)) %>%
  ts_anom_rt(overwrite = c(1:4000), sensorMin = 0.001, sensorMax = 2000,
  last_values = secondmonth$last_values)

# plot spikes/drift removed
thirdblock$data %>% ggplot(aes(x = ts, y = Value, colour = Quality)) +
  geom_point()

# join 3rd block onto the full series data
result <- fulldata$data %>%
  left_join(thirdblock$data %>% select(ts, Quality), by = "ts") %>% na.omit

# confirming all points the same
# sum of zero means there's no difference
sum(result$Quality.x != result$Quality.y)

# but it's really just for adding an hour of data per time
df %>% dplyr::filter(ts > thirdblock$last_values$ts)

# new data from eagle.io
new_eio_data <- df %>% dplyr::filter(ts > thirdblock$last_values$ts & ts < thirdblock$last_values$ts + 60*60 )

# last hour only 3 data points
print(new_eio_data)

# Very fast
starttime <- Sys.time()
new_data_qc <- new_eio_data %>%
 ts_anom_rt(overwrite = c(1:4000), sensorMin = 0.001, sensorMax = 2000, last_values = thirdblock$last_values)
Sys.time() - starttime

new_data_qc

# and another hour
starttime <- Sys.time()
new_eio_data <- df %>% dplyr::filter(ts > new_data_qc$last_values$ts & ts < new_data_qc$last_values$ts + 60*60 )
new_data_qc <- new_eio_data %>%
  ts_anom_rt(overwrite = c(1:4000), sensorMin = 0.001, sensorMax = 2000, last_values = thirdblock$last_values)
Sys.time() - starttime

new_data_qc






















