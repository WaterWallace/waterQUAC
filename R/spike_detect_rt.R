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
#' flagged <- ts_anom(df = df,
#'                    overwrite = 1:4000,
#'                    sensorMin = 0,
#'                    sensorMax = 10)
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
#' @export
spike_detect_rt <- function(ts,
                            value,
                            last_t = 0,
                            last_running_mean = NULL,
                            last_running_var = NULL,
                            last_dev = NULL,
                            halflife_mins = 120,
                            z_threshold = c(0.001,4))
{
  # to run this real time spike detection, the only values it needs for memory are these 4 values

  if(is.null(last_running_mean)) last_running_mean <- value
  if(is.null(last_running_var)) last_running_var <- (0.1*value)^2
  if(is.null(last_dev)) last_dev <- (0.1*value)

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
