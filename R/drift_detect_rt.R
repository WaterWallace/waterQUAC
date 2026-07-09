#' Detect gradual sensor drift in time series water quality data
#'
#' This function detects sustained periods where sensor readings remain consistently
#' above a calculated threshold, which may indicate sensor drift. The threshold is
#' defined as a multiple of the median value, and drift is flagged if readings exceed
#' this threshold continuously for longer than a specified number of days.
#'
#' Depending on the value of "type", the function will accumulate either rising or falling points, or both.
#'
#' @param ts POSIXCt This is the timestamp of the current observation.
#' @param value Numeric This is the sensor value of the current observation.
#' @param last_t POSIXCt This is the timestamp of the last observation.
#' @param last_value Numeric This is the sensor value of the last observation.
#' @param threshold_multiplier A numeric multiplier applied to the moving average value to define the drift threshold. Default is 1.1.
#' @param time_threshold_days Minimum number of **continuous** days above the threshold required to flag sensor drift. Default is 2 days.
#' @param halflife Numeric The halflife (in days) for the exponential decay used in calculating the dynamic mean. Default is 120 days.
#' @param type A string indicating the type of drift to detect: "rising", "falling", or NULL for both. Default is NULL.
#' @param last_mean Numeric The mean value from the previous observation, used for dynamic threshold
#' @param cumulative_time Numeric The cumulative time (in days) above the threshold from the previous observation.
#'
#' @return A data frame with all original columns, a new column `cumulative_time_above_threshold`
#'         (in days), and an updated or added `Quality` column with `"sensor_drift"` flags.
#'
#' @examples
#' library(dplyr)
#' # Simulated data
#' set.seed(42)
#' ts <- seq.POSIXt(from = as.POSIXct("2024-01-01"), by = "hour", length.out = 200)
#' val <- c(rnorm(150, mean = 2), rep(6, 50))  # final 50 points simulate drift
#' df <- data.frame(ts = ts, Value = val, Quality = NA_character_)
#'
#' # Apply drift detection
#' result <- detect_sensor_drift(df, value_col = "Value")
#'
#' # Visualize
#' if (requireNamespace("plotly", quietly = TRUE)) {
#'   plotly::plot_ly(result) |>
#'     plotly::add_markers(
#'       x = ~ts,
#'       y = ~Value,
#'       type = "scatter",
#'       color = ~Quality
#'     )
#' }
#'
#' @export
drift_detect_rt <- function(ts, value, last_t, last_value,
                            threshold_multiplier = 1.1, time_threshold_days = 2, halflife = 120,
                            type = NULL,
                            last_mean = NULL, cumulative_time = NULL)
{

  if(is.null(last_mean))  last_mean <- value
  if(is.null(cumulative_time))  cumulative_time <- value

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
