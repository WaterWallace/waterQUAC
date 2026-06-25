#' Detect gradual sensor drift in time series water quality data
#'
#' This function detects sustained periods where sensor readings remain consistently
#' above a calculated threshold, which may indicate sensor drift. The threshold is
#' defined as a multiple of the median value, and drift is flagged if readings exceed
#' this threshold continuously for longer than a specified number of days.
#'
#' If a column matching "quality" (case-insensitive) exists in the data, drift flags
#' will be written to it. Otherwise, a new column `quality` is created.
#'
#' @param data A data frame containing time series data.
#' @param value_col A string. The name of the column containing numeric sensor values.
#' @param time_col A string. The name of the column containing POSIXct timestamps.
#' @param threshold_multiplier A numeric multiplier applied to the median value to define the drift threshold. Default is 2.
#' @param time_threshold_days Minimum number of **continuous** days above the threshold required to flag sensor drift. Default is 5 days.
#'
#' @return A data frame with all original columns, a new column `cumulative_time_above_threshold`
#'         (in days), and an updated or added `Quality` column with `"sensor_drift"` flags.
#'
#' @note The input data frame (`data`) must include:
#'
#' | Column Name | Type      | Description                                       |
#' |-------------|-----------|---------------------------------------------------|
#' | `<time_col>` | POSIXct   | Timestamp of each observation                     |
#' | `<value_col>`| numeric   | Measured sensor value                             |
#' | `Quality`   | character | Optional; if present, will be used/updated        |
#'
#' All other columns in the input data frame will be preserved in the output.
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
