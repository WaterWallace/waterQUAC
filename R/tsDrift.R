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

detect_sensor_drift <- function(data, value_col, threshold_multiplier = 2, time_threshold_days = 5, overwrite = NULL) {
  # Find the column name that is of class "posixct"
  time_col <- names(data)[sapply(data, function(x) any(class(x) == "POSIXct"))]

  # Ensure the data is sorted by time
  data <- data[order(data[[time_col]]), ]



  # Calculate the threshold based on the multiplier
  threshold <- threshold_multiplier * median(data[[value_col]], na.rm = TRUE)

  # Calculate time difference in seconds
  data$time_diff <- c(NA, diff(as.numeric(as.POSIXct(data[[time_col]]))))

  # Compute cumulative time above threshold (in days)
  cumulative_time <- 0
  cumulative_times <- numeric(nrow(data))

  for (i in seq_len(nrow(data))) {
    if (!is.na(data[[value_col]][i]) && data[[value_col]][i] > threshold) {
      cumulative_time <- cumulative_time + ifelse(!is.na(data$time_diff[i]), data$time_diff[i], 0)
    } else {
      cumulative_time <- 0
    }
    cumulative_times[i] <- cumulative_time / 86400  # convert to days
  }

  # Store cumulative time in the data
  data$cumulative_time_above_threshold <- cumulative_times

  # Detect or create quality column
  pattern <- "(?i)quality"
  if (!any(grepl(pattern, colnames(data)))) {
    data$quality <- NA
    quality_col <- "quality"
  } else {
    quality_col <- colnames(data)[grep(pattern, colnames(data))[1]]  # first match
  }

  # Flag sensor drift conditionally, using overwrite logic
  should_flag <- data$cumulative_time_above_threshold > time_threshold_days
  can_overwrite <- is.na(data[[quality_col]]) |
    (!is.null(overwrite) & data[[quality_col]] %in% overwrite)

  data[[quality_col]][should_flag & can_overwrite] <- "sensor_drift"

  return(data)
}
