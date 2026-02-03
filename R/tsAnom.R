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
#'
ts_anom <- function(df, overwrite, sensorMin, sensorMax, window = 10, prec = 0.0001, diag = FALSE, output=0) {


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

  sp <- tibble::tibble(ts = df[[posixct_column]])

  #df[[posixct_column]])
  #sp15 <- changeInterval(sp %>% as.data.frame, Interval = 15)

  # Calculate the time differences between consecutive timestamps
  time_diff <- diff(sp[["ts"]])

  # Calculate the average data logging interval per day and reduce it to match the defined window. interval = points per window
  interval <- round(((1440 / as.numeric(mean(time_diff), units = "mins")) / 24) * window)


  #median(time_diff)


  #Flatline detection
  sp$centerSD <- zoo::rollapply(df[,2], width = interval, FUN = sd, fill = TRUE, align = 'center', na.rm = TRUE)   # a rolling window of Standard Deviation in parameter values - CENTERED -- rep_width determines the window width for all of these options
  sp$leftSD <-   zoo::rollapply(df[,2], width = interval, FUN = sd, fill = TRUE, align = 'left', na.rm = TRUE)     # a rolling window of Standard Deviation in parameter values - LEFT -- rep_width determines the window width for all of these options
  sp$rightSD <-  zoo::rollapply(df[,2], width = interval, FUN = sd, fill = TRUE, align = 'right', na.rm = TRUE)    # a rolling window of Standard Deviation in parameter values - RIGHT -- rep_width determines the window width for all of these options


  #Spike detection
  sp$median <- zoo::rollapply(suppressWarnings(df[,2]), width = interval, FUN = median,  partial = TRUE, na.rm = TRUE, align = 'center')   # rolling median of the log(value) for given width - med_width - centered
  sp$sd <-     zoo::rollapply(suppressWarnings(df[,2]), width = interval, FUN = sd, na.rm=TRUE, partial = TRUE, align = 'center')            # rolling standard deviation of the median log(value) as calculated above for a larger window - centered

  # Use `!!sym(q_name)` for dynamic column reference
  df <- df %>%
    mutate(
      !!sym(q_name) := case_when(
        !is.na(.data[[q_name]]) & !(.data[[q_name]] %in% overwrite) ~ as.character(.data[[q_name]]),
        df[[2]] < 0 ~ 'impossible',
        df[[2]] < sensorMin ~ 'below_limits',
        df[[2]] > sensorMax ~ 'above_limits',
        sp$centerSD < prec ~ 'repeating_value',
        sp$leftSD < prec ~ 'repeating_value',
        sp$rightSD < prec ~ 'repeating_value',
        abs(suppressWarnings(df[[2]] - sp$median)) > (4 * sp$sd) ~ 'spike',
        TRUE ~ 'OK'
      )
    )

  #unique(df$Quality)

  # Optionally include sp columns in output
  if (diag) {
    df <- bind_cols(df, sp[, -1])  # drop 'ts' column from sp to avoid duplication
  }

  return(df)
}
