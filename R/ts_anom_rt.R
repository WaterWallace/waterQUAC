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
#' @param time_threshold_days Numeric, threshold in days for detecting sensor drift.
#' @param prec Numeric, similarity of a number to be considered a duplicate.
#' @param log Logical, if TRUE, apply log transformation to the values before processing.
#' @param invert Logical, if TRUE, invert the values (useful for certain sensor types).
#' @param halflife_mins Half-life in minutes for exponential decay in running mean. The lower the number, the more responsive to real world changes. However shortens "memory".
#' @param z_threshold Numeric vector of length 2; lower and upper thresholds for z-score to classify spikes. Values outside this range are considered spikes. A lower upper z_score will narrow the bands around the data, detecting more spikes - but may incorrectly flag real-world changes.
#' @param sd_floor Numeric a higher number reduces the number "noise" spikes.
#' @param last_values A `data frame` containing the last processed values for real-time processing. It is a recursive output from this ts_anom_rt() function.
#'
#' @return A named list containing:
#' \describe{
#'   \item{last_values}{A single-row data frame summarising the most recent
#'   observation and drift state. Columns include:
#'   \describe{
#'     \item{ts}{Timestamp of the latest observation.}
#'     \item{value}{Latest observed value.}
#'     \item{mean}{Rolling mean at the latest observation.}
#'     \item{var}{Rolling variance at the latest observation.}
#'     \item{dev}{Deviation from the rolling mean.}
#'     \item{z_score}{Calculated z-score for the observation.}
#'     \item{spike}{Logical indicating whether the latest observation was
#'     classified as a spike.}
#'     \item{drift_ts}{Timestamp of the most recent drift event.}
#'     \item{drift_value}{Value associated with the most recent drift event.}
#'     \item{drift_cumulative_time}{Accumulated duration of the current drift
#'     period.}
#'     \item{drift_mean}{Mean value associated with the current drift state.}
#'     \item{drift_drift}{Logical indicating whether the latest observation
#'     was classified as drift.}
#'     \item{dupes}{Number of duplicate observations detected.}
#'   }}
#'
#'   \item{data}{A data frame (or tibble) containing the input time series with
#'   quality flags applied. Includes:
#'   \describe{
#'     \item{ts}{Observation timestamp.}
#'     \item{Value}{Observed value.}
#'     \item{Quality}{Quality classification for each observation (e.g. "OK",
#'     "spike", or other flag values).}
#'   }}
#' }

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
#'
#' @examples
#' library(dplyr)
#' library(zoo)
#' library(plotly)
#' library(data.table)
#' library(ggplot2)
#'
#' #example Total Suspended Solids dataframe
#' df <- waterQUAC::TSS_data
#'
#' # Apply anomaly detection
#' flagged <- ts_anom_rt(df = df,
#'                    overwrite = 1:4000,
#'                    sensorMin = 0,
#'                    sensorMax = 2000)
#'
#' flagged$data %>%
#'   ggplot(aes(x=ts, y=Value, colour = Quality, shape = Quality)) +
#'   geom_point()
#'
#' \dontrun{
#' # Plot flagged results
#'   plotly::plot_ly(data = flagged$data) |>
#'     plotly::add_markers(
#'       x = ~ts,
#'       y = ~Value,
#'       color = ~Quality,
#'       marker = list(size = 8)
#'     )
#' }
#'
#' @export
ts_anom_rt <- function(df, overwrite = c(1:4000), sensorMin, sensorMax,
                     time_threshold_days = 2,  prec = 0.0001, log = FALSE, invert = FALSE,
                     z_threshold = c(0.001,3), halflife_mins = 120, sd_floor = 0.01, last_values = NULL)
{
  #last_values$ts <- as.POSIXct("2026-06-23 17:00:00")
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
  if (is.null(last_values) || nrow(last_values) == 0){
    last_t <- 0 # last time
    last_running_mean <- df[[value_column]][1] # running mean
    last_dev <- df[[value_column]][1]*0.1 # squared deviance
    last_running_var <- (last_dev)^2 # variance

    # defaults for drift
    last_ts_value <- sensorMin - 1
    last_value <- sensorMin - 1 #df[[value_column]][1] # To check if level rising
    last_drift_t <- 0
    last_drift_mean <- df[[value_column]][1]
    cumulative_time <- 0
  }else{
    last_t <- last_values$ts # last time
    last_running_mean <- last_values$mean # running mean
    last_running_var <- last_values$var # variance
    last_dev <- last_values$dev # squared deviance

    # defaults for drift
    last_ts_value <- last_values$value # just for checking duplicates
    last_value <- last_values$drift_value
    last_drift_t <- last_values$drift_ts
    last_drift_mean <- last_values$drift_mean
    cumulative_time <- last_values$drift_cumulative_time

    print(df[[posixct_column]])
    print(last_t)
    df <- df %>% dplyr::filter(.data[[posixct_column]] > last_t)

    # exit if no new points
    if( nrow(df) == 0 ) return(NULL)

  }

  # Apply rules
  impossible_removed <- df %>% dplyr::filter(.[[2]] <= -99) # this is kind of redundant
  below_limits_removed <- df %>% dplyr::filter(.[[2]] <= sensorMin)
  above_limits_removed <- df %>% dplyr::filter(.[[2]] > sensorMax)

  sp <- tibble::tibble(ts = df[[posixct_column]], value = df[[value_column]])
  duplicates_removed <- removeTSDuplicates(sp, "value", output = 1, prec = prec,
                                           last_values = last_values)

  ######################################
  ## Clean data before despiking

  ## # skip remove Duplicates because the spike detection removes them
  sp <- sp %>% dplyr::filter(!(ts %in% duplicates_removed$ts)) %>%
    dplyr::filter(value > 0) %>%
    dplyr::filter(value > sensorMin) %>%
    dplyr::filter(value < sensorMax)

  # return NULL if there's no valid data left to despike
  if( nrow(sp) == 0 ) return(NULL)

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
  #driftts <- list()
  #drift_detect <- NULL
  #sp <- na.omit(df)
  for(row in 1:nrow(sp))
  {
    # loop through each point
    spike_detect <- spike_detect_rt(ts = sp$ts[row], value = sp$value[row], last_t,
                                    last_running_mean, last_running_var, last_dev,
                                    halflife_mins = halflife_mins,
                                    z_threshold = z_threshold,
                                    sd_floor = sd_floor)

    threshold_multiplier <- 2
    logCorrectedMultiplier <- log(threshold_multiplier * exp(sp$value[row]) ) / sp$value[row]

    drift_detect <- drift_detect_rt (ts = sp$ts[row], value = sp$value[row], last_t = last_drift_t, last_value = last_value,
                                     threshold_multiplier = ifelse(log,logCorrectedMultiplier,threshold_multiplier), # log transform the multiplier
                                     time_threshold_days = time_threshold_days, halflife = 120,
                                     type = "rising",
                                     last_mean = last_drift_mean, cumulative_time = cumulative_time
                                     )
    #driftts[[length(driftts) + 1]] <- drift_detect

    if(!spike_detect$spike){ # only update if drift if not a spike
      # update recursive inputs
      last_value <- sp$value[row]
      last_drift_t <- sp$ts[row]
      last_drift_mean <- drift_detect$mean
      cumulative_time <- drift_detect$cumulative_time

      # store drift detected
      if(drift_detect$drift)  dedrift[[length(dedrift) + 1]] <- drift_detect
    }

    # update recursive inputs
    last_t <- spike_detect$ts
    last_running_mean <- spike_detect$mean
    last_running_var <- spike_detect$var
    last_dev <- spike_detect$dev

    # store spike detected
    if(spike_detect$spike)  despiked[[length(despiked) + 1]] <- spike_detect

  }

  # timestamps of flagged data
  spikesremoved <-  rbindlist(despiked)
  drift_removed <- rbindlist(dedrift)
  #driftts <- rbindlist(driftts)

  # NA if it's in overwrite
  df <- df %>%
    mutate(# Use `!!sym(q_name)` for dynamic column reference
      !!sym(q_name) := ifelse(!is.na(.data[[q_name]]) & !(.data[[q_name]] %in% overwrite),
                              as.character(.data[[q_name]]),
                              NA_character_)
    )

  # add QC column
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

  # sp becomes too filtered for this to work as is, so just leaving it out
  # Optionally include sp columns in output
  #if (diag) {
  #  df <- bind_cols(df, sp[, -1])  # drop 'ts' column from sp to avoid duplication
  #}

  # if this section ends on a duplicate, store the running number of duplicates
  last_dupe <- abs(c(last_ts_value, df$Value) - lag(c(last_ts_value, df$Value))) < 0.001
  rle_last_dupe <- rle(last_dupe)
  last_dupes <- ifelse(tail(rle_last_dupe$values,1),
         tail(rle_last_dupe$lengths,1),
         0)

  names(drift_detect) <- paste0("drift_",names(drift_detect))

  list(
    last_values = bind_cols(spike_detect, drift_detect, dupes = last_dupes),
    data = df)


}









