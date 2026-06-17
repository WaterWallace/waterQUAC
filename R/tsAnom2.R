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

ts_anom2 <- function(df, overwrite = c(1:4000), sensorMin, sensorMax, window = 10, prec = 0.0001, diag = FALSE, lonelyPoints = 4, time_threshold_days = 2, flashy = FALSE, invert = FALSE, log = FALSE, medianfilt = TRUE) {

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

  #plot(sp$value * -1)

  if(invert)
  {
    invertoffset <- max(sp$value %>% na.omit) + 1
    sp$value <- sp$value * -1 + invertoffset
  }



  #plot(sp)



  # fullsp for comparison to the despiked
  fullsp <- sp

  ## Remove lonely points
  sp <- sp %>% mutate(dy = c(diff(as.numeric(ts)/60/60),0)) %>%
    dplyr::mutate(tdifflag = lag(dy,1,0)) %>%
    mutate(mindiff = pmin(dy,tdifflag)) %>%
    mutate(rollingmean = frollmean(mindiff,10, align = "center", fill = NA)) %>%
    mutate(rollingmean = nafill(rollingmean, type = "locf"))  %>%
    mutate(rollingmean = nafill(rollingmean, type = "nocb"))  %>%
    dplyr::filter(mindiff < rollingmean*2 ) %>% # 2 times the rolling average time difference either side is a lonely point
    dplyr::select(-c(tdifflag,mindiff,rollingmean))

  #plot(sp$ts, sp$value)
  #sp$value
  #plot(sp$ts, sp$value)
  #nrow(sp)
  #nrow(fullsp)

  #lonely
  lonelypoints <- fullsp %>% dplyr::filter(!(ts %in% sp$ts))

  #cbind(
  #xts(fullsp$value, fullsp$ts),
  #xts(lonelypoints$value, lonelypoints$ts )) %>% dygraph

  #sp <- sp %>%
    #mutate(dx = abs(c(0,diff(value)))) %>%
    #mutate(lx = lag(value)) %>%
    #mutate(dx = ifelse(dx < 0.01, 0.01, dx))# %>%
    #mutate(pcnt = dx/lx*100) %>%
    #mutate(logpcnt = log(pcnt))


  #plot(sp$ts, sp$logpcnt)
  #points( cciDespike(data.frame(sp$ts, sp$pcnt), hoursAvg = 6), col="red")
  # spiky <- data.frame(sp$ts, sp$pcnt)


  #plot(despikedlog$sp.logpcnt)
  #plot(sp$logpcnt)
  #plot(sp$ts, despiked$sp.ts)
  #sp

  #despiked <- cciDespike(data.frame(sp$ts, sp$pcnt), hoursAvg = floor(window/2)*2, rocstdev = NULL, spikeaction = "infill" )
  #despiked <- despiked %>% na.omit %>% mutate(logpcnt = log(sp.pcnt) )
  #despikedlog <- cciDespike(data.frame(despiked$sp.ts, despiked$logpcnt), hoursAvg = floor(window/2)*2, rocstdev = NULL, spikeaction = "infill")

  #plot(sp$logpcnt)
  #plot(despiked)
  #plot(sp$ts, sp$logpcnt, col="red")
  #points(despikedlog)

  #sp$qc <- "OK"
  #if(!flashy)
  #{


  #plot(despikedts$spike)

  #good <- despikedts %>% dplyr::filter(spike == "OK")
  #interp <- despikedts %>% dplyr::filter(spike != "OK")


  #plot(df$Value)
  #if(invert)
  #{
  #  despikedts[[2]] <- (despikedts[[2]] - invertoffset) * -1
  #}

  #if(log)
  #{
  #  despikedts[[2]] <- exp(despikedts[[2]])
  #}

  #good <- despikedts %>% dplyr::filter(spike == "OK")
  #interp <- despikedts %>% dplyr::filter(spike != "OK")


  #cbind(
  #  xts(df$Value, df$ts),
  #  xts(good$value, good$ts),
  #  xts(interp$value, interp$ts)
  #) %>% dygraph


  #plot(despikedts$ts, despikedts$value )
  #despikedts2 <- cciDespike(despikedts %>% dplyr::select(ts, value), hoursAvg = floor(window/2)*2,
  #                         rocstdev = NULL,
  #                        spikeaction = "omit")

  # I want to return
  #sp <- despikedts[ despikedts$ts %in% despikedts2$ts, ]


  #}

  #sp <- sp[ sp$ts %in% despiked$sp.ts &
  #            sp$ts %in% despikedlog$sp.ts,]


  #nrow(sp)
  #lag(sp$value)
  #nrow(fullsp)
  #nrow(sp)
  #cbind(
  #     xts(sp$rollingmean*2, sp$ts),
  #     xts(sp$mindiff, sp$ts),
  #     xts(sp$value, sp$ts)
  #     ) %>% dygraph

  multiplier <- ifelse(invert, 1,-1)

  ## Remove the point before a sudden drop
  sp <- sp %>% mutate(dy = c(diff(as.numeric(ts)/60/60),0)) %>%
    mutate(dx = c(diff(value),0)) %>% mutate(dydx = dx/dy) %>%
    dplyr::filter(dydx > multiplier*5*sd(na.omit(dydx))) %>% # only remove negative drops, 10 standard deviations from normal
    dplyr::select(-c(dy, dx, dydx))


  if(medianfilt)
  {
    library(RcppRoll)
    library(data.table)

    rolling_median_center_fun <- function(x,y,window_minutes) {
      # returns a lookup function
      f.sp <- approxfun(x, y)                  # linear interpolation
      ts_grid <- seq(min(x), max(x), by = 10*60)  # 1-minute steps
      minutey <- f.sp(ts_grid)                 # numeric vector of interpolated values
      window <- window_minutes
      half_w <- floor(window/2)
      n <- length(minutey)
      minutey_pad <- c(rep(minutey[1], half_w), minutey, rep(minutey[n], half_w))
      roll_pad <- roll_medianr(minutey_pad, n = window)

      return(approxfun(ts_grid, roll_pad[(window):(window + n - 1)]))
    }

    rolling_sd_center_fun <- function(x, y, window_minutes) {
      f.sp <- approxfun(x, y)
      ts_grid <- seq(min(x), max(x), by = 10*60)
      minutey <- f.sp(ts_grid)
      window <- window_minutes
      half_w <- floor(window / 2)
      n <- length(minutey)
      minutey_pad <- c(
        rep(minutey[1], half_w),
        minutey,
        rep(minutey[n], half_w)
      )
      roll_sd_pad <- data.table::frollapply(
        minutey_pad,
        n = window,
        FUN = function(z) sqrt(mean((z - mean(z))^2))
      )
      roll_sd <- roll_sd_pad[window:(window + n - 1)]
      approxfun(ts_grid, roll_sd)
    }


    f.median <- rolling_median_center_fun(sp$ts,sp$value,window*60)
    f.sd <- rolling_sd_center_fun(sp$ts,sp$value,2*window*60) # double it, as per databricks .py script
    sp$median <- f.median(sp$ts)
    sp$sd <- f.sd(sp$ts)


    sp <- sp %>% mutate(residual = value - median) %>%
      mutate(z_score = abs(residual) / pmax(sd,0.01) ) %>%
      mutate( isspike = z_score < 4) %>%
      dplyr::select(-z_score)


  }else{
    # slower moving, appropriate to do over original data
    sp <- cciDespike(spiky = sp %>% dplyr::select(ts, value) %>% as.data.frame, hoursAvg = floor(window/2)*2,
                     stdevs = 2,
                     rocstdev = NULL,
                     spikeaction = "omit",
                     passes = 1, doPlot = TRUE)

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

