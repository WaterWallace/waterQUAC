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
#' @param ts Timestamp of class `POSIXct`
#' @param value Numeric value
#' @param last_t Last timestamp processed (for real-time processing)
#' @param last_running_mean Last running mean value (for real-time processing)
#' @param last_running_var Last running variance value (for real-time processing)
#' @param last_dev Last deviation value (for real-time processing)
#' @param halflife_mins Half-life in minutes for exponential decay in running mean
#' @param z_threshold Numeric vector of length 2; lower and upper thresholds for z-score to classify spikes. Values outside this range are considered spikes.
#'
#' # make the return describe each part and what it means,
#' @return  A `data.frame` with output fields serving as the updated state of the running statistics after processing the current observation:
#' \describe{
#' \item{ts}{Timestamp of the observation}
#' \item{value}{Observed value}
#' \item{mean}{Updated running mean after processing the current observation}
#' \item{var}{Updated running variance after processing the current observation}
#' \item{dev}{Deviation of the current observation from the running mean}
#' \item{z_score}{Z-score of the current observation based on the running mean
#' and variance}
#' \item{spike}{Logical indicator of whether the current observation is classified as a spike (TRUE) or not (FALSE) based on the z-score thresholds}
#' }
#'
#' @importFrom tibble tibble
#' @importFrom rlang sym
#'
#' @examples
#' library(dplyr)
#' library(zoo)
#' library(plotly)
#' library(data.table)
#'
#' #example Total Suspended Solids dataframe
#' df <- waterQUAC::TSS_data
#'
#' # Apply anomaly detection
#' flagged <- spike_detect_rt(ts = df$ts[1],
#'                          value = df$Value[1]
#'                          )
#' print(flagged)
#' # use last values as a recursive input to the functino
#' flagged <- spike_detect_rt(ts = df$ts[2],
#'                         value = df$Value[2],
#'                         last_t = flagged$ts,
#'                         last_running_mean = flagged$mean,
#'                         last_running_var = flagged$var,
#'                         last_dev = flagged$dev
#'                         )
#' print(flagged)
#'
#' # loop through entire example df using a for loop
#' spike_results <- list()
#' for(i in 1:nrow(df)) {
#'   if(i == 1) {
#'     spike_results[[i]] <- spike_detect_rt(ts = df$ts[i],
#'                                      value = df$Value[i])
#'    }else{
#'       spike_results[[i]] <- spike_detect_rt(ts = df$ts[i],
#'                                             value = df$Value[i],
#'                                             last_t = spike_results[[i-1]]$ts,
#'                                             last_running_mean = spike_results[[i-1]]$mean,
#'                                             last_running_var = spike_results[[i-1]]$var,
#'                                            last_dev = spike_results[[i-1]]$dev
#'                                             )
#'    }
#' }
#' spike_results <- data.table::rbindlist(spike_results)
#'
#' # and then add the upper and lower to the plotly plot:
#' spike_results <- spike_results %>%
#' mutate(upper = mean + 4* pmax(sqrt(var),0.01)) %>%
#' mutate(lower = mean - 4* pmax(sqrt(var),0.01))
#'
#' head(spike_results)
#' spike_results %>% dplyr::select(-c(var, dev, z_score, mean)) %>%
#'   ggplot(.) +
#'   geom_point(aes(x=ts, y=value, colour = spike, shape = spike)) +
#'   geom_line(aes(x=ts, y=upper), colour = "grey", linetype = "dashed") +
#'   geom_line(aes(x=ts, y=lower), colour = "grey", linetype = "dashed")
#'
#'
#'
#'
#' \dontrun{
#' # Plot flagged results
#'   plotly::plot_ly(data = spike_results) |>
#'     plotly::add_markers(
#'       x = ~ts,
#'       y = ~value,
#'       color = ~spike,
#'       marker = list(size = 8)
#'     ) %>%
#'     plotly::add_lines(x = ~ts, y = ~upper, name = "Upper Threshold",
#'       line = list(color = 'grey', dash = 'dot')) %>%
#'     plotly::add_lines(x = ~ts, y = ~lower, name = "Lower Threshold",
#'       line = list(color = 'grey', dash = 'dot'))
#'  }
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

  # z_score determines whether it's a spike or not, >4 or <0.001 is a spike
  # not strictly a z score, but calculated on a variance calculated from a lagged squared deviance
  # i.e. it is the deviance before the running mean is updated, so it's not a true z score, but it is a measure of how far away the current value is from the running mean, in terms of standard deviations
  # so it's the deviance from the mean, divided by the standard deviation (sqrt of variance)

  # e.g. for z_score: if deviance / stddev = 4 that's like deviance = 4 * stddev, just neater
  z_score <- this_dev / max(sqrt(this_var), 0.01)
  # update z_score each time

  # if z score is outside the thresholds
  # then don't update the running mean
  spike <- (z_score > z_threshold[2] | z_score < z_threshold[1] )
  weight <- ifelse(spike, 0.1, 1)

  running_mean <- (1 - weight) * last_running_mean + weight * running_mean
  this_var <- (1 - weight) * last_running_var + weight * this_var

  return(data.frame(ts = ts, value = value, mean = running_mean, var = this_var, dev = this_dev, z_score, spike))
}
