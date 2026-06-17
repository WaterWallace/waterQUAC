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
#' @param threshold A numeric to calculate how long is exceeded
#' @param type A string. "rising" or "falling" (or NULL)
#'
#' @return A vector (in days)
#'
#' @note The input data frame (`data`) must include:
#'
#' | Column Name | Type      | Description                                       |
#' |-------------|-----------|---------------------------------------------------|
#' | `<time_col>` | POSIXct   | Timestamp of each observation                     |
#' | `<value_col>`| numeric   | Measured sensor value                             |
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
#' df <- df %>% mutate(daysAbove = daysAboveThreshold(., 2))
#' plot(df$ts, df$daysAbove)
#' @export
daysAboveThreshold <- function(data, threshold, type = NULL, dryperiod = 5)
{
  # data is a dataframe, with 2 columns, a posixct column, and a value column
  # threshold is a number to calculate how long is exceeded
  # type is either "rising" or "falling" (Or NULL) to calculate only rising or only falling
  data$tdiff <- c(0,diff(as.numeric(data[[1]]))) / 60 / 60 / 24 # time in days
  data$tdiff[data[[2]] < threshold] <- 0
  data$tdiff[data$tdiff > 5] <- 1

  if(!is.null(type))
  {
    f.value <- splinefun(data[[1]], data[[2]])
    data$sign <- f.value(data[[1]], deriv = 1) %>% sign
    #data$sign <- sign(data[[2]] - lag(data[[2]],1,0))

    data$tdiff2 <- data$tdiff # copy tdiff for rise/fall dependent
    if(type == "rising")
    {
      data <- data %>% mutate(tdiff2 = ifelse(sign > 0,tdiff,0))
    }else if(type == "falling"){
      data <- data %>% mutate(tdiff2 = ifelse(sign < 0,tdiff,0))
    }
    data$accum <- cumsum(data$tdiff2)
    data <- data %>% dplyr::select(-c(tdiff2,sign))
  }else{
    data$accum <- cumsum(data$tdiff)
  }

  data$lagtdiff <- lag(data$tdiff,1,0)

  # offsets reset the accumulation to zero
  data <- data %>% mutate(offset = ifelse(tdiff == 0 & lagtdiff > 0,accum,0))

  offsetdata <- data %>% dplyr::filter(offset > 0)

  if(nrow(offsetdata) == 0)   return(data %>% pull(accum))

  data$offset <- approx(offsetdata[[1]], offsetdata$offset, data[[1]], method = "constant",rule=1:2)$y
  data$offset[is.na(data$offset)] <- 0 #zero na values
  data <- data %>% mutate(accum = accum - offset)
  return(data %>% pull(accum))

}
