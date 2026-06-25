#' Detect duplicated values in a timeseries
#'
#' @param originalTS A data frame containing time series data. It must have a timestamp column of class `POSIXct` and a numeric value column (assumed to be the second column).
#' @param value_column A vector of quality codes that CAN be overwritten by this function (e.g., manually applied flags that can be updated).
#' @param maxdupes Numeric, Number of duplicates to flag as repeating value
#' @param prec Numeric, similarity of a number to be a duplicate
#' @param output Integer, 0 (output the clean data) or 1 (output the duplicates)
#'
#' @return A dataframe with duplicates values removed
#'
#' @note The input data frame (`df`) must include the following columns:
#'
#' | Column Name | Type      | Description                                               |
#' |-------------|-----------|-----------------------------------------------------------|
#' | `ts`        | POSIXct   | Timestamp of each observation                             |
#' | `Value`     | numeric   | Measured value from the sensor                            |
#'
#' All other columns in the input data frame will be preserved in the output.
#' If `diag = TRUE`, additional diagnostic columns (e.g., rolling SD, median) will be appended.
#'
#' @importFrom stats sd median
#' @importFrom zoo rollapply
#' @importFrom tibble tibble
#' @importFrom dplyr mutate case_when bind_cols
#' @importFrom rlang sym
#' @importFrom data.table rleid
#'
#' @examples
#'
#'
#' @export
removeTSDuplicates <- function(originalTS, value_column, maxdupes = 3, prec = 0.0001, output=0,
                               last_values = NULL)
{

  reps <- 0
  if(!(is.null(last_values) || nrow(last_values) == 0 )){ #
    # For continuity, duplicate the first row for as many duplicates there are
    if(abs(originalTS[[value_column]][1] - last_values$value) < prec)
    reps <- last_values$dupes + 1

    originalTS <- rbind(
      originalTS[1,] %>% slice(rep(1:n(), each = reps)),
      originalTS)
  }


  diffColumn <- originalTS[[value_column]]
  originalTS$diffs <- c(NA, diff(diffColumn))
  originalTS$diffs2 <- c(diff(diffColumn),NA)

  #diffdf <- cbind(diffs,diffs2) %>% as.data.frame
  originalTS <- originalTS %>% mutate(dupe = ( abs(diffs) < prec | abs(diffs2) < prec ))
  #dupes <- originalTS[diffdf$dupe == TRUE,]
  count <- 0
  #originalTS <- originalTS %>% dplyr::filter(dupe == TRUE)

  #originalTS$run <- rleid(originalTS$dupe)
  #dupesonly <- data.frame(
  #  run = rle(originalTS$run)$values,
  #  dupe = rle(originalTS$dupe)$values,
  #  length = rle(originalTS$run)$lengths )%>%
  #  dplyr::filter(dupe == TRUE & length >= maxdupes)
  # dupesremoved <- originalTS %>% dplyr::filter(run %in% dupesonly$run) %>%
  #  distinct(ts, .keep_all = TRUE)

  dupesonly <- originalTS %>% dplyr::filter(dupe == TRUE)
  dupesremoved <- list()
  for(i in  split(dupesonly,rleid(dupesonly[[value_column]])))
  {
    if(nrow(i) >= maxdupes)
    {
      dupesremoved[[count <- count + 1]] <- i
    }
  }
  dupesremoved <- rbindlist(dupesremoved)

  if(output == 1) {
    if(nrow(dupesremoved) == 0) return(dupesremoved)
    return(dupesremoved %>% dplyr::select(-c(diffs,diffs2,dupe)))
  }
  return(originalTS[!(originalTS$ts %in% dupesremoved$ts),] %>% dplyr::select(-c(diffs,diffs2,dupe)))
}
