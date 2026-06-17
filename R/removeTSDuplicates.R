#' Detect duplicated values in a timeseries
#'
#' @param originalTS A data frame containing time series data. It must have a timestamp column of class `POSIXct` and a numeric value column (assumed to be the second column).
#' @param diffColumn A vector of quality codes that CAN be overwritten by this function (e.g., manually applied flags that can be updated).
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
removeTSDuplicates <- function(originalTS, diffColumn, maxdupes = 3, prec = 0.0001, output=0)
{
  originalTS$diffs <- c(0, diff(diffColumn))
  originalTS$diffs2 <- c(diff(diffColumn),0)

  #diffdf <- cbind(diffs,diffs2) %>% as.data.frame
  originalTS <- originalTS %>% mutate(dupe = ( abs(diffs) < prec | abs(diffs2) < prec ))
  #dupes <- originalTS[diffdf$dupe == TRUE,]
  count <- 0
  #originalTS <- originalTS %>% dplyr::filter(dupe == TRUE)
  dupesremoved <- list()

  dupesonly <- originalTS %>% dplyr::filter(dupe == TRUE)


  for(i in  split(dupesonly,rleid(diffColumn)))
  {
  #for(i in  split(dupesonly,rleid(dupesonly[[2]])))
  #{
    #print(i)
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
