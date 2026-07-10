#' Detect duplicated values in a timeseries
#'
#' @param originalTS A data frame containing time series data. It must have a timestamp column of class `POSIXct` and a numeric value column.
#' @param value_column Character, name of the column containing the values to check for duplicates
#' @param maxdupes Numeric, Number of duplicates needed to flag as repeating value
#' @param prec Numeric, similarity of a number to be a duplicate
#' @param output Integer, 0 (output the clean data) or 1 (output the duplicates)
#' @param last_values A data frame containing the last values from the previous time series, used for continuity. It should have columns `value` and `dupes`.
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
#'
#' @importFrom stats sd median
#' @importFrom zoo rollapply
#' @importFrom tibble tibble
#' @importFrom dplyr mutate case_when bind_cols
#' @importFrom rlang sym
#' @importFrom data.table rleid rbindlist
#'
#' @examples
#' #' # Example usage of removeTSDuplicates
#' # Create a sample time series data frame
#' sample_data <- data.frame(
#'  ts = as.POSIXct(c('2024-01-01 00:00', '2024-01-01 00:01', '2024-01-01 00:02',
#'   '2024-01-01 00:03', '2024-01-01 00:04')),
#'  Value = c(1.0, 2.0, 2.0, 3.0, 4.0)
#'  )
#'  print(sample_data)
#'  # Call the function to remove duplicates
#'  cleaned_data <- removeTSDuplicates(sample_data, value_column = "Value", maxdupes = 2,
#'  prec = 0.0001, output = 0)
#'  print(cleaned_data)
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

  originalTS <- originalTS %>% mutate(dupe = ( abs(diffs) < prec | abs(diffs2) < prec ))

  dupesonly <- originalTS %>% dplyr::filter(dupe == TRUE)
  dupesremoved <- list()
  for(i in  split(dupesonly,rleid(dupesonly[[value_column]])))
  {
    if(nrow(i) >= maxdupes)
    {
      dupesremoved[[(length(dupesremoved) + 1)]] <- i
    }
  }
  dupesremoved <- rbindlist(dupesremoved)

  if(output == 1) {
    if(nrow(dupesremoved) == 0) return(dupesremoved)
    return(dupesremoved %>% dplyr::select(-c(diffs,diffs2,dupe)))
  }
  return(originalTS[!(originalTS$ts %in% dupesremoved$ts),] %>% dplyr::select(-c(diffs,diffs2,dupe)))
}
