#' @title Data extraction from the Water Monitoring Information portal
#'
#' @description
#' Extracts data from the Water Monitoring Information Portal (WMIP). This platform is the front end of DRDMW's Hydstra database. It contains river level, discharge, water quality and rainfall data to list a few. What is available for each gauging station can be seen in the WMIP platform. This should be used to determine what parameter codes can be extracted in this function
#'
#' @source \url{https://water-monitoring.information.qld.gov.au/}
#' @param site_id A gauging station number as defined in the WMIP platform.
#' @param var A string referring to the reported variable from the desired site. Refer to \url{https://water-monitoring.information.qld.gov.au/} for information regarding the desired site and what data is available per the desired time period. This parameter defaults to "Level". Options are: level/discharge/rainfall/temperature/conductivity/pH/turbidity
#' @param datasource A string referring to the desired data source for the data to be extracted from. Defaults to "AT" which is the Archive-telemetered composite source. Options are: A/TE/AT/ATQ
#' @param start_time Date* to lookback to. Format = "YYYYMMDD"
#' @param end_time Date* to end the extraction period on. Format = "YYYYMMDD". Defaults to the system date plus one day.
#' @param type String max, min, mean, end, tot, cum, inst, point
#' @param interval String year, month, day, hour, minute, second
#' @param report String start or end
#' @param multiplier
#' @examples
#' df_test <- wmip_hist(site_id = "110001D", start_time = "20220801", var = "temperature")
#'
#' # bulk extract of level data
#' wmip_sites <- c(
#' "102102A",
#' "105107A",
#' "109001A",
#' "109001A",
#' "111101D",
#' "112004A",
#' "112101B",
#' "116001F",
#' "117002A",
#' "119003A",
#' "120006B",
#' "120002C",
#' "120015A",
#' "129001A",
#' "130005A",
#' "130005A",
#' "130113A",
#' "130302A",
#' "130401A",
#' "130401A",
#' "130504B",
#' "132001A",
#' "137001B",
#' "137201A",
#' "141010A",
#' "143107A"
#' )
#'
#'
#' list <- list()
#'
#'
#' for (i in 1:length(wmip_sites)){
#'   site = wmip_sites[i]
#'   list[[i]] <- wmip_hist(site_id = site,
#'                          start_time = "20230101"
#'   )
#'
#' }
#' list <- list[lapply(list, typeof) == "list"]
#' levels = do.call(rbind, list)
#'
#' @return A data frame containing extracted WMIP data for specified gauging station/parameter.
#'
#' @export
wmip_hist <- function (site_id, var = "level", datasource = "AT", start_time,
                       end_time = format(Sys.Date() + 1, "%Y%m%d"),
                       type="mean", interval="hour", report="start", multiplier = 1) {

  stopifnot("type must be max, min, mean, end, tot, cum, inst, point" =
              type %in% c( "max", "min", "mean", "end", "tot", "cum", "inst", "point" ) )

  stopifnot("interval must be year, month, day, hour, minute, second" =
              interval %in% c( "year", "month", "day", "hour", "minute", "second" ) )

  stopifnot("report must be start or end" =
              report %in% c( "start", "end") )

  var <- dplyr::case_when(
    var == "level" ~ "varfrom=100.00&varto=100.00",
    var == "discharge" ~ "varfrom=100.00&varto=140.00",
    var == "preservedQ" ~ "varfrom=140.00&varto=140.00",
    var == "rainfall" ~ "varfrom=10.00&varto=10.00",
    var == "temperature" ~ "varfrom=2080.00&varto=2080.00",
    var == "conductivity" ~ "varfrom=2010.00&varto=2010.00",
    var == "pH" ~ "varfrom=2100.00&varto=2100.00",
    var == "turbidity" ~ "varfrom=2030.00&varto=2030.00"
  )

  WMIP_URL <- paste("https://water-monitoring.information.qld.gov.au/cgi/webservice.pl?function=get_ts_traces&site_list=",
                    site_id, "&datasource=", datasource, "&",
                    var, "&start_time=", start_time, "&end_time=",
                    end_time, "&data_type=",type,"&interval=",interval,
                    "&report_time=",report,"&multiplier=",multiplier,"&format=csv", sep = "")

  df <- try(readr::read_csv(WMIP_URL), silent = TRUE)
  if ("try-error" %in% class(df)) {
    # Get the error message
    err <- df[1]
    print(
      paste0("file not available: check site code is correct and there is data available for the period and variable specified. Details: ", err)
    )
    # Handle the error here
  } else {
    #df <- readr::read_csv(WMIP_URL)
    df$time <- as.POSIXct(sprintf("%1.0f", df$time), format="%Y%m%d%H%M%S", origin = "1970-01-01")
    df <- df %>% mutate(value = ifelse(quality > 150, NA, value))
  }
  return(df)
}


