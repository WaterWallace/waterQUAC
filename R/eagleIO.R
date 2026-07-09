#' @title Extract historic data from WQI's eagle.IO instance
#'
#' @description Function for extracting historic data from WQI's eagle.IO instance. Requires a node ID that has connection status information to return (ie. Campbell pakbus logger)
#'
#' @source \url{https://wqi.eagle.io/}
#'
#' @param key This is required for all functions and can be generated from the account settings in WQI's Eagle.IO instance
#' @param node_id A node ID of a given parameter in eagle.IO, noting historic will only work with nodes that contain historic data (ie level/N-NO3/Turbidity).
#' @param start_time DateTime to start the historic period of capture. Format needs to POSIXCt or in ISO8601. If it is POSIXCt it will be converted to correctly to UTC. If it is ISO8601 it will be used as is and will be treated as already in UTC.
#' @param end_time DateTime to end the historic period of capture. Defaults to the current system time + one day. Format needs to be in ISO8601, see: 2014-10-09T22:38:10Z || 2014-10-09T22:38:10.000Z || 2014-10-09T20:38:10+0200 || 2014-10-09T20:38:10+02:00
#' @return tibble containing returned historic data, value and quality. Time as "Australia/Brisbane"
#'
#' @examples
#' #td <- 86400
#' #START <- format(Sys.time() -30*td, "%Y-%m-%dT%H:%M:%SZ") # note this will be in UTC
#' #content <- eio_hist(key = "XYZ", param = "5903e538bd10c2fa0ce50648", START = 1)
#' #library(tidyverse)
#' #reportableParamRef <- WQI::reportableParamRef
#' #param <- reportableParamRef |>
#' #  filter(GSnum == '1160122')
#' #param <- param$`N-NO3`
#'
#' @export

eio_hist <- function(key, node_id, start_time = NULL, end_time = Sys.time()) {
  #param -- MUST be a node ID corresponding to a historic data source (ie level/N-NO3/Turbidity)

  if(is.null(start_time)) start_time <- Sys.time() - 30 * 24 * 60 * 60
  if(inherits(start_time, "POSIXct")) attr(start_time, "tzone") <- "UTC"
  if(inherits(end_time, "POSIXct")) attr(end_time, "tzone") <- "UTC"

  start_time <- format(start_time, "%Y-%m-%dT%H:%M:%SZ")
  end_time <- format(end_time, "%Y-%m-%dT%H:%M:%SZ")

  URLData <- paste("https://api.eagle.io/api/v1/historic/?params=",node_id,"&startTime=",start_time,"&endTime=",end_time,"&qualityExcluded=NONE",sep = "")

  #API call GET
  APIData <- httr::GET(URLData,
                       httr::add_headers('X-Api-Key' = key,
                                   'Content-Type' = "application/json"))
  Node_content=jsonlite::fromJSON(rawToChar(APIData$content))
  Node_content$header

  Data<- tibble::tibble(ts = Node_content[["data"]][["ts"]])
  Data$ts<-as.POSIXct(Data$ts, format="%Y-%m-%dT%H:%M:%S", tz = "UTC")
  attr(Data$ts, "tzone") <- "Australia/Brisbane"

  Data <- Data |> tibble::add_column("Value" = NA,
                                      "Quality" = NA)
  if ("v" %in% colnames(Node_content[["data"]][["f"]][["0"]])) {
    Data$Value <- Node_content[["data"]][["f"]][["0"]][["v"]]
  } else ""
  if ("q" %in% colnames(Node_content[["data"]][["f"]][["0"]])) {
    Data$Quality <- Node_content[["data"]][["f"]][["0"]][["q"]]
  } else ""

  return(Data)
}


#' @title Extract historic OPUS data from WQI's eagle.IO instance
#'
#' @description Function for extracting a tibble of historic OPUS data from WQI's eagle.IO instance.
#'
#' @source \url{https://wqi.eagle.io/}
#'
#' @param key This is required for all functions and can be generated from the account settings in WQI's Eagle.IO instance
#' @param NNO3 A node ID for a given sites NNO3 parameter in eagle.IO.
#' @param TSSeq A node ID for a given sites TSSeq parameter in eagle.IO.
#' @param abs210 A node ID for a given sites abs210 parameter in eagle.IO.
#' @param abs254 A node ID for a given sites abs254 parameter in eagle.IO.
#' @param abs360 A node ID for a given sites abs360 parameter in eagle.IO.
#' @param SQI A node ID for a given sites SQI parameter in eagle.IO.
#' @param start_time DateTime to start the historic period of capture. Format needs to be in ISO8601, see: 2014-10-09T22:38:10Z || 2014-10-09T22:38:10.000Z || 2014-10-09T20:38:10+0200 || 2014-10-09T20:38:10+02:00
#' @param end_time DateTime to end the historic period of capture. Defaults to the current system time + one day. Format needs to be in ISO8601, see: 2014-10-09T22:38:10Z || 2014-10-09T22:38:10.000Z || 2014-10-09T20:38:10+0200 || 2014-10-09T20:38:10+02:00
#'
#' @examples
#' #loggerRef <- WQI::loggerRef
#' #site <- "1111019"
#' #dex <- loggerRef |>
#' #  dplyr::filter(GSnum == site)
#'
#' #NNO3 <- dex$`OPUSResults - OPUS1000 N NO3`
#' #TSSeq <- dex$`OPUSResults - OPUS1016 TSSeq`
#' #abs210 <- dex$`OPUSResults - OPUS1036 Abs210`
#' #abs254 <- dex$`OPUSResults - OPUS1042 Abs254`
#' #abs360 <- dex$`OPUSResults - OPUS1034 Abs360`
#' #SQI <- dex$`OPUSResults - OPUS1060 SQI`
#'
#' @return tibble containing returned historic data for OPUS reportable parameters and associsated spectral values, value and quality. Time as "Australia/Brisbane"
#'
#' @export
#'
#'
#'

OPUS_Hist <- function(key, NNO3, TSSeq, abs210, abs254, abs360, SQI, start_time, end_time = format(Sys.time() + 86400, "%Y-%m-%dT%H:%M:%SZ")) {

  params <- paste(NNO3, TSSeq, abs210, abs254, abs360, SQI, sep = ",")

  URLData <- paste("https://api.eagle.io/api/v1/historic/?params=",params,"&startTime=",start_time,"&endTime=",end_time,"&qualityExcluded=NONE",sep = "")

  #API call GET
  APIData <- httr::GET(URLData,
                       httr::add_headers('X-Api-Key' = key,
                                         'Content-Type' = "application/json"))
  Node_content=jsonlite::fromJSON(rawToChar(APIData$content))

  Data<- tibble::tibble(ts = Node_content[["data"]][["ts"]])
  Data$ts<-as.POSIXct(Data$ts, format="%Y-%m-%dT%H:%M:%S", tz = "UTC")
  attr(Data$ts, "tzone") <- "Australia/Brisbane"

  Data <- Data |> tibble::add_column("N-NO3" = NA,
                                      "N-NO3_Qual" = NA,
                                      "TSSeq" = NA,
                                      "TSSeq_Qual" = NA,
                                      "abs210" = NA,
                                      "abs254" = NA,
                                      "abs360" = NA,
                                      "SQI" = NA,
                                      )

  if ("v" %in% colnames(Node_content[["data"]][["f"]][["0"]])) {
    Data$`N-NO3` <- Node_content[["data"]][["f"]][["0"]][["v"]]
  } else ""
  if ("q" %in% colnames(Node_content[["data"]][["f"]][["0"]])) {
    Data$`N-NO3_Qual` <- Node_content[["data"]][["f"]][["0"]][["q"]]
  } else ""
  if ("v" %in% colnames(Node_content[["data"]][["f"]][["1"]])) {
    Data$TSSeq <- Node_content[["data"]][["f"]][["1"]][["v"]]
  } else ""
  if ("q" %in% colnames(Node_content[["data"]][["f"]][["1"]])) {
    Data$TSSeq_Qual <- Node_content[["data"]][["f"]][["1"]][["q"]]
  } else ""
  if ("v" %in% colnames(Node_content[["data"]][["f"]][["2"]])) {
    Data$abs210 <- Node_content[["data"]][["f"]][["2"]][["v"]]
  } else ""
  if ("v" %in% colnames(Node_content[["data"]][["f"]][["3"]])) {
    Data$abs254 <- Node_content[["data"]][["f"]][["3"]][["v"]]
  } else ""
  if ("v" %in% colnames(Node_content[["data"]][["f"]][["4"]])) {
    Data$abs360 <- Node_content[["data"]][["f"]][["4"]][["v"]]
  } else ""
  if ("v" %in% colnames(Node_content[["data"]][["f"]][["5"]])) {
    Data$SQI <- Node_content[["data"]][["f"]][["5"]][["v"]]
  } else ""



  return(Data)
}




#' @title Extract historic NICO data from WQI's eagle.IO instance
#'
#' @description Function for extracting a tibble of historic NICO data from WQI's eagle.IO instance.
#'
#' @source \url{https://wqi.eagle.io/}
#'
#' @param key This is required for all functions and can be generated from the account settings in WQI's Eagle.IO instance
#' @param NNO3 A node ID for a given sites NNO3 parameter in eagle.IO.
#' @param refA A node ID for a given sites refA parameter in eagle.IO.
#' @param refB A node ID for a given sites refB parameter in eagle.IO.
#' @param refC A node ID for a given sites refC parameter in eagle.IO.
#' @param refD A node ID for a given sites refD parameter in eagle.IO.
#' @param SQI A node ID for a given sites SQI parameter in eagle.IO.
#' @param start_time DateTime to start the historic period of capture. Format needs to be in ISO8601, see: 2014-10-09T22:38:10Z || 2014-10-09T22:38:10.000Z || 2014-10-09T20:38:10+0200 || 2014-10-09T20:38:10+02:00
#' @param end_time DateTime to end the historic period of capture. Defaults to the current system time + one day. Format needs to be in ISO8601, see: 2014-10-09T22:38:10Z || 2014-10-09T22:38:10.000Z || 2014-10-09T20:38:10+0200 || 2014-10-09T20:38:10+02:00
#'
#' @examples
#' #loggerRef <- WQI::loggerRef
#' #site <- "1350053"
#' #dex <- loggerRef |>
#' #  dplyr::filter(GSnum == site)
#'
#' #NNO3 <- dex$`OPUSResults - OPUS1000 N NO3`
#' #refA <- dex$`Nitrate - NICO RefA`
#' #refB <- dex$`Nitrate - NICO RefB`
#' #refC <- dex$`Nitrate - NICO RefC`
#' #refD <- dex$`Nitrate - NICO RefD`
#' #SQI <- dex$`OPUSResults - OPUS1060 SQI`
#'
#' @return tibble containing returned historic data for OPUS reportable parameters and associsated spectral values, value and quality. Time as "Australia/Brisbane"
#'
#' @export
#'
#'
#'


NICO_Hist <- function(key, NNO3, refA, refB, refC, refD, SQI, start_time, end_time = format(Sys.time() + 86400, "%Y-%m-%dT%H:%M:%SZ")) {

  params <- paste(NNO3, refA, refB, refC, refD, SQI, sep = ",")

  URLData <- paste("https://api.eagle.io/api/v1/historic/?params=",params,"&startTime=",start_time,"&endTime=",end_time,"&qualityExcluded=NONE",sep = "")

  #API call GET
  APIData <- httr::GET(URLData,
                       httr::add_headers('X-Api-Key' = key,
                                         'Content-Type' = "application/json"))
  Node_content=jsonlite::fromJSON(rawToChar(APIData$content))

  Data<- tibble::tibble(ts = Node_content[["data"]][["ts"]])
  Data$ts<-as.POSIXct(Data$ts, format="%Y-%m-%dT%H:%M:%S", tz = "UTC")
  attr(Data$ts, "tzone") <- "Australia/Brisbane"

  Data <- Data |> tibble::add_column("N-NO3" = NA,
                                      "N-NO3_Qual" = NA,
                                      "refA" = NA,
                                      "refB" = NA,
                                      "refC" = NA,
                                      "refD" = NA,
                                      "SQI" = NA,
                                      )

  if ("v" %in% colnames(Node_content[["data"]][["f"]][["0"]])) {
    Data$`N-NO3` <- Node_content[["data"]][["f"]][["0"]][["v"]]
  } else ""
  if ("q" %in% colnames(Node_content[["data"]][["f"]][["0"]])) {
    Data$`N-NO3_Qual` <- Node_content[["data"]][["f"]][["0"]][["q"]]
  } else ""
  if ("v" %in% colnames(Node_content[["data"]][["f"]][["1"]])) {
    Data$refA <- Node_content[["data"]][["f"]][["1"]][["v"]]
  } else ""
  if ("v" %in% colnames(Node_content[["data"]][["f"]][["2"]])) {
    Data$refB <- Node_content[["data"]][["f"]][["2"]][["v"]]
  } else ""
  if ("v" %in% colnames(Node_content[["data"]][["f"]][["3"]])) {
    Data$refC <- Node_content[["data"]][["f"]][["3"]][["v"]]
  } else ""
  if ("v" %in% colnames(Node_content[["data"]][["f"]][["4"]])) {
    Data$refD <- Node_content[["data"]][["f"]][["4"]][["v"]]
  } else ""
  if ("v" %in% colnames(Node_content[["data"]][["f"]][["5"]])) {
    Data$SQI <- Node_content[["data"]][["f"]][["5"]][["v"]]
  } else ""

  return(Data)
}





#' @title Extract communications information for a given logger source
#'
#' @description Function for extracting comms data from WQI's eagle.IO instance. Requires a node ID for a DATA SOURCE that has connection status information to return (ie. Campbell pakbus logger)
#'
#' @source \url{https://wqi.eagle.io/}
#'
#' @param key == This is required for all functions and can be generated from the account settings in WQI's Eagle.IO instance
#' @param node_id == a node ID of a given parameter in eagle.IO, noting historic will only work with nodes that contain historic data
#'
#' @return data frame containing returned comms data
#' @export
EIO_Comms <- function(key, node_id) {
  #param -- MUST be a node ID corresponding to a historic data source (ie level/N-NO3/Turbidity)

  URLData <- paste("https://api.eagle.io/api/v1/nodes/?attr=currentStatus,lastCommsSuccess&filter=_id($eq:",node_id,")",sep="")
  #API call GET
  APIData <- httr::GET(URLData,
                       httr::add_headers('X-Api-Key' = key,
                                   'Content-Type' = "application/json"))
  Node_content=jsonlite::fromJSON(rawToChar(APIData$content))

  return(Node_content)
}





#' @title Extract node data from WQI's eagle.IO instance
#'
#' @description Will extract the current value for a given NODE ID from WQI's eagle.IO instance.
#'
#' @source \url{https://wqi.eagle.io/}
#'
#' @param key == This is required for all functions and can be generated from the account settings in WQI's Eagle.IO instance
#' @param node_id == a node ID of a given parameter in eagle.IO, noting historic will only work with nodes that contain historic data
#'
#' @examples
#' #content <- EIO_Node(key = "XYZ", param = "59cca1064f2ee90c99b94b2e")
#'
#' @return data frame containing returned node data
#' @export
#'
EIO_Node <- function(key, node_id) {
  #param -- MUST be a node ID corresponding to a historic data source (ie level/N-NO3/Turbidity)

  URLData <- paste("https://api.eagle.io/api/v1/nodes/?attr=currentValue&filter=_id($eq:",node_id,")",sep="")
  #API call GET
  APIData <- httr::GET(URLData,
                       httr::add_headers('X-Api-Key' = key,
                                   'Content-Type' = "application/json"))
  Node_content=jsonlite::fromJSON(rawToChar(APIData$content))

  return(Node_content)
}




