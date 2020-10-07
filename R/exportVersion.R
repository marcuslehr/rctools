#' @name exportVersion
#'
#' @title Exports the REDCap Version Number
#' @description Version numbers are returned as a character string.
#'   This feature is available for REDCap 6.0.0 and higher.
#'
#' @param url A url address to connect to the REDCap API
#' @param token A REDCap API token
#' @param ... Arguments to be passed to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#'
#' @details If this function is used in a version of REDCap that does not
#'   support the Export Version Number function, the character string
#'   \code{'5.12.2'} is returned. This is done solely for the convenience 
#'   of always returning a value that can be compared against other versions.
#'   
#' REDCap API Documentation (6.5.0):
#' This method returns the current REDCap version number as plain text 
#' (e.g., 4.13.18, 5.12.2, 6.0.0).
#' 
#' REDCap Version:
#' 6.0.0+ 
#' 
#' Known REDCap Limitations:
#' None
#' 
#' @author Benjamin Nutter
#'
#' @references
#' Please refer to your institution's API documentation.
#'
#' Additional details on API parameters are found on the package wiki at
#' \url{https://github.com/nutterb/redcapAPI/wiki/REDCap-API-Parameters}


exportVersion <- function(url = getOption("redcap_bundle")$redcap_url,
token = getOption("redcap_token"),
 ...,
                                error_handling = getOption("redcap_error_handling")){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = url,
                          add = coll)

  checkmate::assert_character(x = token,
                          add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling,
                                        choices = c("null", "error"),
                                        add = coll)
  
  checkmate::reportAssertions(coll)
  
  body <- list(token=token, 
               content='version')
  
  x <- httr::POST(url=url, 
                  body=body)
  
  if (x$status_code != 200) 
  {
    handled <- redcap_error(x, error_handling)
    #* If the export version API method isn't supported by the REDCap instance,
    #* return "5.12.2".  For convenience, we will treat all pre 6.0.0 
    #* versions the same.  The only inefficiency this will generate is 
    #* in choosing when to run `syncUnderscoreCodings`.
    if (is.null(handled)) return("5.12.2")
  }

  as.character(x)
}