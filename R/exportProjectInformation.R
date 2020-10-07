#' @name exportProjectInformation
#' @aliases exportProjectInformation.redcapApiConnection
#' @aliases exportProjectInformation.redcapDbConnection
#' @importFrom httr POST
#'
#' @title Exports the Project Information
#' @description Retrieve a data frame with the project information.
#'
#' @param url A url address to connect to the REDCap API
#' @param token A REDCap API token
#' @param ... Arguments to be passed to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' 
#' @details If this function is used in a version of REDCap that does not
#'   support the Export Version Number function, the character string
#'   \code{'Version Unknown'} is returned.
#'   
#' REDCap API Documentation (6.5.0):
#' This function allows you to export some of the basic attributes of a given 
#' REDCap project, such as the project's title, if it is longitudinal, if 
#' surveys are enabled, the time the project was created and moved to production, etc.
#' 
#' REDCap Version:
#' 6.5.0 (Perhaps earlier) 
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


exportProjectInformation <- function(url = getOption("redcap_bundle")$redcap_url,
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
  
  body <- list(token = token, 
               content = 'project',
               format = 'csv',
               returnFormat = 'csv')
  
  x <- httr::POST(url = url, 
                  body = body)
  
  if (x$status_code != 200) return(redcap_error(x, error_handling))
  
  utils::read.csv(text = as.character(x), 
                  stringsAsFactors = FALSE, 
                  na.strings="")
}