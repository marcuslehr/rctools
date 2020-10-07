#' @name exportEvents
#' @title Export the Events for a Project
#' 
#' @description Retrieve a data frame giving the users, expiration dates,
#' and data access privileges for each user.
#'
#' @param url A url address to connect to the REDCap API
#' @param token A REDCap API token
#' @param arms A numeric vector or arm numbers to retrieve.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}} 
#' @param ... Arguments to be passed to other methods.
#' 
#' @details
#' It is not sufficient to make the project a longitudinal project. The
#' project must satisfy one of two conditions: 1) have at least two arms and
#' one event defined; or 2) have one arm and at least two events defined. If 
#' neither of these conditions are satisfied, the API will return a message
#' such as \code{ERROR: You cannot export arms for classic projects}, an 
#' error message that isn't as descriptive of the nature of the problem as 
#' we might like.
#' 
#' REDCap API Documentation:
#' This function allows you to export the events for a project
#' 
#' NOTE: this only works for longitudinal projects.
#' 
#' REDCap Version:
#' 5.8.2+ 
#' 
#' Known REDCap Limitations:
#' None 
#' 
#' @return Returns a data frame with six columns
#' \itemize{
#'   \item{\code{event_name} }{The desciptive name of the event.}
#'   \item{\code{arm_num} }{The arm number in which the event occurs.}
#'   \item{\code{day_offset} }{The days offset from the first event.}
#'   \item{\code{offset_min} }{The minimum offset value.}
#'   \item{\code{offset_max} }{The maximium offset value.}
#'   \item{\code{unique_event_name} }{A unique event identifying name.}
#' }
#'
#' @author Benjamin Nutter
#'
#' @references
#' Please refer to your institution's API documentation.
#'
#' Additional details on API parameters are found on the package wiki at
#' \url{https://github.com/nutterb/redcapAPI/wiki/REDCap-API-Parameters}

exportEvents <- function(url = getOption("redcap_bundle")$redcap_url,
token = getOption("redcap_token"),
 arms = NULL, ...,
                          error_handling = getOption("redcap_error_handling"))
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = url,
                          add = coll)
						  
  checkmate::assert_character(x = token,
                          add = coll)
  
  checkmate::assert_character(x = arms,
                              null.ok = TRUE,
                              add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"),
                                        add = coll)
  
  checkmate::reportAssertions(coll)
  
  #* parameters for the Users File Export
  body <- list(token = token, 
               content = 'event', 
               format = 'csv', 
               returnFormat = 'csv')
  
  if (!is.null(arms)) body[['arms']] <- paste0(arms, collapse = ",")
  
  #* Export Users file and convert to data frame
  x <- httr::POST(url = url, 
                  body = body)
  
  if (x$status_code != 200) return(redcap_error(x, error_handling))
  
  utils::read.csv(text = as.character(x),
                  stringsAsFactors = FALSE,
                  na.strings = "")
}
