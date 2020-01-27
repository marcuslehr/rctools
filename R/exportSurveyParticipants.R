#' @name exportSurveyParticipants
#' @title Exports details of participants for a given survey
#' @description Retrieving dataframe of survey participants managed in REDCap
#' @param url A url address to connect to the REDCap API
#' @param token A REDCap API token
#' @param instrument A string type holding the name of "instrument" or survey the participants are managed under
#' @param event A string type holding the name of the event that the instrument belongs to
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param ... additional arguments to pass to other methods.
#' 
#' @details REDCap allows the user to manage a list of participants (if they are known) for each survey. This function 
#' pulls this information into a dataframe. The contents of the dataframe are the contact fields of the participants, 
#' the link to the survey for that participant if the participant hasn't completed the survey yet (otherwise, NA), 
#' the participants record id, and other information.
#' 
#' @author Paddy Tobias

exportSurveyParticipants <- function(url = getOption("redcap_bundle")$redcap_url,
token = getOption("redcap_token"),
 instrument, event, ...,
                                          error_handling = getOption("redcap_error_handling")){
  .params <- list(token=token, 
                  instrument = instrument,
                  event = event,
                  content='participantList',
                  format='csv', returnFormat='csv')
  
  x <- httr::POST(url=url, 
                  body=.params)
  
  if (x$status_code != 200) return(redcap_error(x, error_handling))
  
  x <- utils::read.csv(textConnection(as.character(x)), 
                       stringsAsFactors=FALSE, 
                       na.strings="")

  return(x)
}