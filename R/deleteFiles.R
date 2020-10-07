#' @name deleteFiles
#' @title Delete a File attached to a Record
#' 
#' @description This function allows you to remove a document that has been 
#' attached to an individual record
#' 
#' @param url A url address to connect to the REDCap API
#' @param token A REDCap API token
#' @param record The record ID in which the desired file is stored. Must be length 1.
#' @param field The field name in which the file is stored. Must be length 1.
#' @param event The event name for the file.  Must be length 1.  
#'   This applies only to longitudinal projects.  If the event is not
#'   supplied for a longitudinal project, the API will return an error message.
#' @param bundle A \code{redcapBundle} object as created by \code{rc_bundle}.
#' @param ... Arguments to be passed to other methods
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' 
#' @author Benjamin Nutter
#'
#' @references
#' Please refer to your institution's API documentation.
#' 
#' Additional details on API parameters are found on the package wiki at
#' \url{https://github.com/nutterb/redcapAPI/wiki/REDCap-API-Parameters}

deleteFiles <- function(url = getOption("redcap_bundle")$redcap_url,
token = getOption("redcap_token"),
 record = NULL, 
                              field = NULL, event = NULL, ..., 
                              bundle = getOption("redcap_bundle"),
                              error_handling = getOption("redcap_error_handling")){
  if (is.numeric(record)) record <- as.character(record)
  
  coll <- checkmate::makeAssertCollection()
  
  massert(~ url + token + bundle,
          fun = checkmate::assert_class,
          classes = list(url = "character", token = "character",
                         bundle = "redcapBundle"),
          null.ok = list(url = FALSE, token = FALSE,
                         bundle = TRUE),
          fixed = list(add = coll))
  
  massert(~ record + field + event,
          fun = checkmate::assert_character,
          null.ok = list(event = TRUE),
          fixed = list(len = 1,
                       add = coll))
  
  error_handling <- checkmate::matchArg(x = error_handling,
                                        choices = c("null", "error"),
                                        add = coll)
  
  checkmate::reportAssertions(coll)
  
  #* make sure 'field' exist in the project and are 'file' fields
  if (is.null(bundle$data_dict))
  {
    data_dict <- exportMetaData(url, token)
  }
  if (!field %in% data_dict$field_name) 
    coll$push(paste0("'", field, "' does not exist in the project."))
  
  if (data_dict$field_type[data_dict$field_name == field] != "file")
    coll$push(paste0("'", field, "' is not of field type 'file'"))
  
  #* make sure 'event' exists in the project
  if (is.null(bundle$event_data)) 
    events_list <- exportEvents(url, token)
  
  if (class(events_list) == "data.frame")
  {
    if (!event %in% events_list$unique_event_name) 
      coll$push(paste0("'", event, "' is not a valid event name in this project."))
  }
  
  checkmate::reportAssertions(coll)
  
  body <- list(token = token, 
               content = 'file',
               action = 'delete', 
               record = record,
               field = field, 
               returnFormat = 'csv')
  
  if (is.null(event)) body[['event']] <- event
  
  #* Delete the file
  #* The tryCatch here seems a little quirky.  My best understanding is that since the API isn't returning
  #* anything into the 'content' attribute returned by POST, POST is casting an error.  Oddly, an error in this
  #* case, an error means the action was successfully performed.  The tryCatch call negotiates that oddity to
  #* get the desired result.
  
  x <- 
    tryCatch(
      httr::POST(url = url, 
                 body = body),
      error = function(cond) list(status_code = 200))
  
  if (x$status_code != "200")
    redcap_error(x, error_handling)
  else 
    message("The file was successfully deleted")
  
}
