#' @name importFiles
#' @title Imports a File to REDCap to Attach to a Record
#' 
#' @description A single file may be attached to a single record.  The 
#'   behavior of this function is consistent with the
#'   behavior of the API, which only allows one file to be uploaded at a time
#'   
#' @param url A url address to connect to the REDCap API
#' @param token A REDCap API token
#' @param file Character string giving the file path to the file to be imported.
#' @param record The record ID in which the desired file is stored. Must be length 1.
#' @param field The field name in which the file is stored. Must be length 1.
#' @param event The event name for the file.  Must be length 1.  This applies 
#'   only to longitudinal projects.  If the event is not supplied for a 
#'   longitudinal project, the API will return an error
#' @param overwrite Logical.  When \code{FALSE}, the function checks if a 
#'   file already exists for that record.  If a file exists, the function 
#'   terminates to prevent overwriting.  When \code{TRUE}, no additional 
#'   check is performed.
#' @param bundle A \code{redcapBundle} object as created by \code{rc_setup}.
#' @param repeat_instance The repeat instance number of the repeating
#'   event or the repeating instrument. When available in your instance
#'   of REDCap, and passed as NULL, the API will assume a value of 1.
#' @param ... Arguments to be passed to other methods
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' 
#' @details The function may only import a single file
#' 
#' @author Benjamin Nutter
#' 
#' @export

importFiles <- function(url = getOption("redcap_bundle")$redcap_url,
token = getOption("redcap_token"),
 file, record, field, event = NULL, 
                              overwrite=TRUE, repeat_instance = NULL, ...,
                              bundle=NULL,
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
  
  massert(~ file + record + field + event,
          fun = checkmate::assert_character,
          null.ok = list(event = TRUE),
          fixed = list(len = 1, 
                       add = coll))
  
  checkmate::assert_logical(x = overwrite,
                            len = 1,
                            add = coll)
  
  checkmate::assert_integerish(x = repeat_instance,
                               len = 1,
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  
  #* Use working directory if 'dir' is not specified
  if (!file.exists(file)) 
    coll$push(paste0("No file found at '", file, "'"))
  
  #* make sure 'field' exist in the project and are 'file' fields
  if (is.null(bundle$data_dict)) 
    data_dict <- exportMetaData(url, token)
  
  if (!field %in% data_dict$field_name) 
    coll$push(paste("'", field, "' does not exist in the project.", sep=""))
  
  if (data_dict$field_type[data_dict$field_name == field] != "file")
    coll$push(paste0("'", field, "' is not of field type 'file'"))
  
  #* make sure 'event' exists in the project
  if (is.null(bundle$events)) 
    events_list <- exportEvents(url, token)
  
  if (class(events_list) == 'data.frame'){
    if (!event %in% events_list$unique_event_name) 
      coll$push(paste0("'", event, "' is not a valid event name in this project."))
  }
  
  if (!overwrite){
    fileThere <- rc_export(url, token, 
                               records = record, 
                               fields = field, 
                               events = event)
    if (!is.na(fileThere[field])) 
      coll$push("A file exists and overwrite=FALSE")
  }
  
  checkmate::reportAssertions(coll)
  
  body <- list(token = token, 
               content = 'file',
               action = 'import', 
               record = record,
               field = field, 
               file = httr::upload_file(file), 
               returnFormat = 'csv')
  
  if (!is.null(event)) body[['event']] <- event
  if (!is.null(repeat_instance)) body[['repeat_instance']] <- as.character(repeat_instance)
  #* Export the file
  file <- 
    tryCatch(
      httr::POST(
        url = url, 
        body = body),
      error = function(cond) list(status_code = "200"))
  
  if (file$status_code != "200") 
    redcap_error(file, error_handling)
  else 
    message("The file was successfully uploaded")
}