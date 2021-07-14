#' @name rc_import
#' @title Import Records to a REDCap Database
#' 
#' @description Imports records from a \code{data.frame} to a REDCap Database
#'
#' @param url A url address to connect to the REDCap API
#' @param token Path to a text file containing your REDCap API token
#' @param record_data A \code{data.frame} to be imported to the REDCap project.
#' @param data_dict A \code{redcapBundle} object as created by
#'   \code{rc_bundle}.
#' @param overwriteBehavior Character string.  'normal' prevents blank
#'   fields from overwriting populated fields.  'overwrite' causes blanks to
#'   overwrite data in the REDCap database.
#' @param returnContent Character string.  'count' returns the number of
#'   records imported; 'ids' returns a list of record ids imported;
#'   'nothing' returns no message.
#' @param returnData Logical.  Prevents the REDCap import and instead
#'   returns the data frame that would have been given
#'   for import.  This is sometimes helpful if the API import fails without
#'   providing an informative message. The data frame can be written to a csv
#'   and uploaded using the interactive tools to troubleshoot the
#'   problem.  Please shoot me an e-mail if you find errors I havne't
#'   accounted for.
#' @param logfile An optional filepath (preferably .txt) in which to print the
#'   log of errors and warnings about the data.
#'   If \code{""}, the log is printed to the console.
#' @param batch.size Specifies size of batches.  A negative value
#'   indicates no batching.
#'
#' @details
#' A record of imports through the API is recorded in the Logging section
#' of the project.
#'
#' \code{rc_import} prevents the most common import errors by testing the
#' data before attempting the import.  Namely
#' \enumerate{
#'   \item Check that all variables in \code{record_data} exist in the REDCap data data_dictionary.
#'   \item Check that the study id variable exists
#'   \item Force the study id variable to the first position in the data frame (with a warning)
#'   \item Remove calculated fields (with a warning)
#'   \item Verify that REDCap date fields are represented in the data frame as
#'     either character, POSIXct, or Date class objects.
#'   \item Determine if values are within their specified validation limits.
#' }
#'
#' See the documentation for \code{\link{validateImport}} for detailed
#' explanations of the validation.
#'
#' @author Benjamin Nutter\cr
#' with thanks to Josh O'Brien and etb (see references)
#' @author Marcus Lehr
#'
#' @references
#' \url{http://stackoverflow.com/questions/12393004/parsing-back-to-messy-api-strcuture/12435389#12435389}\cr
#' \url{https://github.com/etb/my-R-code/blob/master/R-pull-and-push-from-and-to-REDCap.R}\cr
#' See also the REDCap API documentation
#' Please refer to your institution's API documentation.
#'
#' Additional details on API parameters are found on the package wiki at
#' \url{https://github.com/nutterb/redcapAPI/wiki/REDCap-API-Parameters}
#'
#' @seealso \code{\link{validateImport}}
#'
#' @export

rc_import <- function(record_data,
                      url = getOption("redcap_bundle")$redcap_url,
                      token = getOption("redcap_token"),
                      data_dict = getOption("redcap_bundle")$data_dict,
                      overwriteBehavior = 'normal',
                      returnContent = 'count',
                      returnData = FALSE, logfile = "", batch.size=-1
                      ) {
  
  fields = names(record_data)
  
  validate_args(required = c('url','token','record_data','data_dict'),
                record_data = record_data, url = url, token = token,
                overwriteBehavior = overwriteBehavior, returnContent = returnContent,
                logfile = logfile, data_dict = data_dict, batch.size = batch.size,
                fields = fields)
  
  
  #** Remove survey identifiers and data access group fields from data
  # Is survey data not uploadable? Need to test
  w.remove <- 
    which(names(record_data) %in% 
            c("redcap_survey_identifier",
              paste0(unique(data_dict$form_name), "_timestamp"),
              "redcap_data_access_group"))
  if (length(w.remove)) record_data <- record_data[-w.remove]
  
  
  #** If the study id is not in the the first column, move it and print a warning
  id_field = getID(data_dict = data_dict, id_field = NULL)
  
  if (id_field != names(record_data)[1]) {
    
    record_data = dplyr::select(record_data, !!id_field, dplyr::everything())
    
    message("The variable'", id_field, 
            "' was not in the first column. ",
            "It has been moved to the first column.")
  }
  
  #*** Remove calculated fields
  fields_calc <- data_dict$field_name[data_dict$field_type == "calc"]
  fields_calc = fields_calc[fields_calc %in% names(record_data)]
  
  if (length(fields_calc) > 0) {
    record_data <- record_data[!names(record_data) %in% fields_calc]
    
    message("The variable(s) '", 
            paste(fields_calc, collapse="', '"),
            "' are calculated fields and cannot be imported. ",
            "They have been removed from the imported data frame.")
  }
  
  #** Confirm that date fields are either character, Date class, or POSIXct
  date_vars <- data_dict$field_name[grepl("date_", data_dict$text_validation_type_or_show_slider_number)]
  date_vars = date_vars[date_vars %in% names(record_data)]
  
  bad_date_fmt <- 
    !vapply(X = record_data[date_vars], 
            FUN = function(x) is.character(x) | "Date" %in% class(x) | "POSIXct" %in% class(x),
            FUN.VALUE = logical(1))
  
  if (any(bad_date_fmt)) {
    stop(paste0("The variables '", 
                     paste(date_vars[bad_date_fmt], collapse="', '"),
                     "' must have class Date, POSIXct, or character."))
  }
  
  msg <- paste0("REDCap Data Import Log: ", Sys.time(),
                "\nThe following (if any) conditions were noted about the data.\n\n")
  
  if (is.null(logfile)) 
    message(msg) 
  else 
    write(msg, logfile)
  
  record_data <- validateImport(data = record_data,
                         data_dict = data_dict,
                         logfile = logfile)
  
  if (returnData) return(record_data)
  
  #** Format the data for REDCap import
  #** Thanks go to:
  #**   https://github.com/etb/my-R-code/blob/master/R-pull-and-push-from-and-to-REDCap.R
  #**   http://stackoverflow.com/questions/12393004/parsing-back-to-messy-api-strcuture/12435389#12435389
  
  if (batch.size > 0) {
    import_records_batched(url = url,
						               token = token, 
                           data = record_data,
                           batch.size = batch.size,
                           overwriteBehavior = overwriteBehavior,
                           returnContent = returnContent)
  }
  else {
    import_records_unbatched(url = url,
							               token = token,
                             data = record_data,
                             overwriteBehavior = overwriteBehavior,
                             returnContent = returnContent)
  }
}

#####################################################################
## UNEXPORTED FUNCTIONS
#####################################################################

import_records_batched <- function(url = getOption("redcap_bundle")$redcap_url,
                                   token = getOption("redcap_token"),
                                   data, batch.size, 
                                   overwriteBehavior,
                                   returnContent) {
  n.batch <- nrow(data) %/% batch.size + 1
  
  ID <- data.frame(row = 1:nrow(data))
  
  ID$batch.number <- rep(1:n.batch, 
                         each = batch.size, 
                         length.out = nrow(data))
  
  data <- split(data, 
                f = ID$batch.number)
  
  out <- lapply(X = data, 
                FUN = data_frame_to_string)
  
  att <- list("Content-Type" = 
                structure(c("text/html", "utf-8"),
                          .Names = c("", "charset")))
  out <- lapply(X = out, 
                FUN = function(d){
                  attributes(d) <- att; 
                  return(d)
                })
  
  x <- vector("list", length = length(out))
  
  for (i in seq_along(out)) {
    httr::POST(url=url,
               body=list(token = token, 
                         content = 'record', 
                         format = 'csv',
                         type = 'flat', 
                         overwriteBehavior = overwriteBehavior,
                         returnContent = returnContent,
                         returnFormat = 'csv', 
                         data = out[[i]]))
  }
  
  if (all(unlist(sapply(X = x, 
                        FUN = function(y) y["status_code"])) == "200")) {
    vapply(x, as.character, character(1))
  }
  else {
    status.code <- unlist(sapply(X = x, 
                                 FUN = function(y) y["status_code"]))
    msg <- sapply(x, as.character)
    
    stop(paste(paste0(status.code[status.code != "200"], 
                      ": ", 
                      msg[status.code != "200"]), 
               collapse="\n"))
  }
}


import_records_unbatched <- function(url = getOption("redcap_bundle")$redcap_url,
                                     token = getOption("redcap_token"),
                                     data, overwriteBehavior,
                                     returnContent) {

  out <- data_frame_to_string(data)
  
  ## Reattach attributes
  attributes(out) <- 
    list("Content-Type" = structure(c("text/html", "utf-8"),
                                    .Names = c("", "charset")))
  
  x <- httr::POST(url=url,
                  body=list(token = token, 
                            content = 'record', 
                            format = 'csv',
                            type = 'flat', 
                            overwriteBehavior = overwriteBehavior,
                            returnContent = returnContent,
                            returnFormat = 'csv', 
                            dateFormat = "YMD",
                            data = out))
  
  if (x$status_code == "200") 
    as.character(x) 
  else 
    redcap_error(x, error_handling = "error")
}

data_frame_to_string <- function(data) {
  paste0(
    utils::capture.output(
      utils::write.table(data, 
                         sep = ",",
                         col.names = TRUE,
                         row.names = FALSE,
                         na = '')
    ),
    collapse = "\n"
  )
}
