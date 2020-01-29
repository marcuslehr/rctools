#' @name rc_import
#' @title Import Records to a REDCap Database
#' 
#' @description Imports records from a \code{data.frame} to a REDCap Database
#'
#' @param url A url address to connect to the REDCap API
#' @param token A REDCap API token
#' @param record_data A \code{data.frame} to be imported to the REDCap project.
#' @param bundle A \code{redcapBundle} object as created by
#'   \code{rc_setup}.
#' @param overwriteBehavior Character string.  'normal' prevents blank
#'   fields from overwriting populated fields.  'overwrite' causes blanks to
#'   overwrite data in the REDCap database.
#' @param returnContent Character string.  'count' returns the number of
#'   records imported; 'ids' returns the record ids that are imported;
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
                       overwriteBehavior = c('normal', 'overwrite'),
                       returnContent = c('count', 'ids', 'nothing'),
                       returnData = FALSE, logfile = "", 
                       bundle = getOption("redcap_bundle"), batch.size=-1)
{
  
  coll <- checkmate::makeAssertCollection()
  
  massert(~ url + token + bundle + record_data,
          fun = checkmate::assert_class,
          classes = list(url = "character", token = "character",
                         bundle = "redcapBundle",
                         record_data = "data.frame"),
          null.ok = list(bundle = TRUE),
          fixed = list(add = coll))
  
  overwriteBehavior <- 
    checkmate::matchArg(x = overwriteBehavior, 
                        choices = c('normal', 'overwrite'),
                        add = coll)
  
  returnContent <- 
    checkmate::matchArg(x = returnContent, 
                        choices = c('count', 'ids', 'nothing'),
                        add = coll)
  
  checkmate::assert_logical(x = returnData,
                            len = 1,
                            add = coll)
  
  checkmate::assert_character(x = logfile,
                              len = 1,
                              add = TRUE)
  
  checkmate::assert_integerish(x = batch.size,
                               len = 1,
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (is.null(bundle$data_dict))
    message("bundle$data_dict not found. Please supply a bundle object containing $data_dict from rc_setup()")
  else
    data_dict <- bundle$data_dict
  
  if (!is.null(bundle$version)) version <- bundle$version
  
  try(
    if (utils::compareVersion(version, "5.5.21") == -1 )
      data_dict <- syncUnderscoreCodings(record_data, 
                                         data_dict, 
                                         export = FALSE),
    silent = T)
  
  suffixed <- checkbox_suffixes(fields = data_dict$field_name,
                                data_dict = data_dict, 
                                version = version)
  
  form_names <- unique(data_dict$form_name)
  
  data_dict <- 
    data_dict[data_dict$field_name %in% 
                sub(pattern = "___[a-z,A-Z,0-9,_]+", 
                    replacement = "", 
                    x = names(record_data)), ]
  
  #** Check that all of the variable names in 'record_data' exist in REDCap Database
  .checkbox <- data_dict[data_dict$field_type == "checkbox", ]
  
  .opts <- lapply(X = .checkbox$select_choices_or_calculations, 
                  FUN = function(x) unlist(strsplit(x, 
                                                    split = " [|] ")))
  .opts <- lapply(X = .opts, 
                  FUN = function(x) gsub(pattern = ",[[:print:]]+", 
                                         replacement = "", 
                                         x = x))
  
  check_var <- paste(rep(.checkbox$field_name, 
                         vapply(.opts, 
                                FUN = length,
                                FUN.VALUE = numeric(1))), 
                     unlist(.opts), 
                     sep="___")
  
  with_complete_fields <- 
    c(unique(data_dict$field_name), 
      paste(form_names, "_complete", sep=""), 
      check_var)
  
  #** Remove survey identifiers and data access group fields from data
  w.remove <- 
    which(names(record_data) %in% 
            c("redcap_survey_identifier",
              paste0(unique(data_dict$form_name), "_timestamp"),
              "redcap_data_access_group"))
  if (length(w.remove)) record_data <- record_data[-w.remove]
  
  if (!all(names(record_data) %in% c(with_complete_fields, "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")))
  {
    coll$push(paste0("The variables ", 
                     paste(names(record_data)[!names(record_data) %in% with_complete_fields], collapse=", "),
                     " do not exist in the REDCap Data data_dictionary"))
  }
  
  #** Check that the study id exists in record_data
  if (!data_dict$field_name[1] %in% names(record_data))
  {
    coll$push(paste0("The variable '", 
                     data_dict$field_name[1], 
                     "' cannot be found in 'record_data'. ",
                     "Please include this variable and place it in the first column."))
  }
  
  #** If the study id is not in the the first column, move it and print a warning
  if (data_dict$field_name[1] %in% names(record_data) && 
      data_dict$field_name[1] != names(record_data)[1])
  {
    message("The variable'", data_dict$field_name[1], 
            "' was not in the first column. ",
            "It has been moved to the first column.")
    w <- which(names(record_data) == data_dict$field_name[1])
    record_data <- record_data[c(w, (1:length(record_data))[-w])]
  }
  
  #** Confirm that date fields are either character, Date class, or POSIXct
  date_vars <- data_dict$field_name[grepl("date_", data_dict$text_validation_type_or_show_slider_number)]
  
  bad_date_fmt <- 
    !vapply(X = record_data[date_vars], 
            FUN = function(x) is.character(x) | "Date" %in% class(x) | "POSIXct" %in% class(x),
            FUN.VALUE = logical(1))
  
  if (any(bad_date_fmt))
  {
    coll$push(paste0("The variables '", 
                     paste(date_vars[bad_date_fmt], collapse="', '"),
                     "' must have class Date, POSIXct, or character."))
  }
  
  #*** Remove calculated fields
  calc_field <- data_dict$field_name[data_dict$field_type == "calc"]
  
  if (length(calc_field) > 0)
  {
    message("The variable(s) '", 
            paste(calc_field, collapse="', '"),
            "' are calculated fields and cannot be imported. ",
            "They have been removed from the imported data frame.")
    record_data <- record_data[!names(record_data) %in% calc_field]
  }
  
  checkmate::reportAssertions(coll)
  
  
  idvars <- 
    if ("redcap_event_name" %in% names(record_data)) 
      c(data_dict$field_name[1], "redcap_event_name") 
  else 
    data_dict$field_name[1]
  
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
  
  if (batch.size > 0)
  {
    import_records_batched(url = url,
						               token = token, 
                           data = record_data,
                           batch.size = batch.size,
                           overwriteBehavior = overwriteBehavior,
                           returnContent = returnContent)
  }
  else
  {
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
                                   returnContent)
{
  n.batch <- nrow(data) %/% batch.size + 1
  
  ID <- data.frame(row = 1:nrow(data))
  
  ID$batch.number <- rep(1:n.batch, 
                         each = batch.size, 
                         length.out = nrow(data))
  
  data[is.na(data)] <- ""
  
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
  
  for (i in seq_along(out))
  {
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
                        FUN = function(y) y["status_code"])) == "200"))
  {
    vapply(x, as.character, character(1))
  }
  else 
  {
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
                                     returnContent)
{
  data[is.na(data)] <- ""

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

data_frame_to_string <- function(data)
{
  paste0(
    utils::capture.output(
      utils::write.table(data, 
                         sep = ",",
                         col.names = TRUE,
                         row.names = FALSE)
    ),
    collapse = "\n"
  )
}
