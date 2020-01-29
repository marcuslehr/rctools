#' @name rc_export
#' 
#' @title Export Records from a REDCap Database
#' @description Exports records from a REDCap Database, allowing for 
#'   subsets of subjects, fields, records, and events. By default, all
#'   records will be exported. If a report ID is supplied, only data
#'   from that report will be exported. 
#'   
#' @param report_id Numeric. ID number for a report created in REDCap. 
#'   
#' @param url A url address to connect to the REDCap API
#' @param token A REDCap API token
#' @param bundle A \code{redcapBundle} object as created by \code{rc_setup}. 
#'   For the purposes of this function, the data dictionary and events data 
#'   may be required.
#' 
#' @param records A vector of study id's to be returned.  If \code{NULL}, all 
#'   subjects are returned.
#' @param fields A character vector of fields to be returned.  If \code{NULL}, 
#'   all fields are returned.
#' @param forms A character vector of forms to be returned.  If \code{NULL}, 
#'   all forms are returned.
#' @param events A character vector of events to be returned from a 
#'   longitudinal database.  If \code{NULL}, all events are returned.
#' @param survey Logical. Specifies whether or not to export the survey identifier field 
#'   (e.g., "redcap_survey_identifier") or survey timestamp fields 
#'   (e.g., form_name+"_timestamp") when surveys are utilized in the project. 
#'   If you do not pass in this flag, it will default to "false". If set to 
#'   "true", it will return the redcap_survey_identifier field and also the 
#'   survey timestamp field for a particular survey when at least 
#'   one field from that survey is being exported. NOTE: If the survey 
#'   identifier field or survey timestamp fields are imported via API data 
#'   import, they will simply be ignored since they are not real fields in 
#'   the project but rather are pseudo-fields.
#' @param dag Logical. Specifies whether or not to export the "redcap_data_access_group" 
#'   field when data access groups are utilized in the project. If you do not 
#'   pass in this flag, it will default to "false". NOTE: This flag is only 
#'   viable if the user whose token is being used to make the API request is 
#'   *not* in a data access group. If the user is in a group, then this 
#'   flag will revert to its default value.
#' @param form_complete_auto \code{logical(1)}. When \code{TRUE},
#'  the \code{[form]_complete} fields for any form 
#'   from which at least one variable is requested will automatically
#'   be retrieved.  When \code{FALSE} (Default), these fields must be 
#'   explicitly requested.  
#'   
#' @param format Logical.  Determines whether the data will be formatted with
#' \code{rc_format} (Default = TRUE)
#' @param ... Additional arguments to be passed to \code{rc_format}
#' 
#' @param colClasses A (named) vector of colum classes passed to 
#'   \code{\link[utils]{read.csv}} calls. 
#'   Useful to force the interpretation of a column in a specific type and 
#'   avoid an unexpected recast.
#' @param batch.size Integer.  Specifies the number of subjects to be included 
#'   in each batch of a batched export.  Non-positive numbers export the 
#'   entire project in a single batch. Batching the export may be beneficial 
#'   to prevent tying up smaller servers.  See details for more explanation.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' 
#' @details
#' A record of exports through the API is recorded in the Logging section 
#' of the project.
#' 
#' It is unnecessary to include "redcap_event_name" or the "redcap_repeat" variables
#' in the fields argument. These fields are automatically exported for any 
#' longitudinal database. If the user does include them in the fields argument, 
#' they are removed quietly in the parameter checks.
#' 
#' A 'batched' export is one where the export is performed over a series of 
#' API calls rather than one large call.  For large projects on small servers, 
#' this may prevent a single user from tying up the server and forcing others 
#' to wait on a larger job.  The batched export is performed by first 
#' calling the API to export the subject identifier field (the first field
#' in the meta data).  The unique ID's are then assigned a batch number with 
#' no more than \code{batch.size} ID's in any single batch.  The batches are 
#' exported from the API and stacked together.
#' 
#' In longitudinal projects, \code{batch.size} may not necessarily be the 
#' number of records exported in each batch.  If \code{batch.size} is 10 and 
#' there are four records per patient, each batch will consist of 40 records.  
#' Thus, if you are concerned about tying up the server with a large, 
#' longitudinal project, it would be prudent to use a smaller batch size.
#' 
#' @section 
#' Note about export rights (6.0.0+): Please be aware that Data Export user rights will be 
#' applied to this API request. For example, if you have "No Access" data export rights 
#' in the project, then the API data export will fail and return an error. And if you 
#' have "De-Identified" or "Remove all tagged Identifier fields" data export rights, 
#' then some data fields *might* be removed and filtered out of the data set returned 
#' from the API. To make sure that no data is unnecessarily filtered out of your API 
#' request, you should have "Full Data Set" export rights in the project.
#' 
#' @section REDCap Version:
#' >= 6.0.0 
#' 
#' @section Deidentified Batched Calls:
#' Batched calls to the API are not a feature of the REDCap API, but may be imposed 
#' by making multiple calls to the API.  The process of batching the export requires
#' that an initial call be made to the API to retrieve only the record IDs.  The
#' list of IDs is then broken into chunks, each about the size of \code{batch.size}.
#' The batched calls then force the \code{records} argument in each call.
#' 
#' When a user's permissions require a de-identified data export, a batched call 
#' should be expected to fail.  This is because, upon export, REDCap will hash the 
#' identifiers.  When R attempts to pass the hashed identifiers back to REDCap, 
#' REDCap will try to match the hashed identifiers to the unhashed identifiers in the
#' database.  No matches will be found, and the export will fail.
#' 
#' Users who are exporting de-identified data will have to settle for using unbatched
#' calls to the API (ie, \code{batch.size = -1})
#' 
#' @author Jeffrey Horner
#' @author Marcus Lehr
#' 
#' @references
#' Please refer to your institution's API documentation.
#' 
#' Additional details on API parameters are found on the package wiki at
#' \url{https://github.com/nutterb/redcapAPI/wiki/REDCap-API-Parameters}
#' 
#' This functionality was originally developed by Jeffrey Horner in the \code{redcap} package.
#' \url{https://github.com/vubiostat/redcap}
#' 
#' See also \code{read_redcap_oneshot} in the \code{REDCapR} package by Will Beasley.
#' \url{https://github.com/OuhscBbmc/REDCapR}
#' 
#' Borrowed code from http://stackoverflow.com/a/8099431/1017276 to 
#' create a list of arbitrary length.
#' 
#' @export

rc_export <- function(report_id = NULL,
                       url = getOption("redcap_bundle")$redcap_url,
                       token = getOption("redcap_token"),
                       bundle = getOption("redcap_bundle"),
                       records = NULL, fields = NULL, forms = NULL,
                       events = NULL, survey = TRUE, dag = TRUE,
                       form_complete_auto = FALSE,
                       format = TRUE, ...,
                       colClasses = NA, batch.size = -1, 
                       error_handling = getOption("redcap_error_handling")
                       ) {
  
  # Initial coercions
  if (is.null(url) & !is.null(bundle)) url = bundle$redcap_url 
  
  if (is.numeric(records)) records <- as.character(records)
  
  
  #* Error Collection Object
  coll <- checkmate::makeAssertCollection()
  
  massert(~ url + token + fields + forms + records + events,
          fun = checkmate::assert_character,
          null.ok = list(fields = T, forms = T,
                         records = T, events = T),
          fixed = list(add = coll))
  
  checkmate::assert_logical(x = format,
                              len = 1,
                              add = coll)
  
  # checkmate::reportAssertions(coll)
    

# Export Report -----------------------------------------------------------

    if (!is.null(report_id)) {
      
      # Error checking
      # coll <- checkmate::makeAssertCollection()
      
      checkmate::assert_integerish(x = report_id,
                                   len = 1,
                                   add = coll)
      
      if (format & !is.null(bundle$data_dict)) {
        data_dict = bundle$data_dict
        checkmate::assert_class(x = data_dict,
                                classes = 'data.frame',
                                add = coll)
      } 
      if (format & is.null(bundle$data_dict)) stop("When `format=TRUE`, $data_dict must be supplied in REDCap bundle")
        
        
      error_handling <- checkmate::matchArg(x = error_handling, 
                                            choices = c("null", "error"),
                                            add = coll)
      checkmate::reportAssertions(coll)
      
      # Create body for POST
      body <- list(token = token, 
                   content = 'report',
                   format = 'csv', 
                   returnFormat = 'csv',
                   report_id = report_id)
      
      # Export data
      x <- httr::POST(url = url, 
                      body = body)
      
      # Report errors
      if (x$status_code != 200) redcap_error(x, error_handling)
      
      # Convert data to data.frame
      x <- utils::read.csv(text = as.character(x), 
                           stringsAsFactors = FALSE, 
                           na.strings = "")
    }
    

# Export Records ----------------------------------------------------------

    else {    
    
      # Error checking
      if (is.null(bundle)) 
        stop("A REDCap bundle containing $data_dict and $events is required. Please supply
      a REDCap bundle as created by rc_setup()")
      
      # coll <- checkmate::makeAssertCollection()
      
      massert(~ survey + dag + form_complete_auto,
              fun = checkmate::assert_logical,
              fixed = list(len = 1,
                           add = coll))
      
      checkmate::assert_class(x = bundle,
                              classes = "redcapBundle",
                              add = coll)
      
      checkmate::assert_integerish(x = batch.size,
                                   len = 1,
                                   add = coll)
      
      error_handling <- checkmate::matchArg(x = error_handling,
                                            choices = c("null", "error"),
                                            add = coll)
      
      # checkmate::reportAssertions(coll)
    
    
      #* Secure the data dictionary
      if (!is.null(bundle$data_dict))
        data_dict <- bundle$data_dict
      else
        stop("$data_dict not found in bundle object. Please supply a REDCap bundle object, as
           created by rc_setup(), containing $data_dict")
    
      #* for purposes of the export, we don't need the descriptive fields.
      #* Including them makes the process more error prone, so we'll ignore them.
      data_dict <- data_dict[!data_dict$field_type %in% "descriptive", ]
      
      
      #* Secure the events table
      # events_list = NULL
      if (!is.null(events) & !is.null(bundle$events))
        events_list <- bundle$events
      
      if (!is.null(events) & is.null(bundle$events)) 
        warning("$events not found in bundle object. The supplied events list cannot be validated.")
      
      
      #* Check that all event names exist in the events list
      if (!is.null(events) && inherits(events_list, "data.frame"))
      {
        bad_events <- events[!events %in% events_list$unique_event_name]
        if (length(bad_events))
          coll$push(paste0("The following are not valid event names: ",
                           paste0(bad_events, collapse = ", ")))
      }
    
      form_complete_fields <-
        sprintf("%s_complete",
                unique(data_dict$form_name))
      form_complete_fields <-
        form_complete_fields[!is.na(form_complete_fields)]
      
      #* Check that all fields exist in the meta data
      if (!is.null(fields))
      {
        bad_fields <- fields[!fields %in% c(data_dict$field_name,
                                            form_complete_fields)]
        if (length(bad_fields))
          coll$push(paste0("The following are not valid field names: ",
                           paste0(bad_fields, collapse = ", ")))
      }
      
      #* Check that all form names exist in the meta data
      if (!is.null(forms))
      {
        bad_forms <- forms[!forms %in% data_dict$form_name]
        if (length(bad_forms))
          coll$push(paste0("The following are not valid form names: ",
                           paste0(bad_forms, collapse = ", ")))
      }
    
      checkmate::reportAssertions(coll)
      
    
      #* Create the vector of field names
      if (!is.null(fields)) #* fields were provided
      {
        # redcap_event_name is automatically included in longitudinal projects
        field_names <- fields[!fields %in% c("redcap_event_name", 
                                             "redcap_repeat_instrument", 
                                             "redcap_repeat_instance")]
      }
      else if (!is.null(forms))
      {
        field_names <- data_dict$field_name[data_dict$form_name %in% forms]
      }
      else
        #* fields were not provided, default to all fields.
        field_names <- data_dict$field_name
      
      #* Expand 'field_names' to include fields from specified forms.
      if (!is.null(forms))
        field_names <-
        unique(c(field_names,
                 data_dict$field_name[data_dict$form_name %in% forms]))
    
    
      suffixed <-
        checkbox_suffixes(
          # The subset prevents `[form]_complete` fields from
          # being included here.
          fields = field_names[field_names %in% data_dict$field_name],
          data_dict = data_dict
        )
      
      # Identify the forms from which the chosen fields are found
      included_form <-
        unique(
          data_dict$form_name[data_dict$field_name %in% field_names]
        )
      
      # Add the form_name_complete column to the export
      if (form_complete_auto){
        field_names <- c(field_names,
                         sprintf("%s_complete", included_form))
      }
      
      body <- list(token = token,
                   content = 'record',
                   format = 'csv',
                   type = 'flat',
                   exportSurveyFields = tolower(survey),
                   exportDataAccessGroups = tolower(dag),
                   returnFormat = 'csv')
      
      
      body[['fields']] <- paste0(field_names, collapse=",")
      if (!is.null(forms)) body[['forms']] <- paste0(forms, collapse=",")
      if (!is.null(events)) body[['events']] <- paste0(events, collapse=",")
      if (!is.null(records)) body[['records']] <- paste0(records, collapse=",")
      
      if (batch.size < 1) {
        x <- unbatched(url = url,
                       token = token,
                       body = body,
                       id = data_dict$field_name[1],
                       colClasses = colClasses,
                       error_handling = error_handling)
      } else {
        x <- batched(url = url,
                     token = token,
                     body = body,
                     batch.size = batch.size,
                     id = data_dict$field_name[1],
                     colClasses = colClasses,
                     error_handling = error_handling)
        }
      }

# Format ------------------------------------------------------------------

    
    if (format) x = rc_format(x, data_dict = data_dict, ...)
    
    x
  }

# Non-exported functions ----------------------------------------------------

#*** UNBATCHED EXPORT
unbatched <- function(url = getOption("redcap_bundle")$redcap_url,
                      token = getOption("redcap_token"),
                      body, id, colClasses, error_handling)
{
  colClasses[[id]] <- "character"
  colClasses <- colClasses[!vapply(colClasses,
                                   is.na,
                                   logical(1))]
  
  x <- httr::POST(url = url, 
                  body = body)
  
  if (x$status_code != 200) redcap_error(x, error_handling = error_handling)
  
  x <- as.character(x)
  # probably not necessary for data.  Useful for meta data though. (See Issue #99)
  # x <- iconv(x, "utf8", "ASCII", sub = "")
  utils::read.csv(text = x, 
                  stringsAsFactors = FALSE, 
                  na.strings = "",
                  colClasses = colClasses)
}


#*** BATCHED EXPORT
batched <- function(url = getOption("redcap_bundle")$redcap_url,
                    token = getOption("redcap_token"),
                    body, batch.size, id, colClasses, error_handling)
{
  colClasses[[id]] <- "character"
  colClasses <- colClasses[!vapply(colClasses,
                                   is.na,
                                   logical(1))]
  
  #* 1. Get the IDs column
  #* 2. Restrict to unique IDs
  #* 3. Determine if the IDs look hashed (de-identified)
  #* 4. Give warning about potential problems joining hashed IDs
  #* 5. Read batches
  #* 6. Combine tables
  #* 7. Return full data frame
  
  
  #* 1. Get the IDs column
  id_body <- body
  id_body[['fields']] <- id
  IDs <- httr::POST(url = url,
                    body = id_body)
  
  if (IDs$status_code != 200) redcap_error(IDs, error_handling)
  
  IDs <- as.character(IDs)
  # probably not necessary for data.  Useful for meta data though. (See Issue #99)
  # IDs <- iconv(IDs, "utf8", "ASCII", sub = "")
  IDs <- utils::read.csv(text = IDs,
                         stringsAsFactors = FALSE,
                         na.strings = "",
                         colClasses = colClasses[id])
  
  #* 2. Restrict to unique IDs
  unique_id <- unique(IDs[[id]])
  
  #* 3. Determine if the IDs look hashed (de-identified)
  #* 4. Give warning about potential problems joining hashed IDs
  if (all(nchar(unique_id) == 32L))
  {
    warning("The record IDs in this project appear to be de-identified. ",
            "Subject data may not match across batches. ",
            "See 'Deidentified Batched Calls' in '?rc_export'")
  }
  
  #* Determine batch numbers for the IDs.
  batch.number <- rep(seq_len(ceiling(length(unique_id) / batch.size)),
                      each = batch.size,
                      length.out = length(unique_id))
  
  #* Make a list to hold each of the batched calls
  #* Borrowed from http://stackoverflow.com/a/8099431/1017276
  batch_list <- vector("list", max(batch.number))
  
  #* 5. Read batches
  for (i in unique(batch.number))
  {
    body[['records']] <- paste0(unique_id[batch.number == i], collapse = ",")
    x <- httr::POST(url = url, 
                    body = body)
    
    if (x$status_code != 200) redcap_error(x, error_handling = "error")
    
    x <- as.character(x)
    # probably not necessary for data.  Useful for meta data though. (See Issue #99)
    # x <- iconv(x, "utf8", "ASCII", sub = "")
    batch_list[[i]] <- utils::read.csv(text = x,
                                       stringsAsFactors = FALSE,
                                       na.strings = "",
                                       colClasses = colClasses)
    Sys.sleep(1)
  }
  
  #* 6. Combine tables and return
  do.call("rbind", batch_list)
}
