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
#' @param url Character. A url address to connect to the REDCap API
#' @param token Character. A REDCap API token
#' @param data_dict Dataframe. REDCap project data data_dictionary. By default, 
#' this will be fetched from the REDCap bundle option, as created by \code{rc_bundle}.
#' Otherwise, a data.frame containing the project data dictionary must be supplied.
#' @param id_field Character. The name of the record_id field for your REDCap project.
#' 
#' @param records Character. A vector of study id's to be returned.  If \code{NULL}, all 
#'   subjects are returned.
#' @param fields Character. A vector of fields to be returned.  If \code{NULL}, 
#'   all fields are returned.
#' @param forms Character. A vector of forms to be returned.  If \code{NULL}, 
#'   all forms are returned.
#' @param events Character. A vector of events to be returned from a longitudinal database.
#'   If \code{NULL}, all events are returned.
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
#' @param form_complete_auto Logical. If \code{fields} are passed,
#' REDCap does not return form complete fields unless specifically requested.
#' However, if \code{TRUE}, the \code{[form]_complete} fields for any form 
#' from which at least one variable is requested will automatically be 
#' retrieved.
#'   
#' @param format Logical.  Determines whether the data will be formatted with
#' \code{rc_format} (Default = FALSE)
#' @param ... Additional arguments to be passed to \code{rc_format}
#' @param strip Logical. If \code{TRUE}, empty rows and columns will be removed from
#' record_data. See \code{rc_strip} for more information or call seperately for more
#' options. 
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
#'  
#' Note about export rights (6.0.0+): Please be aware that Data Export user rights will be 
#' applied to this API request. For example, if you have "No Access" data export rights 
#' in the project, then the API data export will fail and return an error. And if you 
#' have "De-Identified" or "Remove all tagged Identifier fields" data export rights, 
#' then some data fields *might* be removed and filtered out of the data set returned 
#' from the API. To make sure that no data is unnecessarily filtered out of your API 
#' request, you should have "Full Data Set" export rights in the project.
#' 
#' REDCap Version:
#' >= 6.0.0 
#' 
#' Deidentified Batched Calls:
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
                       data_dict = getOption("redcap_bundle")$data_dict,
                       id_field = getOption("redcap_bundle")$id_field,
                       records = NULL, fields = NULL, forms = NULL,
                       events = NULL, survey = TRUE, dag = TRUE,
                       form_complete_auto = FALSE,
                       format = FALSE, ..., strip = TRUE,
                       colClasses = NA, batch.size = -1, 
                       error_handling = getOption("redcap_error_handling")
                       ) {

# Checks ------------------------------------------------------------------
  
  required = c('url','token')
  
  if (format | form_complete_auto) {
    if (is.null(data_dict)) 
      stop("data_dict must be supplied when the 'format' or 'form_complete_auto' arguments are TRUE.")
    required = c(required,'data_dict')
  }
  
  if (!is.null(fields)|!is.null(forms) & is.null(report_id)) {
    # Get record_id field name
    id_field = getID(id_field = id_field,
                     data_dict = data_dict)
    required = c(required,'id_field')
  }
  
  validate_args(required = required,
                url = url, token = token, data_dict = data_dict,
                fields = fields, forms = forms, events = events,
                records = records, survey = survey, dag = dag,
                form_complete_auto = form_complete_auto, format = format,
                colClasses = colClasses, batch.size = batch.size,
                error_handling = error_handling, strip = strip)
    

# Export Report -----------------------------------------------------------

    if (!is.null(report_id)) {
      
      # Create body for POST
      body <- list(token = token, 
                   content = 'report',
                   format = 'csv', 
                   report_id = report_id,
                   csvDelimiter = '',
                   rawOrLabel = 'raw',
                   rawOrLabelHeaders = 'raw',
                   exportCheckboxLabel = 'false',
                   returnFormat = 'csv')
      
      # Export data
      x <- httr::POST(url = url, 
                      body = body)
      
      # Report errors
      if (x$status_code != 200) redcap_error(x, error_handling)
      
      # Check for data
      if (length(x$content)==1) stop("No records were returned from report ", report_id)
      
      # Convert data to data.frame
      x <- utils::read.csv(text = as.character(x), 
                           stringsAsFactors = FALSE, 
                           na.strings = "")
    }
    

# Export Records ----------------------------------------------------------

    else {
      # Append default and complete fields to the export
      if (!is.null(fields)|!is.null(forms))
        # Append default fields
        fields <- unique(c(id_field,
                           "redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance",
                           fields))
      
      
      if (!is.null(data_dict)) {
        
        #* for purposes of the export, we don't need the descriptive fields.
        #* Including them makes the process more error prone, so we'll ignore them.
        ## I believe this only affected checkbox_suffixes (no longer used here)
        data_dict <- data_dict[!data_dict$field_type %in% "descriptive",]
        
        # Auto append complete fields if desired. Auto only useful when manually selecting fields
        if (!is.null(fields) & form_complete_auto) {
          form_complete_fields <- sprintf("%s_complete", unique(data_dict$form_name[data_dict$field_name %in% fields]))
          form_complete_fields <- form_complete_fields[!is.na(form_complete_fields)]
          fields <- unique(c(fields, form_complete_fields))
        }
      }
      
      # Create body for POST()
      body <- list(token = token,
                   content = 'record',
                   format = 'csv',
                   type = 'flat',
                   csvDelimiter = '',
                   rawOrLabel = 'raw',
                   rawOrLabelHeaders = 'raw',
                   exportCheckboxLabel = 'false',
                   exportSurveyFields = tolower(survey),
                   exportDataAccessGroups = tolower(dag),
                   returnFormat = 'csv')
      
      # Expand body to include provided selections
      if (!is.null(fields)) body[['fields']] <- paste0(fields, collapse=",")
      if (!is.null(forms)) body[['forms']] <- paste0(forms, collapse=",")
      if (!is.null(events)) body[['events']] <- paste0(events, collapse=",")
      if (!is.null(records)) body[['records']] <- paste0(records, collapse=",")
      
      if (batch.size < 1) {
        x <- unbatched(url = url,
                       token = token,
                       body = body,
                       id = id_field,
                       colClasses = colClasses,
                       error_handling = error_handling)
      } else {
        x <- batched(url = url,
                     token = token,
                     body = body,
                     batch.size = batch.size,
                     id = id_field,
                     colClasses = colClasses,
                     error_handling = error_handling)
        }
      }

# Formatting ------------------------------------------------------------------

    
    if (format) x = rc_format(x, data_dict = data_dict, ...)
  
    if (strip) x = rc_strip(x)
    
    x
  }

# Non-exported functions ----------------------------------------------------

#*** UNBATCHED EXPORT
unbatched <- function(url = url,
                      token = token,
                      body, id, colClasses, error_handling)
{
  colClasses[[id]] <- "character"
  colClasses <- colClasses[!vapply(colClasses,
                                   is.na,
                                   logical(1))]
  
  x <- httr::POST(url = url, 
                  body = body)
  
  if (x$status_code != 200) redcap_error(x, error_handling = error_handling)
  
  # Check for data
  if (length(x$content)==1) stop("No records were returned")
  
  x <- as.character(x)
  # probably not necessary for data.  Useful for meta data though. (See Issue #99)
  # x <- iconv(x, "utf8", "ASCII", sub = "")
  utils::read.csv(text = x, 
                  stringsAsFactors = FALSE, 
                  na.strings = "",
                  colClasses = colClasses)
}


#*** BATCHED EXPORT
batched <- function(url = url,
                    token = token,
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
