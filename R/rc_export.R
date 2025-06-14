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
#' @param token Character. Path to a text file containing your REDCap API token
#' @param data_dict Dataframe. REDCap project data data_dictionary. Required only
#' if the \code{format} or \code{form_complet_auto} options are \code{TRUE}. 
#' By default, this will be fetched from the REDCap bundle option, as created by 
#' \code{rc_bundle}.
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
#'   
#' @param survey_fields Logical. Specifies whether or not to export survey specific
#'  fields. (e.g., "redcap_survey_identifier" and form_name+"_timestamp"). Only
#'  relevant when survey fields are being exported. Default is \code{TRUE}. 
#' @param dag_field Logical. Specifies whether or not to export the 
#'  "redcap_data_access_group" field. This will only appear if DAGs are setup in
#'  the project and you have access to more than one. Default is \code{TRUE}.
#' @param form_complete_auto Logical. If \code{fields} are passed,
#' REDCap does not return form complete fields unless specifically requested.
#' However, if \code{TRUE}, the \code{[form]_complete} fields for any form 
#' from which at least one variable is requested will automatically be 
#' retrieved.
#' @param format Logical.  Determines whether the data will be formatted with
#' \code{rc_format} using the default options (Default = FALSE)
#' @param filter_logic Character. Optional logic filter for record exports. Use REDCap
#'  style syntax- ie. similar to branching logic, calculations, etc.
#' @param modified_after Character. A date string in the format "YYYY-MM-DD HH:MM:SS" 
#'  to specify the earliest data modification date to include in the export. In other
#'  words, only data entered into REDCap after this date will be returned.
#' @param modified_before Character. A date string in the format "YYYY-MM-DD HH:MM:SS"
#'  to specify the latest data modification date to include in the export. In other
#'  words, only data entered into REDCap before this date will be returned.
#' @param ... Additional arguments to be passed to \code{rc_api_call}. Any arguments
#'   accepted by the API may be passed, even if not pre-coded by this function.
#' @param strip Logical. If \code{TRUE}, empty rows and columns will be removed from
#' record_data. See \code{rc_strip} for more information or call seperately for more
#' options. 
#' 
#' @param batch_size Integer.  Specifies the number of subjects to be included 
#'   in each batch of a batched export.  Non-positive numbers export the 
#'   entire project in a single batch. Batching the export may be beneficial 
#'   to prevent tying up smaller servers.  See details for more explanation.
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
#' no more than \code{batch_size} ID's in any single batch.  The batches are 
#' exported from the API and stacked together.
#' 
#' In longitudinal projects, \code{batch_size} may not necessarily be the 
#' number of records exported in each batch.  If \code{batch_size} is 10 and 
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
#' list of IDs is then broken into chunks, each about the size of \code{batch_size}.
#' The batched calls then force the \code{records} argument in each call.
#' 
#' When a user's permissions require a de-identified data export, a batched call 
#' should be expected to fail.  This is because, upon export, REDCap will hash the 
#' identifiers.  When R attempts to pass the hashed identifiers back to REDCap, 
#' REDCap will try to match the hashed identifiers to the unhashed identifiers in the
#' database.  No matches will be found, and the export will fail.
#' 
#' Users who are exporting de-identified data will have to settle for using unbatched
#' calls to the API (ie, \code{batch_size = -1})
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
                       records = NULL, fields = NULL, forms = NULL, events = NULL, 
                       survey_fields = TRUE, dag_field = TRUE,
                       form_complete_auto = FALSE, format = FALSE,
                       filter_logic = '',
                       modified_after = '', modified_before = '',
                       strip = ifelse(is.null(report_id)&is.null(fields),T,F),
                       batch_size = -1, ...
                       ) {

# Checks
  
  required = c('url','token')
  
  # Add data dict to requirements if needed
  if (format | form_complete_auto) {
    if (is.null(data_dict)) 
      stop("data_dict must be supplied when the 'format' or 'form_complete_auto' arguments are TRUE.")
    required = c(required,'data_dict')
  }
  
  # Add ID field to requirements if needed
  if ((!is.null(fields)|!is.null(forms)|batch_size>0) & is.null(report_id)) {
    # Get record_id field names
    id_field = getID(id_field = id_field,
                     data_dict = data_dict)
    required = c(required,'id_field')
  }
  
  # IDs are generally integers. Convert to character if passed
  if (is.numeric(records)) records = as.character(records)
  
  
  validate_args(required = required, record_data = NULL,
                url = url, token = token, data_dict = data_dict, id_field = id_field,
                fields = fields, forms = forms, events = events,
                records = records, survey_fields = survey_fields, dag_field = dag_field,
                form_complete_auto = form_complete_auto, format = format,
                filter_logic = filter_logic, batch_size = batch_size, strip = strip)

  # If a report ID is provided, export the report
  if (!is.null(report_id)) x = rc_api_call(url,token,'report', report_id = report_id)
  
  # Else export records
    else {
      ## Adding default fields may now be redundant 
      # Append default and complete fields to the export
      if (!is.null(fields)|!is.null(forms))
        # Append default fields
        fields <- unique(c(id_field,
                           # As of > v13.3 the redcap fields return an error. 
                           # They are automatically added to the export so long as the record_id field is requested
                           # "redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance",
                           fields))
      
      
      # Add _complete fields
      if (!is.null(data_dict)) {
        
        #* for purposes of the export, we don't need the descriptive fields.
        #* Including them makes the process more error prone, so we'll ignore them.
        ## I believe this only affected get_column_labels (no longer used here)
        data_dict <- data_dict[!data_dict$field_type %in% "descriptive",]
        
        # Auto append complete fields if desired. Auto only useful when manually selecting fields
        if (!is.null(fields) & form_complete_auto) {
          form_complete_fields <- sprintf("%s_complete", unique(data_dict$form_name[data_dict$field_name %in% fields]))
          form_complete_fields <- form_complete_fields[!is.na(form_complete_fields)]
          fields <- unique(c(fields, form_complete_fields))
        }
      }
      
      # Call API
      if (batch_size < 1) {
        x = rc_api_call(url,token,'record', ...,
                        fields = fields, forms = forms, 
                        events = events, records = records,
                        filterLogic = filter_logic,
                        exportSurveyFields = tolower(survey_fields),
                        exportDataAccessGroups = tolower(dag_field),
                        dateRangeBegin = modified_after, 
                        dateRangeEnd = modified_before
                        )
      } else {
        x <- batched_export(url, token,
                             batch_size = batch_size,
                             id_field = id_field)
        }
      }

# Formatting ------------------------------------------------------------------

    
  if (format) x = rc_format(x, data_dict = data_dict)
  
  if (strip) x = rc_strip(x, id_field = id_field)
  
  return(x)
  }

# Non-exported functions ----------------------------------------------------

#*** BATCHED EXPORT
batched_export <- function(url, token,
                            batch_size, id_field)
{
 ## Function overview:
  #* 1. Get the IDs column
  #* 2. Restrict to unique IDs
  #* 3. Determine if the IDs look hashed (de-identified)
  #* 4. Give warning about potential problems joining hashed IDs
  #* 5. Read batches
  #* 6. Combine tables
  #* 7. Return full data frame
  
  
  #* 1. Get the IDs column
  IDs = rc_api_call(url,token,'record', fields = id_field, 
                    filterLogic = filter_logic, ...)
  
  #* 2. Restrict to unique IDs
  unique_ids <- unique(IDs[[id_field]])
  
  #* 3. Determine if the IDs look hashed (de-identified)
  #* 4. Give warning about potential problems joining hashed IDs
  if (all(nchar(unique_ids) == 32L))
  {
    warning("The record IDs in this project appear to be de-identified. ",
            "Subject data may not match across batches. ",
            "See 'Deidentified Batched Calls' in '?rc_export'")
  }
  
  #* Determine batch numbers for the IDs.
  batch.number <- rep(seq_len(ceiling(length(unique_ids) / batch_size)),
                      each = batch_size,
                      length.out = length(unique_ids))
  
  #* Make a list to hold each of the batched calls
  #* Borrowed from http://stackoverflow.com/a/8099431/1017276
  batch_list <- vector("list", max(batch.number))
  
  #* 5. Read batches
  for (i in unique(batch.number))
  {
    # Export batch
    batch_list[[i]] = rc_api_call(url,token,'record', 
                                  records = unique_ids[batch.number == i],
                                  fields = fields, forms = forms, events = events,
                                  filterLogic = filter_logic, ...,
                                  exportSurveyFields = tolower(survey_fields),
                                  exportDataAccessGroups = tolower(dag_field),
                                  dateRangeBegin = modified_after, 
                                  dateRangeEnd = modified_before
                                  )
    
    # Pause
    Sys.sleep(1)
  }
  
  #* 6. Combine tables and return
  return( do.call("rbind", batch_list) )
}
