#' @name rc_exportReports 
#' @title Export Reports from a REDCap Database
#' 
#' @description Exports reports from a REDCap Database and formats data if requested
#' 
#' @param url A url address to connect to the REDCap API
#' @param token A REDCap API token
#' @param report_id Integer.  Gives the report id of the desired report. 
#' This is located on the Report Builder page of the user interface on REDCap.
#' @param meta_data REDCap project metadata (aka data dictionary). By default, 
#' $meta_data is expected in a REDCap bundle object, as created by \code{rc_setup}.
#' Otherwise, a data.frame containing the metadata must be supplied.
#' 
#' @param format Logical.  Determines whether the data will be formatted with
#' \code{rc_formatRecords} (Default = TRUE)
#' @param ... Additional arguments to be passed to \code{rc_formatRecords}
#' 
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' 
#' @details
#' A record of exports through the API is recorded in the Logging section of 
#' the project.
#' 
#' Reports are exported based on their id number, which can be looked up in 
#' the Reports page of a project
#' 
#' @section REDCap API Documentation (6.5.0):
#' This function allows you to export the data set of a report created on a project's 
#' "Data Exports, Reports, and Stats" page.
#' 
#' Note about export rights (6.0.0+): Please be aware that Data Export user rights will be 
#' applied to this API request. For example, if you have "No Access" data export rights 
#' in the project, then the API report export will fail and return an error. And if you 
#' have "De-Identified" or "Remove all tagged Identifier fields" data export rights, 
#' then some data fields *might* be removed and filtered out of the data set returned 
#' from the API. To make sure that no data is unnecessarily filtered out of your API 
#' request, you should have "Full Data Set" export rights in the project.
#' 
#' @section REDCap Version:
#' 6.0.0+
#' 
#' @section Known REDCap Limitations:
#' None
#' 
#' @author Benjamin Nutter
#' @author Marcus Lehr
#' 
#' @export

rc_exportReports <- function(report_id,
                             url = getOption("redcap_bundle")$redcap_url,
                             token = getOption("redcap_token"),
                             meta_data = getOption("redcap_bundle")$meta_data,
                             format = TRUE, ...,
                             error_handling = getOption("redcap_error_handling")){
  
  #* Secure the meta data.
  if (is.null(meta_data)) 
    stop("$meta_data not found in REDCap bundle. Please create a REDCap bundle containing
    $meta_data with rc_setup() or supply meta_data via a data.frame")
  
  
  if (!is.numeric(report_id)) report_id <- as.numeric(report_id)
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_integerish(x = report_id,
                               len = 1,
                               add = coll)
  
  massert(~ url + token + meta_data,
          fun = checkmate::assert_class,
          classes = list(url = "character", token = "character",
                         meta_data = "data.frame"),
          fixed = list(add = coll))
  
  checkmate::assert_logical(x = format,
                            len = 1,
                            add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"),
                                        add = coll)
  
  checkmate::reportAssertions(coll)
  
  
  
  #* for purposes of the export, we don't need the descriptive fields. 
  #* Including them makes the process more error prone, so we'll ignore them.
  meta_data <- meta_data[!meta_data$field_type %in% "descriptive", ]  
  
  
  body <- list(token = token, 
               content = 'report',
               format = 'csv', 
               returnFormat = 'csv',
               report_id = report_id)
  
  x <- httr::POST(url = url, 
                  body = body)
  
  if (x$status_code != 200) redcap_error(x, error_handling)
  
  x <- utils::read.csv(text = as.character(x), 
                       stringsAsFactors = FALSE, 
                       na.strings = "")
  
  if (format) x = rc_formatRecords(x, meta_data = meta_data, ...)
  
  x 
  
}