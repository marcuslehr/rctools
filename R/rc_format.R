#' @name rc_format
#'
#' @title Format records data
#' @description  Uses REDCap project metadata to format records data.
#' @details This function takes raw REDCap data and adds column labels, converts
#' columns to numeric/character/factor as appropriate, and applies factor and 
#' checkbox labels. Formatting details of the returned dataframe can be found
#' via attributes(record_data)$redcap_formatting.
#' 
#' @param record_data Dataframe. Record data export from REDCap
#' @param data_dict Dataframe. REDCap project data data_dictionary. By default, 
#' this will be fetched from the REDCap bundle option, as created by \code{rc_bundle}.
#' Otherwise, a data.frame containing the project data dictionary must be supplied.
#' @param event_data Dataframe. REDCap event data. By default, this will be fetched 
#' from the REDCap bundle option, as created by \code{rc_bundle}.
#' 
#' @param factor_labels Logical.  Determines whether categorical fields (eg radio,
#' dropdown, checkbox) will be formatted as numeric codes or labels. Note that 
#' for checkbox fields, 0 (representing an unchecked box) will be replaced with NA.
#' @param col_labels Logical.  Determines if the field labels are applied to the 
#' dataframe columns.
#' @param dates Logical. Determines if date variables are converted to POSIXct 
#' format during the download.
#' @param event_labels Logical. Determines if event labels are applied to redcap_event_name 
#' column (Default = \code{TRUE}) or left as unique event names.
#' @param strip Logical. If \code{FALSE}, empty rows and columns will be removed from
#' record_data. See \code{rc_strip} for more information or call separately for more
#' options. 
#' 
#' @importFrom magrittr '%>%'
#' 
#' @author Marcus Lehr
#' @author Benjamin Nutter
#'
#' @export

rc_format <- function(record_data, data_dict = getOption("redcap_bundle")$data_dict,
                      event_data = getOption("redcap_bundle")$event_data,
                      factor_labels = TRUE, col_labels = TRUE, dates = TRUE,
                      event_labels = TRUE, strip = FALSE)
{
  
  validate_args(required = c('record_data','data_dict'),
                record_data = record_data, data_dict = data_dict,
                factor_labels = factor_labels, col_labels = col_labels, dates = dates,
                event_labels = event_labels,
                event_data = event_data, strip = strip)

  
  #* for purposes of the export, we don't need the descriptive fields. 
  #* Including them causes errors in checkbox_suffixes
  data_dict <- data_dict[!data_dict$field_type %in% "descriptive",]
  
  # Apply formatting/type conversions
  record_data <- fieldToVar(records = record_data, 
                            data_dict = data_dict, 
                            factor_labels = factor_labels, 
                            dates = dates)
  
  # All NA cols are formatted as logical and cause join issues
  if ('redcap_repeat_instrument' %in% names(record_data))
    record_data$redcap_repeat_instrument = as.character(record_data$redcap_repeat_instrument)
  if ('redcap_repeat_instance' %in% names(record_data))
    record_data$redcap_repeat_instance = as.character(record_data$redcap_repeat_instance)
  
  if (col_labels){
    # Get field names
    fields = names(record_data)[names(record_data) %in% data_dict$field_name]
    
    # Currently generating labels for all fields
    column_labels = checkbox_suffixes(field_names = data_dict$field_name,
                                      data_dict = data_dict)
    
    # Apply column labels
    Hmisc::label(record_data) = as.list(column_labels[match(names(record_data),names(column_labels))])
    
    # Hmisc inserts 'labelled' into the column classes
    # Removing the labelled class to prevent downstream issues
    for (col in seq_along(record_data)) {
      class(record_data[[col]]) <- setdiff(class(record_data[[col]]), 'labelled')
    }
  }
  else {
    # Remove column labels
    # https://stackoverflow.com/questions/2394902/remove-variable-labels-attached-with-foreign-hmisc-spss-import-functions
    for (col in seq_along(record_data)) {
      class(record_data[[col]]) <- setdiff(class(record_data[[col]]), 'labelled')
      attr(record_data[[col]],"label") <- NULL
    }
  }
  
  if (!is.null(report_data[['redcap_event_name']])) {
    # Move check to beginning of function?
    if (is.null(event_data)) 
      stop("bundle$event_data must be provided to label redcap_event_name")
    
    # Check for previous labeling of events. Using any() is less strict than all() and will introduce NAs for event names not in metadata
    if (any(record_data$redcap_event_name %in% event_data$event_name))
        levels = event_data$event_name
    else levels = event_data$unique_event_name # Default assumption. Could be better to explicitly check
    
    if (event_labels)
      # Convert to labeled values
      record_data$redcap_event_name = factor(record_data$redcap_event_name, 
                                             levels = levels, 
                                             labels = event_data$event_name)
    else
      # Undo if labeled
      record_data$redcap_event_name = factor(record_data$redcap_event_name,
                                             levels = levels,
                                             labels = event_data$unique_event_name)
  }
  
  if (strip) record_data = rc_strip(record_data)
  
  # Append formatting details to df attributes
  format_record = c(factor_labels,col_labels,dates)
  names(format_record) = c('factor_labels','col_labels','dates')
  attr(record_data, 'redcap_formatting') = format_record
  
  return(record_data)
}
