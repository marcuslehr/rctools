#' @name rc_combine_checkboxes
#'
#' @title Combine Wide-Format Checkbox Fields
#' @description Combines REDCap checkbox fields from wide format (multiple binary 
#'   columns with ___[code] suffixes) into a single column with comma-separated 
#'   values representing the selected options.
#' @details This function takes REDCap data in wide format where checkbox fields 
#'   are represented as multiple binary columns (e.g., cb_field___1, cb_field___2) 
#'   and combines them into a single column containing comma-separated values of 
#'   the selected option codes or labels. The wide-format columns are then removed 
#'   from the data frame.
#'   
#' @param record_data Dataframe. Record data export from REDCap
#' @param data_dict Dataframe. REDCap project data data_dictionary. By default, 
#'   this will be fetched from the REDCap bundle option, as created by \code{rc_bundle}.
#'   Otherwise, a data.frame containing the project data dictionary must be supplied.
#' @param fields Character vector. Names of checkbox fields to combine. If NULL 
#'   (default), all checkbox fields will be identified and combined. Field names 
#'   should be the root field name (without ___[code] suffix).
#' @param factor_labels Logical. Determines whether the combined values are 
#'   represented as numeric codes (FALSE) or option labels (TRUE). Default is TRUE.
#'   
#' @return Dataframe with combined checkbox columns and wide-format columns removed.
#'   
#' @author Marcus Lehr
#'
#' @export

rc_combine_checkboxes <- function(record_data, 
                                  fields = NULL,
                                  data_dict = getOption("redcap_bundle")$data_dict,
                                  factor_labels = TRUE)
{
  
  validate_args(required = c('record_data'),
                record_data = record_data, 
                data_dict = data_dict,
                fields = fields,
                factor_labels = factor_labels)
  
  # Identify checkbox fields if not specified
  if (is.null(fields)) {
    if (!is.null(data_dict)) {
      # Get checkbox fields from data_dict
      fields <- data_dict$field_name[data_dict$field_type == "checkbox"]
    } else {
      # Extract checkbox fields by looking for ___\d+ pattern in column names
      checkbox_cols <- names(record_data)[grepl("___\\d+$", names(record_data))]
      fields <- unique(gsub("___\\d+$", "", checkbox_cols))
    }
  }
  
  # If no checkbox fields found, return data unchanged
  if (length(fields) == 0) {
    return(record_data)
  }
  
  # Process each checkbox field
  for (field in fields) {
    # Find all columns for this checkbox field
    field_cols <- grep(paste0("^", field, "___\\d+$"), names(record_data))
    
    if (length(field_cols) > 0) {
      # Get the coding information if data_dict is available
      if (!is.null(data_dict) && field %in% data_dict$field_name) {
        coding <- data_dict$select_choices_or_calculations[data_dict$field_name == field]
        parsed_coding <- parse_field_choices(coding)
      } else {
        parsed_coding <- NULL
      }
      
      # Extract the wide-format data for this field
      wide_data <- record_data[, field_cols, drop = FALSE]
      
      # Combine the checkbox columns
      combined <- apply(wide_data, 1, function(row) {
                    # Get indices of checked boxes (value = 1)
                    checked_indices <- which(as.numeric(row) == 1)
                    
                    if (length(checked_indices) == 0) {
                      return(NA_character_)
                    }
                    
                    # Extract the codes from column names (the part after ___)
                    checked_codes <- as.numeric(gsub(paste0("^", field, "___"), "", 
                                                     names(wide_data)[checked_indices]))
                    
                    # Sort codes for consistent output
                    checked_codes <- sort(checked_codes)
                    
                    if (factor_labels && !is.null(parsed_coding)) {
                      # Convert codes to labels
                      labels <- sapply(checked_codes, function(code) {
                        idx <- which(parsed_coding$numbers == as.character(code))
                        if (length(idx) > 0) parsed_coding$labels[idx] else as.character(code)
                      })
                      paste(labels, collapse = ", ")
                    } else {
                      # Use numeric codes
                      paste(checked_codes, collapse = ",")
                    }
                  })
      
      # Add the combined column
      record_data[[field]] <- combined
      
      # Remove the wide-format columns
      record_data <- record_data[, -field_cols, drop = FALSE]
    }
  }
  
  return(record_data)
}

## TODO
  # Doesn't work on labeled data. Should handle that first.
  # Doesn't maintain col position, moves to end.
  # Can probably be simplified to rc_format() -> unite()
