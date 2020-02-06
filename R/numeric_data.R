#' @name numeric_data
#'
#' @title Filters REDCap records data for numeric data only
#' @description Unless a vector of variables/field names is passed to the 
#' \code{fields} argument, the fields to be analyzed will be guessed based on
#' column type. Furthermore, all non-numeric data will be removed before analysis.
#' If mixed numeric/non-numeric data (e.g. "160 cm") are passed, the first numerical
#' instance will be extracted from the data. A long format dataframe is returned.
#' 
#' This is an internal function only.
#' 
#' @param record_data Dataframe. Records data export from REDCap. For the
#' purposes of this function, only quantitative data will be kept. 
#' @param data_dict Dataframe. REDCap project data data_dictionary. By default, 
#' $data_dict is expected in the REDCap bundle option, as created by 
#' \code{rc_setup}.
#' @param sex_var String. Name of variable indicating the sex of subjects. If
#' included, it will be used as one of the melting factors.
#' @param fields Character. A vector of field/variable names to be analyzed
#' may be passed manually. 
#' 
#' @importFrom magrittr '%>%'
#' 
#' @author Marcus Lehr

numeric_data <- function(record_data, data_dict = NULL, 
                         sex_var = NA, fields = NULL) {
  
  # Grab record ID field name
  id_field = getID(record_data, data_dict)
  
  # Remove unwanted columns from record data
  record_data = dplyr::select(record_data, 
                              -dplyr::contains('redcap_repeat'), # Repeat instruments
                              -dplyr::contains('redcap_survey'), # Survey data
                              -dplyr::contains('_complete'), # Form complete fields
                              -dplyr::contains('___')) # Checkbox data
  
  
  
  if (!is.null(fields)) field_names = fields
  
  else {
    # Check format status of data and format if necessary
    if (!any(unlist(sapply(record_data, class))=='labelled')) {
      # Use data_dict if available
      if (!is.null(data_dict))
        record_data = rc_format(record_data, data_dict)
      else {
        warning("Please provide data_dict or record_data formatted with rc_format() to avoid
                inappropriate variables being passed.")
        # Format columns without data_dict
        record_data = readr::type_convert(record_data)
      }
    }
    # Select only fields which (may) contain quantitative data
    field_names = sapply(record_data, 
                         function(x) any(grepl('integer|numeric|character',
                                               class(x)))) %>% .[.==T] %>% names()
    
    # # Add id_field and remove redcap fields
    # field_names = c(id_field, sex_var, field_names[!grepl('redcap',field_names)]) %>% 
    #                   unique() %>% na.omit()
    
    ## I don't think this offers any additional benefit and it requires data_dict
    # text_fields = data_dict$field_name[grepl('text|calc',data_dict$field_type)] 
    # field_names = intersect(num_fields, text_fields) %>% c(id_field,.) %>%  unique()
  }
  
  # Add melting variables to fields list
  meltVars = c(id_field, sex_var, 'redcap_event_name') %>% na.omit()
  field_names = c(meltVars, field_names) %>% unique()
  
  # Subset data
  record_data = record_data[field_names]
  
  # Fill sex variable, if applicable
  if (!is.na(sex_var)) 
    record_data = rc_fill(record_data, sex_var)
  
  # Convert to long format. Dates get destroyed by melt()
  record_data = suppressWarnings(
    reshape2::melt(record_data, id.vars=meltVars, na.rm = T) %>% 
       droplevels() #dplyr::as_tibble() %>% 
  )
  
  # Insure against non-validated fields by making sure only digits are passed
    record_data = record_data[stringr::str_detect(record_data$value,"\\d+"),]
    # Give user a warning if numbers will be stripped
    if (max(unlist(lapply(
          stringr::str_extract_all(record_data$value, "\\d+\\.?\\d*"),length)
        ))>1) 
      warning("Some fields contain multiple numbers. Only the first will be used.")
    # Extract only first number from values
    record_data$value = stringr::str_extract(record_data$value, "\\d+\\.?\\d*") %>% as.numeric()
  
  return(record_data)
}