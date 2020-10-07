#' @name numeric_only
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
#' \code{rc_bundle}.
#' @param sex_var String. Name of variable indicating the sex of subjects. If
#' included, it will be used as one of the melting factors.
#' @param fields Character. A vector of field/variable names to be analyzed
#' may be passed manually. 
#' @param long_format Logical. Determines whether the returned dataframe will
#' be in long or wide format. Default is \code{TRUE}.
#' @param drop_message Logical. Determine if a message is shown to the user about
#' dropping non-numerical data
#' 
#' @importFrom magrittr '%>%'
#' 
#' @author Marcus Lehr

numeric_only <- function(record_data, 
                         data_dict = getOption("redcap_bundle")$data_dict, 
                         sex_var = NA, fields = NULL, 
                         long_format = TRUE, drop_message = TRUE) {
  
  # Notify user
  if (drop_message == T)
    message("This function is designed to work with numeric data only. 
            All non-numeric fields will be dropped.")
  
  # Get ID column names
  id_field = getID(record_data, data_dict)
  rc_fields = c('redcap_event_name','redcap_repeat_instrument','redcap_repeat_instance')
  
  # Remove unwanted columns from record data
  record_data = dplyr::select(record_data,
                              -dplyr::contains('_complete'), # Form complete fields
                              -dplyr::contains('___')) # Checkbox data
  
  
  # Generate fields if not provided
  if (is.null(fields)) {
    # Check format status of data and format if necessary
    if (!any(unlist(sapply(record_data, class))=='labelled')) {
      # Use data_dict if available
      if (!is.null(data_dict))
        record_data = rc_format(record_data, data_dict)
      else {
        warning("Please provide data_dict or record_data formatted with rc_format() to avoid
                inappropriate variables being passed.")
        # Format columns without data_dict
        record_data = suppressMessages(readr::type_convert(record_data))
      }
    }
    # Select only fields which (may) contain quantitative data
    fields = sapply(record_data, 
                         function(x) any(grepl('integer|numeric|character',
                                               class(x)))) %>% .[.==T] %>% names()

    # Remove fields with < 3 levels (likely yes/no fields or other factors)
    factor_names = names(record_data)[sapply(record_data, function(x) 
                        length(levels(as.factor(x)))) < 3] %>% as.vector()
    fields = setdiff(fields, factor_names)

    ## I don't think this offers any additional benefit and it requires data_dict
    # text_fields = data_dict$field_name[grepl('text|calc',data_dict$field_type)] 
    # fields = intersect(num_fields, text_fields) %>% c(id_field,.) %>%  unique()
  }
  
  # Create melting variables and add to fields list
  rc_factors = c(id_field, sex_var, rc_fields) %>% stats::na.omit()
  rc_factors = rc_factors[rc_factors %in% names(record_data)]
  fields = c(rc_factors, fields) %>% unique()
  
  # Subset data
  record_data = record_data[fields]
  
  # Fill sex variable before melt, if applicable
  if (!is.na(sex_var))
    if(any(is.na(record_data[sex_var]))) # This condition is an attempt to avoid an error when the
                                        # var has already been filled. An error will still be thrown
																				# for an incomplete fill
    record_data = rc_fill(record_data, sex_var)
  
  # Convert to long format. Dates get destroyed by melt()
  record_data = suppressWarnings(
    reshape2::melt(record_data, id.vars=rc_factors, na.rm = T) %>% 
       droplevels()
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
    
  
  if (!long_format) {
    cast_formula = paste(paste(rc_factors, collapse = ' + '),"~ variable")
    record_data = reshape2::dcast(record_data, cast_formula)
  }
  
  return(record_data)
}