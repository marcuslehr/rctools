#' @name rc_outliers
#'
#' @title Identify outliers in REDCap records data
#' @description  Identifies outliers for each variable, defined as being further
#' from the mean than the threshold number of standard deviations. This follows the
#' convention used within REDCap, however defaults to a standard deviation threshold
#' of 3 rather than 2. Data is returned in long format with a column specifying
#' outlier status.
#' @details Unless a vector of variables/field names is passed to the 
#' \code{fields} argument, the fields to be analyzed will be guessed based on
#' column type. All non-numeric data will be removed before analysis.
#' If mixed numeric/non-numeric data (e.g. "160 cm") are passed, the first numerical
#' instance will be extracted from the data. If a sex variable is provided, then
#' variables will be grouped by sex for outlier analysis.
#' 
#' @param record_data Dataframe. Records data export from REDCap. For the
#' purposes of this function, only quantitative data will be kept. 
#' @param sex_var String. Name of variable indicating the sex of subjects. If
#' included, variables will be grouped by sex when determining outliers.
#' @param sd_threshold Integer. Threshold value for the number of standard
#' deviations from the mean a value can be before being flagged as an outlier.
#' @param fields Character. A vector of field/variable names to be analyzed
#' may be passed manually. 
#' @param filtered Logical. When \code{TRUE}, only outlier values will be returned.
#' Default is \code{FALSE}.
#'
#' @param data_dict Dataframe. A REDCap project data dictionary. By default, 
#' $data_dict is expected in the REDCap bundle option, as created by 
#' \code{rc_bundle}.
#' @param mappings Dataframe. A REDCap table containing form/event mappings.
#' @param id_field Character. Field name corresponding to the 'record_id' field.
#' 
#' 
#' @author Marcus Lehr
#' 
#' @export


# A detection function argument should be added.
rc_outliers <- function(record_data, sex_var = NA, sd_threshold = 3,
                        fields = NULL, filtered = FALSE,
                        data_dict = getOption("redcap_bundle")$data_dict,
                        mappings = getOption("redcap_bundle")$mappings,
                        id_field = getOption("redcap_bundle")$id_field) {
  
  validate_args(required = c('record_data'),
								record_data = record_data, 
								sex_var = sex_var,
                sd_threshold = sd_threshold, 
								data_dict = data_dict,
                fields = fields, 
								filtered = filtered
								)
  
  # Get ID column name
  id_field = getID(record_data, data_dict, id_field)
  rc_fields = intersect(c('redcap_event_name','redcap_repeat_instrument','redcap_repeat_instance'),
                        names(record_data))
	
  # Collect pooling data for form names
  pooled_vars = attributes(record_data)$pooled_vars
  # Attempt to grab from options? Supply argument to user?
  
	# Attempt to isolate numeric fields. This converts the data to long format.
  record_data = numeric_only(record_data, data_dict, sex_var, fields)
  
  # Add form names
  record_data = add_form_names(record_data, pooled_vars, data_dict, mappings, id_field)
  
  # Identify outliers for each variable
  groups = c(sex_var, 'variable') %>% stats::na.omit()
  record_data = record_data %>% dplyr::group_by_at(groups) %>% 
                  dplyr::mutate(outlier = abs(scale(value))>sd_threshold) %>%
                  dplyr::ungroup() %>% dplyr::arrange_at(c(id_field,'redcap_event_name','variable'))
  
  # NAs result from single values and (I think) standard deviations of 0. Replace them with FALSE
  record_data$outlier[is.na(record_data$outlier)] = FALSE
  record_data$outlier = as.logical(record_data$outlier) # Preserve formatting
  
  # Filter data
  if (filtered) record_data = record_data %>% 
                                      dplyr::filter(outlier == T) %>% 
                                      dplyr::select(-outlier)
  
  return(record_data)
}