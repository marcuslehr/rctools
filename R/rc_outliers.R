#' @name rc_outliers
#'
#' @title Identify outliers in REDCap records data
#' @description  Identifies outliers for each variable, defined as being further
#' from the mean than the threshold number of standard deviations. Outliers are
#' then returned as a data.frame.
#' @details Unless a vector of variables/field names is passed to the 
#' \code{fields} argument, the fields to be analyzed will be guessed based on
#' column type. Furthermore, all non-numeric data will be removed before analysis.
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
#' @param data_dict Dataframe. REDCap project data data_dictionary. By default, 
#' $data_dict is expected in the REDCap bundle option, as created by 
#' \code{rc_setup}. 
#' @param fields Character. A vector of field/variable names to be analyzed
#' may be passed manually. 
#' @param plot Logical. Select whether plots of the data should be produced. 
#' @param unfiltered Logical. Select whether an unfiltered dataframe should be
#' returned, or only outliers (default).
#' 
#' @importFrom magrittr '%>%'
#' 
#' @author Marcus Lehr
#' 
#' @export

rc_outliers <- function(record_data, sex_var = NA, sd_threshold = 2.5,
                        data_dict = getOption("redcap_bundle")$data_dict,
                        fields = NULL, plot = FALSE, unfiltered = FALSE) {
  
  validate_args(required = c('record_data'),
								record_data = record_data, sex_var = sex_var,
                sd_threshold = sd_threshold, data_dict = data_dict,
                fields = fields, plot = plot, unfiltered = unfiltered)
  
  # Get ID column name
  id_field = getID(record_data, data_dict)
  rc_fields = c('redcap_event_name','redcap_repeat_instrument','redcap_repeat_instance')
	
	# Retrieve numeric data from records
  record_data = numeric_only(record_data, data_dict, 
                             sex_var, fields)
  
  # Add form names and reorder columns
  if (!is.null(data_dict)) {
    instrVarMap = data_dict[,1:2] %>% dplyr::rename(variable = field_name)
    record_data = suppressWarnings(dplyr::left_join(record_data, instrVarMap, by = 'variable'))
    record_data = record_data[na.omit(c(id_field, sex_var, rc_fields, 'form_name', 'variable', 'value'))]
  }
  else message("Form names cannot be added unless data_dict is supplied.")
  
  # Identify outliers for each variable
  group_by = c(sex_var, 'variable') %>% na.omit() %>% paste(., collapse = ',')
  record_data = record_data %>% dplyr::group_by_(group_by) %>% 
                  dplyr::mutate(outlier = abs(scale(value))>sd_threshold) %>%
                  dplyr::ungroup() %>% dplyr::arrange_(id_field)
  
  # NAs result from single values and (I think) standard deviations of 0. Replace them with FALSE
  record_data$outlier[is.na(record_data$outlier)] = FALSE
  
  
  # Make plots
  if (plot) plot_outliers(record_data, id_field, sex_var)
  
  # Filter data
  if (unfiltered == F) record_data = record_data %>% 
                                      dplyr::filter(outlier == T) %>% 
                                      dplyr::select(-outlier)
  
  return(record_data)
}
