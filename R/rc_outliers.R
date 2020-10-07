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
#' @param filtered Logical. Select whether non-outlier values should be returned.
#' Default is \code{FLASE}.
#'
#' @param data_dict Dataframe. REDCap project data dictionary. By default, 
#' $data_dict is expected in the REDCap bundle option, as created by 
#' \code{rc_bundle}.
#' @param id_field Character. Field name corresponding to the 'record_id' field.
#' 
#' @importFrom magrittr '%>%'
#' 
#' @author Marcus Lehr
#' 
#' @export

rc_outliers <- function(record_data, sex_var = NA, sd_threshold = 3,
                        fields = NULL, filtered = FALSE,
                        data_dict = getOption("redcap_bundle")$data_dict,
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
  id_field = getID(record_data, data_dict)
  rc_fields = intersect(c('redcap_event_name','redcap_repeat_instrument','redcap_repeat_instance'),
                        names(record_data))
	
  # Collect pooling data for form names
  pooled_vars = attributes(record_data)$pooled_vars
  # Attempt to grab from options? Supply argument to user?
  
	# Retrieve numeric data from records
  record_data = numeric_only(record_data, data_dict, sex_var, fields)
  
  # Add form names
  if (!is.null(data_dict)) {
    record_data = suppressWarnings(
                    data_dict[,1:2] %>% dplyr::rename(variable = field_name) %>% 
                      dplyr::right_join(record_data, by = 'variable')
                  )
    
    if (!is.null(pooled_vars)) {
      # Add form names from data_dict
      pooled_vars = suppressWarnings(
                      pooled_vars %>% dplyr::rename(variable = pooled_vars, field_name = rc_vars) %>% 
                        dplyr::left_join(data_dict[,1:2], by = 'field_name') %>% 
                        dplyr::rename(redcap_repeat_instance = field_name)
                    )
      # Join form names into record_data
      record_data = suppressWarnings(
                      record_data %>% dplyr::left_join(pooled_vars, by = c('variable','redcap_repeat_instance')) %>% 
                        dplyr::mutate(form_name = dplyr::coalesce(form_name.x, form_name.y)) %>% 
                        dplyr::select(-form_name.x, -form_name.y)
                    )
    }
    
    # Reorder data
    record_data = record_data[stats::na.omit(c(id_field, sex_var, rc_fields, 'form_name', 'variable', 'value'))]
  } else message("Form names cannot be added unless data_dict is supplied.")
  
  # Identify outliers for each variable
  groups = c(sex_var, 'variable') %>% stats::na.omit()
  record_data = record_data %>% dplyr::group_by_at(groups) %>% 
                  dplyr::mutate(outlier = abs(scale(value))>sd_threshold) %>%
                  dplyr::ungroup() %>% dplyr::arrange_at(c(id_field,'redcap_event_name','variable'))
  
  # NAs result from single values and (I think) standard deviations of 0. Replace them with FALSE
  record_data$outlier[is.na(record_data$outlier)] = FALSE
  record_data$outlier = as.logical(record_data$outlier) # Preserve fomatting
  
  # Filter data
  if (filtered) record_data = record_data %>% 
                                      dplyr::filter(outlier == T) %>% 
                                      dplyr::select(-outlier)
  
  return(record_data)
}

# Recycle bin -------------------------------------------------------------

## This has now been exported as an independent function
  # # Make plots
  # if (plot) plot_outliers(outlier_data = record_data, 
  #                         sex_var = sex_var, 
  #                         id_field = id_field)
  
# # Fill in first level pooled vars without repeat instruments
      # if (!is.null(mappings)) {
      #   pooled_vars = suppressWarnings(
      #                   mappings[,2:3] %>% dplyr::rename(form_name = form, redcap_event_name = unique_event_name) %>% 
      #                     dplyr::right_join(pooled_vars, by = 'form_name')
      #                 )
      #   
      #   # first_pool_form = pooled_vars %>% dplyr::select(variable, redcap_event_name, form_name) %>% 
      #   #                                    dplyr::group_by(variable, redcap_event_name) %>% dplyr::summarise_all(~dplyr::first(.))
      #   # 
      #   # record_data = suppressWarnings(record_data %>% 
      #   #                 dplyr::filter(variable %in% unique(pooled_vars$variable) & 
      #   #                               is.na(redcap_repeat_instrument)) %>%
      #   #                 dplyr::select(-form_name) %>% 
      #   #                 dplyr::left_join(first_pool_form, by = c('variable','redcap_event_name')) %>% 
      #   #                 dplyr::right_join(record_data, by = setdiff(names(record_data),'form_name')) %>% 
      #   #                 dplyr::mutate(form_name = dplyr::coalesce(form_name.x, form_name.y)) %>% 
      #   #                 dplyr::select(-form_name.x, -form_name.y)
      #   #               )
      #                   
      # } else if (any(is.na(dplyr::filter(record_data, variable %in% unique(pooled_vars$variable))$redcap_repeat_instrument)))
      #     warning("Some form names could not be added because 'mappings' is not available.")