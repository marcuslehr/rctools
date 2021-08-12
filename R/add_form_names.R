#' @name add_form_names
#'
#' @title Add form names to long format RC data
#' @description  Adds a form name column, taking pooled data into account.
#' This is an internal function only
#' 
#' @param long_data Dataframe. A long format data frame, as created by 
#' \code{rc_missing}. At minimum there should be a record_id column, a
#' redcap_event_name column, and a column containing variable names.
#' Wide-to-long conversion functions include reshape2::melt() 
#' (used in rctools), tidyr::gather(), and tidyr::pivot_longer().
#' @param pooled_vars Dataframe. Dataframe attribute appended to record_data by
#' \code{pooled_vars}.
#' @param data_dict Dataframe. REDCap project data data_dictionary. By default, 
#' this will be fetched from the REDCap bundle option, as created by \code{rc_bundle}.
#' Otherwise, a data.frame containing the project data dictionary must be supplied.
#' @param mappings Dataframe. Redcap metadata that maps forms to events.
#' @param id_field Character. Field name corresponding to the 'record_id' field.
#' 
#' @author Marcus Lehr


add_form_names <- function(long_data, pooled_vars = NULL,
                           data_dict = getOption("redcap_bundle")$data_dict,
                           mappings = getOption("redcap_bundle")$mappings,
                           id_field = getOption("redcap_bundle")$id_field) {
  
  # Add form names for non-pooled vars
  if (!is.null(data_dict)) {
    long_data = suppressWarnings(
      data_dict[,1:2] %>% dplyr::rename(variable = field_name) %>% 
        dplyr::left_join(long_data, ., by = 'variable')
    )
    
    if (!is.null(pooled_vars)) {
      # Add form names from data_dict
      pooled_vars = suppressWarnings(
                      pooled_vars %>% dplyr::rename(variable = pooled_vars, field_name = rc_vars) %>% 
                        dplyr::left_join(data_dict[,1:2], by = 'field_name') %>% 
                        dplyr::rename(redcap_repeat_instance = field_name)
                    )
      # Merge with mappings for event names
      mappings = mappings %>% dplyr::rename(form_name = form, redcap_event_name = unique_event_name) %>% 
                  dplyr::left_join(pooled_vars, by = 'form_name')
      
      # Join form names into long_data
      long_data = suppressWarnings(
                    long_data %>% 
                      # Join form names for repeat vars
                      dplyr::left_join(pooled_vars, by = c('variable','redcap_repeat_instance')) %>% 
                      dplyr::mutate(form_name = dplyr::coalesce(form_name.x, form_name.y)) %>% 
                      dplyr::select(-form_name.x, -form_name.y) %>% 
                      # Fill in form names for non-repeat pooled vars
                      dplyr::left_join(dplyr::select(mappings, redcap_event_name,variable,form_name), 
                                       by = c('redcap_event_name','variable')) %>% 
                      dplyr::mutate(form_name = dplyr::coalesce(form_name.x, form_name.y)) %>% 
                      dplyr::select(-form_name.x, -form_name.y) %>% dplyr::distinct()
                  )
    }
    
    # Reorder data
    id_field = getID(long_data, data_dict, id_field)
    rc_fields = c('redcap_event_name','redcap_repeat_instrument','redcap_repeat_instance')
    rc_factors = intersect(c(id_field, rc_fields), names(long_data))
    long_data = long_data %>% select(all_of(rc_factors), form_name, dplyr::everything())
  }
  else message("Form names cannot be added unless data_dict is supplied.")
  
  return(long_data)
}
