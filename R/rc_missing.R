#' @name rc_missing
#'
#' @title Find missing record data
#' @description  Finds data points which are ostensibly missing from Redcap records data.
#' 
#' For each event/variable combination, if a value exists for at least one
#' record then it is expected to exist for all records. Any event or variable which is empty within
#' the entire dataframe will inherently be dropped. If an event has no values for a given record, 
#' none of the variables will be flagged unless there is data for that record in the following event. 
#' Additionally, if the subject is marked as having completed the study (i.e. \code{completion_field 
#' == 'Yes'|1}), then empty events for that subject will not be discarded.
#'
#' The logic of this function does not extend to repeat instruments or checkbox, calculated,
#' or hidden fields. If present in the record data, they will be dropped. Additionally, this function
#' does not yet support branching logic. It will be implemented in a future version.
#'
#' @param record_data Record data export from REDCap
#' @param completion_field Character. The REDCap variable which indicates whether or not a subject
#' has completed the study. This should be indicated by a 'Yes' or a '1' (i.e. a Yes/No
#' field in REDCap). If not provided, some missing data may be lost.
#' @param repeats Character. A vector of repeat instrument names which should be kept. This should
#' ONLY be used for repeat instruments which are expected to have a consistent number of occurrences
#' per each event/ID combination. All repeat instruments not supplied here will be removed to avoid
#' excessive false positives.
#' 
#' @param data_dict Dataframe. REDCap project data data_dictionary. By default, 
#' $data_dict is expected in a REDCap bundle object, as created by \code{rc_bundle}. Otherwise, an 
#' equivalent data.frame containing the project data dictionary must be supplied.
#' @param events Character. Vector of REDCap event names. If supplying manually, chronological
#' ordering of events must be ensured. 
#' @param form_perm Dataframe. User access permissions for REDCap forms. By default, $form_perm
#' is expected in the REDCap bundle, as created by \code{rc_bundle}. Otherwise, an equivalent 
#' data.frame containing form permission data must be supplied.
#' @param mappings Dataframe. Redcap metadata that maps forms to events.
#' @param id_field Character. The name of the record_id field for your REDCap project.
#'
#'
#' @author Marcus Lehr
#' 
#' @export

rc_missing <- function(record_data, 
                       completion_field = NULL, repeats = NULL,
                       data_dict = getOption("redcap_bundle")$data_dict,
                       events = getOption("redcap_bundle")$event_data$unique_event_name,
                       form_perm = getOption("redcap_bundle")$form_perm,
                       mappings = getOption("redcap_bundle")$mappings,
                       id_field = getOption("redcap_bundle")$id_field
                       ) {


# Checks ------------------------------------------------------------------
  
  validate_args(required = c('record_data','data_dict','events'),
                record_data = record_data,
                completion_field = completion_field,
                repeats = repeats,
                data_dict = data_dict,
                events = events,
                form_perm = form_perm
                )
  
# Prep data -------------------------------------------------------------------  
  
  ## Get record_id field name ---
  id_field = getID(record_data, data_dict, id_field)
  
  ## Filter ---
  
  # Check for pooling and populate repeat list if empty
  pooled_vars = attributes(record_data)$pooled_vars
  if (!is.null(pooled_vars) & is.null(repeats)) {
    repeats = levels(pooled_vars$pooled_vars)
    message("Pooled variables being used as repeats") # Could use clearer phrasing here
  }
  
  # Remove repeat instrument data unless specified in repeats
  if (any(!is.na(record_data[['redcap_repeat_instrument']]))) { # Check if col exists and contains data
    if (any(repeats %in% record_data$redcap_repeat_instrument)) # Check if values are contained in 'repeats'
      record_data = dplyr::filter(record_data, redcap_repeat_instrument %in% repeats |
                                  is.na(redcap_repeat_instrument))
    else { # Data exists and is not specified in 'repeats'
      message("All repeat instruments will be dropped. If you would like to keep
        any repeat instruments, please supply the `repeats` argument. This should
        only be done for instruments where a consistent number of repeat instances are expected.")
    
      record_data = dplyr::filter(record_data, is.na(redcap_repeat_instrument)) #%>% # Repeat rows
                      # dplyr::select(-dplyr::contains('redcap_repeat')) # Repeat ID columns
    }
   } # No data exists in repeat cols, go ahead and remove them if present
  # else if (any(grepl('redcap_repeat', names(record_data)))) 
  #   record_data = record_data[!grepl('redcap_repeat', names(record_data))]
  
  # Remove other unwanted fields
  fields_calc = intersect(data_dict$field_name[data_dict$field_type == 'calc'], names(record_data))
  fields_hidden = intersect(data_dict$field_name[data_dict$field_annotation == '@HIDDEN'], names(record_data))
  
  if (length(fields_calc)>0 |
      length(fields_hidden)>0 |
      any(grepl('___',names(record_data))) # Look for checkbox data
      ) {
    message("The logic of this function does not translate to checkbox 
            or calculated fields. All such data will be dropped.")

    record_data = dplyr::select(record_data, 
                                -dplyr::all_of(fields_calc),
                                -dplyr::all_of(fields_hidden), 
											          -dplyr::contains('___')) 
  }
  
  ## Completion data ---
  
  if (any(names(record_data)==completion_field)) {
    # Grab completion data then remove from data
    completion_data = dplyr::select(record_data, !!id_field, !!completion_field) %>% 
                        stats::na.omit() %>%
                        dplyr::distinct()
    record_data = dplyr::select(record_data, -!!completion_field)
    # completion_data$record_id = as.character(completion_data$record_id)
  }
  else message("Completion field not found. Some missing data may not be captured.")
  
	# Remove _complete fields
  if (any(grepl('_complete',names(record_data))))
    record_data = dplyr::select(record_data, -dplyr::contains('_complete'))


# Collect missing data ----------------------------------------------------  
  
  rc_fields = c('redcap_event_name','redcap_repeat_instrument','redcap_repeat_instance')
  rc_factors = intersect(c(id_field, rc_fields), names(record_data))
  
  # This is a potential fix for being fed formatted data, but I'm not sure if it's a good idea
  # bc rc_format requires event_data, which isn't fed into this function. Also could return data
  # in a different format than submitted. Will just let it error out for now. 
  # # Attempt to coerce event names to raw
  # record_data = rc_format(record_data, data_dict, event_labels = F)
  
  # Convert data to long format. Globally empty events and IDs will be lost
  record_data = suppressWarnings(
              reshape2::melt(record_data, id.vars = rc_factors, na.rm = T) %>% dplyr::as_tibble() %>%
                      dplyr::filter(value != '') %>% droplevels()
                          )
  # # Ensure ID field is always character for joins
  # record_data[[id_field]] = as.character(record_data[[id_field]]) 

  # Collecting IDs outside the loop allows capturing of (non-globally) empty events
  # IDs = as.character(unique(record_data[[id_field]]))
  IDs = unique(record_data[[id_field]])

  # Create a frame of all event/variable combos for each ID
  event_var_combos = dplyr::select(record_data, dplyr::contains('redcap'), variable) %>% dplyr::distinct() %>% 
                      dplyr::arrange_at(c('redcap_event_name', 'variable'))
  expected_data = data.frame()
  for (i in IDs) {
    x = event_var_combos
    x[id_field] = i
    x = dplyr::select(x, !!id_field, dplyr::everything())
    expected_data = rbind(expected_data, x)
  }
  
  # Fill in existing data for logic
  cast_formula = paste(paste(rc_factors,collapse = ' + '), "~ variable")
  data_wide_full = dplyr::full_join(expected_data, record_data, by = c(rc_factors, 'variable')) %>% 
                          reshape2::dcast(., cast_formula)
  
  # Filter events for those remaining in the data
  events = events[events %in% record_data$redcap_event_name]
  if (!length(events)) stop("Events in data do not match supplied events")
  
  # Ensure proper event ordering for logic
  data_wide_full$redcap_event_name = factor(data_wide_full$redcap_event_name, levels = events)
  data_wide_full = dplyr::arrange_at(data_wide_full, c(id_field,'redcap_event_name'))
  
  # Capture all expected data points not present in record_data
  missing_data_all = dplyr::anti_join(expected_data, dplyr::select(record_data, -value), by = c(rc_factors, 'variable'))
  missing_data_all$variable = as.character(missing_data_all$variable)

# Filter data -------------------------------------------------------------

  # Continue if missing data are found
  if (nrow(missing_data_all) > 0) {
  
    # Filter variables from empty events unless there is data in a following event or the participant as completed the study
    if (!is.null(completion_field)) {
      visit_data_summary = data_wide_full %>%
                dplyr::mutate(row_sums = rowSums(!is.na(dplyr::select(.,-dplyr::all_of(rc_factors))))) %>% # Count variables with data. Incompatible with checkbox fields
                dplyr::group_by_at(rc_factors[1:2]) %>% # Removing repeats to look at entire event. Including would break data_following
                dplyr::summarise(var_count = sum(row_sums)) %>% # I think sum() is only relevant when repeats are present
                dplyr::mutate(data_following = cumsum(var_count) != sum(var_count)) %>% # If total sum per ID is == cumsum, then all following visits must be empty (if any)
                dplyr::left_join(., completion_data, by = id_field) # Add completion field for next step
                
      
      missing_data = suppressWarnings(
                      dplyr::left_join(missing_data_all, visit_data_summary, by = rc_factors[1:2]) %>% # Logic has no repeats
                        dplyr::filter(var_count > 0 | data_following | !!dplyr::sym(completion_field) == 'Yes|1') %>% 
                        dplyr::select(dplyr::all_of(rc_factors), variable)
                      )
    }
    # Perform above filtering without completion_field if not provided
    else {
      visit_data_summary = data_wide_full %>%
                dplyr::mutate(row_sums = rowSums(!is.na(dplyr::select(.,-dplyr::all_of(rc_factors))))) %>% 
                dplyr::group_by_at(rc_factors[1:2]) %>% 
                dplyr::summarise(var_count = sum(row_sums)) %>% 
                dplyr::mutate(data_following = !sum(var_count) == cumsum(var_count))
      
      missing_data = suppressWarnings(
                      dplyr::left_join(missing_data_all, visit_data_summary, by = rc_factors[1:2]) %>% 
                        dplyr::filter(var_count > 0 | data_following) %>% 
                        dplyr::select(dplyr::all_of(rc_factors), variable)
                      )
    }
    
    # Add form names and remove vars from hidden forms
    if (nrow(missing_data) > 0) {
      
      # Add form names for ease of locating in Redcap
      missing_data = add_form_names(missing_data, pooled_vars, data_dict, mappings, id_field)
      
      # Remove data from forms where no users have access
      if (!is.null(form_perm)) {
        
        forms_hidden = setdiff(unique(subset(form_perm, view == 'No access')$form_name),
                               unique(subset(form_perm, view != 'No access')$form_name))
        
        if (any(forms_hidden %in% missing_data$form_name)) {
          message("The following forms will be removed as no users have access to them: ", 
                  paste(intersect(forms_hidden,missing_data$form_name), collapse = ', '))
          missing_data = missing_data[!missing_data$form_name %in% forms_hidden,]
        }
      }
    }
    
    # Ignore fields dependent on other fields. Event dependencies are handled via event_var_combos
    # Implementing branching logic eval would be quite the endeavor. Preferably RC will implement data quality API methods. Their missing data method takes branching logic into account
      field_dependent_fields = data_dict$field_name[which(stringr::str_detect(data_dict$branching_logic, paste(data_dict$field_name,collapse='|')))]
      if (any(field_dependent_fields %in% missing_data$variable)) {
        message("Fields with branching logic dependent on other fields will be removed as their presence in the data are not consistent.")
        missing_data = missing_data %>% dplyr::filter(!variable %in% field_dependent_fields)
      } 
    
    # Notify user if no data remains
    if (nrow(missing_data) == 0) message("No missing data were found.")
    
    # End function
    return(missing_data)
  }
  else {
    message("No missing data were found.")
    return(missing_data_all)
  }
}
