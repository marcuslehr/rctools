#' @name rc_missing
#'
#' @title Find missing record data
#' @description  Finds data points which are ostenibly missing from Redcap records data.
#' 
#' For each event/variable combination, if a value exists for at least one
#' record then it is expected to exist for all records. If an entire event is empty for
#' a given record, none of the variables will be flagged unless there is data for that
#' record in the following event. Additionally, if the subject is marked as having
#' completed the study (i.e. \code{completion_field == 'Yes'|1}), then empty events for
#' that subject will not be discarded.
#'
#' The logic of this function does not extend to checkbox variables or repeat instruments.
#' If present in the record data, they will be dropped.
#'
#' @param record_data Record data export from REDCap
#' @param completion_field Character. The REDCap variable which indicates whether or not a subject
#' has completed the study. This should be indicated by a 'Yes' or a '1' (i.e. a Yes/No
#' field in REDCap). If not provided, some missing data may be lost.
#' @param bundle List. A project metadata bundle as created by \code{rc_setup}. By default, the 
#' bundle is expected in the option "redcap_bundle".
#' @param data_dict Dataframe. REDCap project data data_dictionary. By default, 
#' $data_dict is expected in a REDCap bundle object, as created by \code{rc_setup}. Otherwise, an 
#' equivalent data.frame containing the project data dictionary must be supplied.
#' @param event_data Dataframe. REDCap project event metadata. By default, $event_data is expected in
#' the REDCap bundle, as created by \code{rc_setup}. Otherwise, an equivalent data.frame containing 
#' event data must be supplied.
#' @param form_perm Dataframe. User access permissions for REDCap forms. By default, $form_perm
#' is expected in the REDCap bundle, as created by \code{rc_setup}. Otherwise, an equivalent 
#' data.frame containing form permission data must be supplied.
#' 
#' @param plot Logical. Determine if missing results should be plotted. These are useful 
#' for investigating the structure of missing data, especially when a large amount is returned.
#' @param table Logical. If \code{TRUE} a list will be returned containing the missing dataframe
#' and two missing tables. The tables contain the percent of missing record_idor variable counts.
#' To view formatted tables, use square bracket indexing- e.g. data[['IDs_table']]
#'
#' @importFrom magrittr '%>%'
#'
#' @author Marcus Lehr
#' @export

rc_missing <- function(record_data, completion_field = NULL, 
                       bundle = getOption("redcap_bundle"),
                       data_dict = getOption("redcap_bundle")$data_dict,
                       event_data = getOption("redcap_bundle")$event_data,
                       form_perm = getOption("redcap_bundle")$form_perm,
                       plot = FALSE, table = FALSE) {


# Checks ------------------------------------------------------------------

  # Get individual metadata objects from bundle if not supplied
  if (!is.null(bundle)) {
    if (is.null(data_dict)) data_dict = bundle$data_dict
    if (is.null(event_data)) event_data = bundle$event_data
    if (is.null(form_perm)) form_perm = bundle$form_perm
  }
  
  validate_args(required = c('record_data', 'data_dict'),
                record_data = record_data,
                completion_field = completion_field,
                bundle = bundle,
                data_dict = data_dict,
                event_data = event_data,
                form_perm = form_perm,
                plot = plot,
                table = table)
  
  
  ## Event data ---

  if (!is.null(event_data$unique_event_name)) {
    events = event_data$unique_event_name

  } else if (!is.null(record_data$redcap_event_name)) {

    #Collect list of events present in data, ensuring event order is preserved
    events = as.character(unique(record_data$redcap_event_name))
    message("Event list not found in REDCap bundle- it will be captured from record data.")

  } else {
    stop("Event data could not be found. Please supply event metadata from Redcap or ensure that your 
         data contains the 'redcap_event_name' column.")
  }
  

# Prep data -------------------------------------------------------------------  
  
  ## Get record_id field name ---
  id_field = getID(record_data, data_dict)
  
  ## Filter ---
  
  if (any(!is.na(record_data[['redcap_repeat_instance']])) | 
      any(data_dict$field_type == 'calc') |
      any(grepl('___',names(record_data))) ) {
    
    message("The logic of this function does not translate to repeat instruments, checkbox,
		or calculated fields. All such data will be dropped.")
    
    # Remove repeat instrument data
    if (!is.null(record_data$redcap_repeat_instance))
      record_data = dplyr::filter(record_data, is.na(redcap_repeat_instance)) %>% # Repeat rows
                      dplyr::select(-dplyr::contains('redcap_repeat')) # Repeat ID columns
    
    # Remove other unwanted columns
    fields_calc = data_dict$field_name[data_dict$field_type == 'calc']
    fields_calc = fields_calc[fields_calc %in% names(record_data)]
    record_data = dplyr::select(record_data, 
                                -all_of(fields_calc), # Calculated fields
											          -dplyr::contains('___')) # Checkbox data
  }
  
  # Remove hidden fields
  fields_hidden = data_dict$field_name[data_dict$field_annotation == '@HIDDEN'] %>% na.omit()
  if (any(fields_hidden %in% names(record_data))) {
    message("Any fields hidden by the '@HIDDEN action tag will dropped.")
    record_data = dplyr::select(record_data, -all_of(fields_hidden))
  }
  
  ## Completion data ---
  
  if (any(names(record_data)==completion_field)) {
    # Grab completion data then remove from data
    completionData = dplyr::select(record_data, all_of(id_field), all_of(completion_field)) %>% na.omit()
    record_data = dplyr::select(record_data, -all_of(completion_field))
    # completionData$record_id = as.character(completionData$record_id)
  }
  else message("Completion field not found. Some missing data may not be captured.")
  
	# Remove _complete fields
  if (any(grepl('_complete',names(record_data))))
    record_data = dplyr::select(record_data, -dplyr::contains('_complete'))

  # Convert data to long format. Globally empty events and IDs will be lost
  meltVars = c(id_field,'redcap_event_name')
  record_data = suppressWarnings(
              reshape2::melt(record_data, id.vars = meltVars, na.rm = T) %>% dplyr::as_tibble() %>%
                      dplyr::filter(!value == '') %>% droplevels()
                          )

  # Filter events for those remaining in the data
  events = events[events %in% record_data$redcap_event_name]

  # Collecting IDs outside the loop allows capturing of (non-globally) empty events
  IDs = as.character(unique(record_data[[id_field]]))


# Collect missing data ----------------------------------------------------

  # Loop through all events to find missing data. The variables that should occur within
  # an event are determined by the presence of a value for at least one subject. If a subject
  # has no data for an event (within the subset of variables pulled from redcap), then they will not
  # be captured as missing unless data exists for the following event or the subject is indicated to
  # have completed the study via the 'study_complete' field.

  missingDataAll = data.frame()
  for (e in events) {
    #Filter data for an event and capture variables that are present
    eventData = dplyr::filter(record_data, redcap_event_name == e)

    # Any vars empty for this entire event are dropped
    vars = as.character(unique(eventData$variable)) 

    #Generate a table with all possible combinations
    expectedData = data.frame(record_id = rep(IDs, length(vars)) %>%
                                              sort() %>% as.numeric(),
                               redcap_event_name = e,
                               variable = vars)
    names(expectedData)[1] = id_field

    #Collect entries which are expected but not present
    missingDataAll <- suppressWarnings(suppressMessages(
              rbind(missingDataAll, dplyr::anti_join(expectedData, eventData))
      ))
  }

# Filter data -------------------------------------------------------------

  if (nrow(missingDataAll) > 0) {
    
    # Sort df
    missingDataAll = dplyr::arrange(missingDataAll, !!dplyr::sym(id_field))

    ## Filter missingData to remove values from empty events which do not have data in a following event
    expectedEvents = data.frame(record_id = sort(rep(IDs, length(events))) %>% as.numeric(),
                                redcap_event_name = events)
    names(expectedEvents)[1] = id_field
    cast_formula = paste(paste(id_field)," + redcap_event_name ~ variable")
    dataWide = reshape2::dcast(record_data, cast_formula)
    
    # Add expected events which are missing
    dataWide = suppressWarnings(
                      dplyr::full_join(expectedEvents, dataWide, by = meltVars)
                   )
    
    # Filter events which are empty AND do not have data in the following event (per subject)
    if (!is.null(completion_field)) {
      logic = suppressWarnings(dataWide %>%
                dplyr::mutate(var_sums = rowSums(!is.na(.))-2) %>% dplyr::group_by_(id_field) %>%
                dplyr::mutate(data_following = ifelse(test = dplyr::lead(var_sums) != 0, yes = T, no = F)) %>%
                dplyr::left_join(., completionData, by = id_field) %>%
                dplyr::select(all_of(id_field), redcap_event_name, var_sums, data_following, all_of(completion_field))
              )
      missingData = suppressWarnings(
                      dplyr::left_join(missingDataAll, logic, by = meltVars) %>%
                      dplyr::filter(var_sums > 0 | data_following == T | !!dplyr::sym(completion_field) == 'Yes|1') %>%
                      dplyr::select(all_of(id_field), redcap_event_name, variable)
                    )
    }
    
    # Perform above filtering without completion_field if not provided
    else {
      logic = suppressWarnings(dataWide %>%
               dplyr::mutate(var_sums = rowSums(!is.na(.))-2) %>% dplyr::group_by_(id_field) %>%
               dplyr::mutate(data_following = ifelse(test = dplyr::lead(var_sums) != 0, yes = T, no = F)) %>%
               dplyr::select(all_of(id_field), redcap_event_name, var_sums, data_following)
              )
      missingData = suppressWarnings(
                      dplyr::left_join(missingDataAll, logic, by = meltVars) %>%
                      dplyr::filter(var_sums > 0 | data_following == T) %>%
                      dplyr::select(all_of(id_field), redcap_event_name, variable)
                    )
    }
    
    
      
    # Add form names for ease of locating in Redcap
    instrVarMap = data.frame(variable = data_dict$field_name,
                             form_name = data_dict$form_name)
    missingData = suppressWarnings(
                  dplyr::left_join(missingData, instrVarMap, by = 'variable') %>%
                    dplyr::select(all_of(id_field), redcap_event_name, form_name, variable)
                  )
    
    # Remove forms where no users have access
    if (!is.null(form_perm)) {
      
      form_perm = bundle2935$form_perm
      forms_available = subset(form_perm, permission != 'No access')$form_name %>% unique()
      forms_hidden = subset(form_perm, permission == 'No access')$form_name %>% unique()
      forms_hidden = forms_hidden[!forms_hidden %in% forms_available]
      
      if (length(forms_hidden) > 0) {
        message("The following forms will be removed as no users have access to them: ", paste(forms_hidden, collapse = ', '))
        missingData = missingData[!missingData$form_name %in% forms_hidden,]
      }
    }
  }
  else {
    message("No missing data were found.")
    return(missingDataAll)
  }


# Return ------------------------------------------------------------------

  if (nrow(missingData)/nrow(record_data) > .1 &
      plot == F & table == F)
    warning("More than 10% of the data appears to be missing. Manual review for false positives is recommended,
            consider using the 'plot' or 'table' arguments.")
  
  if (plot == T) plot_missing(missingData)
  
  if (table == T) {
    return_data = list(missing_data = missingData)
    tables = table_missing(missingData, IDs, logic, id_field)
    return_data = c(return_data, tables)
    
    message("List object returned. Use '[' indexing to view formatted tables.")
    return(return_data)
  } 
  else return(missingData)
}

# Table function --------------------------------------------------------

table_missing <- function(missingData, IDs, logic, id_field = getOption("redcap_bundle")$id_field) {
  
  # Tally IDs
  tally = dplyr::count(missingData, redcap_event_name, variable)
  tally$pct_missing = tally$n/length(IDs)*100
  tally = reshape2::dcast(tally, variable~redcap_event_name, value.var = 'pct_missing')
  tally[is.na(tally)] = 0
  
  ## This will return an unsaved table in the source viewer
  # table_ids = tally
  # View(table_ids)
  
### Formatted tables are not exporting properly :'(
  
  # Create attr list for table
  table_attr = list('variable' = formattable::formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold")))

  for (e in names(tally)[-1]) {
    table_attr[[e]] = formattable::color_tile('white','red')
  }

  tables = list(IDs_table = formattable::formattable(tally,table_attr, 
                                               caption = paste0('Percent of IDs Missing (n = ',length(IDs),')'))
            )
    
  
  # Tally vars
  var_maxes = logic %>% dplyr::group_by(redcap_event_name) %>% dplyr::summarise(max = max(var_sums))
  
  tally = dplyr::count(missingData, redcap_event_name, !!dplyr::sym(id_field))
  tally = dplyr::left_join(tally, var_maxes, by = 'redcap_event_name')
  tally$pct_missing = tally$n/tally$max*100
  
  cast_formula = paste(id_field,'~ redcap_event_name')
  tally = reshape2::dcast(tally, cast_formula, value.var = 'pct_missing')
  tally[is.na(tally)] = 0
  
  # table_vars = tally
  # View(table_vars)
  
  # Create attr list for table
  table_attr = list()
  table_attr[[id_field]] = formattable::formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold"))

  for (e in names(tally)[-1]) {
    table_attr[[e]] = formattable::color_tile('white','red')
  }

  tables[['variables_table']] = formattable::formattable(tally,table_attr, caption = paste('Percent of Variables Missing.
                                                               Totals: ', paste(var_maxes$max, collapse = ',')))

  return(tables)
}
  

