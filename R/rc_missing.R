#' @name rc_missing
#'
#' @title Find missing record data
#' @description  Finds data points which are ostenibly missing from Redcap records data.
#' @details For each event/variable combination, if a value exists for at least one
#' record then it is expected to exist for all records. If an entire event is empty for
#' a given record, none of the variables will be flagged unless there is data for that
#' record in the following event. Additionally, if the subject is marked as having
#' completed the study (i.e. \code{completion_field == 'Yes'|1}), then empty events for
#' that subject will not be discarded.
#'
#' The logic of this function does not extend to checkbox variables or repeat instruments.
#' If present in the record data, they will be dropped.
#'
#' @param record_data A raw data export from REDCap.
#' @param completion_field The REDCap variable which indicates whether or not a subject
#' has completed the study. This should be indicated by a 'Yes' or a '1' (i.e. a Yes/No
#' field in REDCap).
#' @param data_dict REDCap project data data_dictionary. By default, 
#' $data_dict is expected in a REDCap bundle object, as created by \code{rc_setup}.
#' Otherwise, a data.frame containing the metadata must be supplied.
#' @param events REDCap project event metadata. By default, $events is expected in
#' the REDCap bundle, as created by \code{rc_setup}. Otherwise, a data.frame containing
#' event data must be supplied.
#'
#' @importFrom magrittr '%>%'
#'
#' @author Marcus Lehr
#' @export

rc_missing <- function(record_data, completion_field, 
                           data_dict = getOption("redcap_bundle")$data_dict,
                           events = getOption("redcap_bundle")$events) {


# Checks ------------------------------------------------------------------

  ## record_id field ---

  if (!is.null(data_dict)) {
    # Ensure record_id field is named appropriately
    colnames(record_data)[colnames(record_data)==data_dict[1,1]] = 'record_id'

  } else {
      message("$data_dict not found in REDCap bundle. 'record_id' field will be assumed to be the first column")
      names(record_data)[1] = 'record_id'
    }

  ## Event data ---

  if (!is.null(events$unique_event_name)) {
    events = events$unique_event_name

  } else if (!is.null(record_data$redcap_event_name)) {

    #Collect list of events present in data, ensuring event order is preserved
    events = as.character(unique(record_data$redcap_event_name))
    message("$events could not be found in REDCap bundle. Event list will be captured from record data.")

  } else {
    stop("Event data could not be found. Please ensure that your data contains the 'redcap_event_name' column.")
  }

  ## Completion data ---

  # Would be good to test for a single result here
  if (any(names(record_data)==completion_field)) {
    names(record_data)[names(record_data)==completion_field] = 'study_complete'
  } else {
    stop("Completion field could not be found. Please include a field in the record data indicating
            whether or not a subject has completed the study.")
  }

  ## Repeat/checkbox fields ---
  if (any(!is.na(record_data$redcap_repeat_instance))|any(grepl('___',names(record_data)))) {
    message("The logic of this function does not translate to repeat instruments or checkbox fields.
              All such data will be dropped.")
    record_data = dplyr::filter(record_data, is.na(redcap_repeat_instance)) %>%
                dplyr::select(-redcap_repeat_instrument,-redcap_repeat_instance,
                              -dplyr::contains('___'))
  }


# Setup -------------------------------------------------------------------

  # Grab completion data then remove '_complete' fields from data
  completionData = dplyr::select(record_data, record_id, study_complete) %>% na.omit()
  # completionData$record_id = as.character(completionData$record_id)
  record_data = dplyr::select(record_data, -dplyr::contains('complete'))

  # Convert data to long format. Globally empty events and IDs will be lost
  meltVars = c('record_id','redcap_event_name')
  record_data = suppressWarnings(
              reshape2::melt(record_data, id.vars = meltVars, na.rm = T) %>% dplyr::as_tibble() %>%
                      dplyr::filter(!value == '') %>% droplevels()
                          )

  # Filter events for those remaining in data
  events = events[events %in% record_data$redcap_event_name]

  # Collecting IDs outside the loop allows capturing of (non-globally) empty events
  IDs = as.character(unique(record_data$record_id))



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
    vars = as.character(unique(eventData$variable)) # rc_variables was a backup from pooling.
                                                    # Need to resolve this such that pooling doesn't break this

    #Generate a table with all possible combinations
    expectedData = data.frame(record_id = rep(IDs, length(vars)) %>%
                                              sort() %>% as.numeric(),
                               redcap_event_name = e,
                               variable = vars)

    #Collect entries which are expected but not present
    missingDataAll <- suppressWarnings(suppressMessages(
              rbind(missingDataAll, dplyr::anti_join(expectedData, eventData))
      ))
  }
  missingDataAll = dplyr::arrange(missingDataAll, record_id)

# Filter data -------------------------------------------------------------

  if (nrow(missingDataAll) > 0) {

    ## Filter missingData to remove values from empty events which do not have data in a following event
    expectedEvents = data.frame(record_id = sort(rep(IDs, length(events))) %>% as.numeric(),
                                redcap_event_name = events)
    dataWide = reshape2::dcast(record_data, record_id+redcap_event_name~variable)
    dataWideFull = suppressWarnings(
                      dplyr::full_join(expectedEvents, dataWide, by = meltVars)
                   )

    logic = suppressWarnings(dataWideFull %>%
              dplyr::mutate(var_sums = rowSums(!is.na(.))-2) %>% dplyr::group_by(record_id) %>%
              dplyr::mutate(datafollowing = ifelse(test = dplyr::lead(var_sums) != 0, yes = T, no = F)) %>%
              dplyr::left_join(., completionData, by = 'record_id') %>%
              dplyr::select(record_id, redcap_event_name, var_sums, datafollowing, study_complete)
            )


    missingData = suppressWarnings(
                  dplyr::left_join(missingDataAll, logic, by = meltVars) %>%
                    dplyr::filter(var_sums > 0 | datafollowing == T |
                                    study_complete == 'Yes' | study_complete == 1) %>%
                    dplyr::select(record_id, redcap_event_name, variable)
                  )

    if (!is.null(bundle[['data_dict']])) {

      # Add form names for ease of locating in Redcap
      instrVarMap = data.frame(variable = bundle[['data_dict']]$field_name,
                               form_name = bundle[['data_dict']]$form_name)
      missingData = suppressWarnings(
                    dplyr::left_join(missingData, instrVarMap, by = 'variable') %>%
                      dplyr::select(record_id, redcap_event_name, form_name, variable)
                    )
    }

    return(missingData)

  } 
  else {
    message("No missing data were found.")
    return(missingDataAll)
  }
}
