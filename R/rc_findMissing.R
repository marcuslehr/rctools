#' @name rc_findMissing
#'
#' @title Find missing record data
#' @description  Finds data points which are ostenibly missing from Redcap records data.
#' @details For each event/variable combination, if a value exists for at least one
#' record then it is expected to exist for all records. If an entire event is empty for
#' a given record, none of the variables will be flagged unless there is data for that
#' record in the following event. Additionally, if the subject is marked as having
#' completed the study (i.e. \code{completionField == 'Yes'|1}), then empty events for
#' that subject will not be discarded.
#'
#' The logic of this function does not extend to checkbox variables or repeat instruments.
#' If present in the record data, they will be dropped.
#'
#' @param records A raw data export from REDCap.
#' @param completionField The REDCap variable which indicates whether or not a subject
#' has completed the study. This should be indicated by a 'Yes' or a '1' (i.e. a Yes/No
#' field in REDCap).
#' @param bundle A bundle object created by \code{rc_exportBundle} containing project metadata.
#'
#' @importFrom magrittr '%>%'
#'
#' @author Marcus Lehr
#' @export

rc_findMissing <- function(records, completionField, bundle = NULL) {


# Checks ------------------------------------------------------------------

  ## record_id field ---

  if (!is.null(bundle[["meta_data"]])) {
    # Ensure record_id field is named appropriately
    colnames(records)[colnames(records)==bundle[["meta_data"]][1,1]] = 'record_id'

  } else {
    origID = names(records)[grepl('hnrcid|hnrc_id|record_id', names(records), ignore.case = T)]

    if (length(origID) == 1) {
      message("[['meta_data']] not found in bundle object. 'record_id' field will be assumed to be: '", origID, "'")
      names(records)[names(records)==origID] = 'record_id'

    } else if (length(origID != 1)) {
      names(records)[1] = 'record_id'
      message("'record_id' field could not be found, it will assumed to be the first column.
              For better reproducibility, please supply a bundle object containing [['meta_data']]")
    }
  }

  ## Event data ---

  if (!is.null(bundle[["events"]])) {
    events = bundle[["events"]]$unique_event_name

  } else if (!is.null(records$redcap_event_name)) {

    #Collect list of events present in data, ensuring event order is preserved
    events = as.character(unique(records$redcap_event_name))
    message("[['events']] could not be found in bundle object. Event list will be captured from record data.")

  } else {
    message("Event data could not be found. Please supply a bundle object containing [['events']]")
    break
  }

  ## Completion data ---

  # Would be good to test for a single result here
  if (any(names(records)==completionField)) {
    names(records)[names(records)==completionField] = 'study_complete'
  } else {
    message("Completion field could not be found. Please include a field in the record data indicating
            whether or not a subject has completed the study.")
  }

  ## Repeat/checkbox fields ---
  if (any(!is.na(records$redcap_repeat_instance))|any(grepl('___',names(records)))) {
    message("The logic of this function does not translate to repeat instruments or checkbox fields.
              All such data will be dropped.")
    records = dplyr::filter(records, is.na(redcap_repeat_instance)) %>%
                dplyr::select(-redcap_repeat_instrument,-redcap_repeat_instance,
                              -dplyr::contains('___'))
  }


# Setup -------------------------------------------------------------------

  # Grab completion data then remove '_complete' fields from data
  completionData = dplyr::select(records, record_id, study_complete) %>% na.omit()
  # completionData$record_id = as.character(completionData$record_id)
  records = dplyr::select(records, -dplyr::contains('complete'))

  # Convert data to long format. Globally empty events and IDs will be lost
  meltVars = c('record_id','redcap_event_name')
  data = suppressWarnings(
              reshape2::melt(records, id.vars = meltVars, na.rm = T) %>% dplyr::as_tibble() %>%
                      dplyr::filter(!value == '') %>% droplevels()
                          )

  # Filter events for those remaining in data
  events = events[events %in% data$redcap_event_name]

  # Collecting IDs outside the loop allows capturing of (non-globally) empty events
  IDs = as.character(unique(data$record_id))



# Collect missing data ----------------------------------------------------

  # Loop through all events to find missing data. The variables that should occur within
  # an event are determined by the presence of a value for at least one subject. If a subject
  # has no data for an event (within the subset of variables pulled from redcap), then they will not
  # be captured as missing unless data exists for the following event or the subject is indicated to
  # have completed the study via the 'study_complete' field.



  missingDataAll = data.frame()
  for (e in events) {
    #Filter data for an event and capture variables that are present
    eventData = dplyr::filter(data, redcap_event_name == e)

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
    dataWide = reshape2::dcast(data, record_id+redcap_event_name~variable)
    dataWideFull = dplyr::full_join(expectedEvents, dataWide, by = meltVars)

    logic = dataWideFull %>%
      dplyr::mutate(var_sums = rowSums(!is.na(.))-2) %>% dplyr::group_by(record_id) %>%
      dplyr::mutate(datafollowing = ifelse(test = dplyr::lead(var_sums) != 0, yes = T, no = F)) %>%
      dplyr::left_join(., completionData, by = 'record_id') %>%
      dplyr::select(record_id, redcap_event_name, var_sums, datafollowing, study_complete)


    missingData = suppressWarnings(
                  dplyr::left_join(missingDataAll, logic, by = meltVars) %>%
                    dplyr::filter(var_sums > 0 | datafollowing == T |
                                    study_complete == 'Yes' | study_complete == 1) %>%
                    dplyr::select(record_id, redcap_event_name, variable)
                  )

    if (!is.null(bundle[['meta_data']])) {

      # Add form names for ease of locating in Redcap
      instrVarMap = data.frame(variable = bundle[['meta_data']]$field_name,
                               form_name = bundle[['meta_data']]$form_name)
      missingData = suppressWarnings(
                    dplyr::left_join(missingData, instrVarMap, by = 'variable') %>%
                      dplyr::select(record_id, redcap_event_name, form_name, variable)
                    )
    }

    return(missingData)

  } else {
    message("No missing data were found.")
    return(missingDataAll)
  }
}
