#' @name rc_logic_check
#'
#' @title Check branching logic for errors
#' @description  Check branching logic for common errors
#' @details This function will verify that all event and variable names used in the
#' branching logic exist in the project metadata. It will also warn of faulty logic
#' concerning \code{[event-name] = 'event_name'} conditions.
#'
#' @param data_dict REDCap project data data_dictionary. By default, 
#' $data_dict is expected in the REDCap bundle option, as created by \code{rc_setup}.
#' Otherwise, a data.frame containing the metadata must be supplied.
#' @param event_data REDCap events metadata. By default, $event_data is expected 
#' in the REDCap bundle option, as created by \code{rc_setup}.
#' Otherwise, a data.frame containing the metadata must be supplied.
#'
#' @author Marcus Lehr
#' 
#' @export

rc_logic_check <- function(data_dict = getOption("redcap_bundle")$data_dict,
                           event_data = getOption("redcap_bundle")$event_data) {

  validate_args(required = c('data_dict','event_data'),
                data_dict = data_dict, event_data = event_data)
  

    branching_logic = data_dict$branching_logic
    events = event_data$unique_event_name
    field_names = data_dict$field_name
    
    validate_events(branching_logic, events)
    validate_variables(branching_logic, field_names)
    validate_event_logic(branching_logic)
}

# Validate event names --------------------------------------------------------------------

validate_events <- function(branching_logic, events) {

  # branching_logic = data_dict$branching_logic
  # events = event_data$unique_event_name

  # Extract all events matching the pattern "[event-name] (=|!=) 'event_name'"
  eventCalls1 = stringr::str_extract_all(branching_logic, "(?<=\\[event-name\\]\\s?(=|!=)\\s?('|\"))(\\w*)(?=('|\"))")
  # Extract all events matching the pattern "[event-name][var_name]"
  eventCalls2 = stringr::str_extract_all(branching_logic, "(?<=\\[)(\\w*)(?=\\]\\[)")

  # Combine lists, remove smart variables and NAs
  eventCalls = c(eventCalls1,eventCalls2)
  eventCalls = unlist(
                  lapply(eventCalls, function(x) {
                    x = x[!is.na(x)]
                    x[!stringr::str_detect(x, 'event-name')]
                  })
               )

  # Identify invalid names
  invalidEvents = unique(eventCalls[!eventCalls %in% events])

  if (length(invalidEvents) > 0) {

    # Locate invalid events in data
    invalidIndex = stringr::str_detect(branching_logic, invalidEvents)
    invalidIndex[is.na(invalidIndex)] = FALSE

    # Print results
    message("\nInvalid event names were found in the following fields:")
    print(data_dict$field_name[invalidIndex])
    message("Invalid event names:")
    print(invalidEvents)
  } else {
    message("\nNo invalid event names were found")
  }
}

# Validate variable names -------------------------------------------------

validate_variables <- function(branching_logic, field_names) {

  # branching_logic = data_dict$branching_logic
  # field_names = data_dict$field_name

  # Extract varaible names used in branching logic by mathcing the pattern "[var_name]"
  # without capturing events by excluding bracketed words followed by "["
  varCalls = stringr::str_extract_all(branching_logic, "(?<=\\[)(\\w+)(?=\\]([^\\[]|$))")
  varCalls = unlist(
                lapply(varCalls, function(x) x[!is.na(x)] )
                )

  # Find invalid names
  invalidVars = unique(varCalls[!varCalls %in% field_names])

  if (length(invalidVars) > 0) {

    # Locate invalid variables in data
    invalidIndex = stringr::str_detect(branching_logic, invalidVars)
    invalidIndex[is.na(invalidIndex)] = FALSE

    # Print results
    message("\nInvalid variable names were found in the following fields:")
    print(data_dict$field_name[invalidIndex])
    message("Invalid variable names:")
    print(invalidVars)
  } else {
    message("\nNo invalid variable names were found")
  }
}


# Check [event-name] logic -----------------------------------------------------

# Warn user of "[event-name] = 'event_name' AND [event-name] = 'event_name'" (field always hidden)
# or "[event-name] != 'event_name' OR [event-name] != 'event_name'" logic (field never hidden)

validate_event_logic <- function(branching_logic) {

  # branching_logic = data_dict$branching_logic
  invalidLogic = data.frame(operator = c('=','!='),
                            gate = c('AND','OR'),
                            result = c('hidden', 'displayed'))
  logicErrors = 0
  for (i in 1:nrow(invalidLogic)) {
    # Find logic matching one of the above descriptions
    invalidIndex = stringr::str_detect(branching_logic, stringr::regex(
                         paste0("\\[event-name\\] ?", invalidLogic[i,1],
                                " ?('|\")\\w*('|\") ", invalidLogic[i,2],
                                " \\[event-name\\] ?", invalidLogic[i,1]),
                         ignore_case = T
                         ))
    invalidIndex[is.na(invalidIndex)] = FALSE
    if (any(invalidIndex)) {
      message("\nUse of \"[event-name]", invalidLogic[i,1],
              "'event_name' ",invalidLogic[i,2],
              " [event-name]", invalidLogic[i,1],
              "'event_name'\" logic found.\n",
              "This will result in the field always being ", invalidLogic[i,3],
              ". Please review branching\nlogic in the following fields:")
      print(data_dict$field_name[invalidIndex])
      logicErrors = logicErrors+1
    }
  }
  if (logicErrors == 0) message("\nNo [event-name] logic errors were found.")
}
