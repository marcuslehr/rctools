#' @name rc_logic_check
#'
#' @title Check branching logic for errors
#' @description  Check branching logic for common errors
#' @details This function will verify that all event and variable names used in the
#' branching logic exist in the project metadata. 
#' 
#' Warnings will be shown for faulty event logic in the format 
#' \code{[event-name] = 'event_name' AND [event-name] = 'event_name'} (field always hidden) or
#' \code{[event-name] != 'event_name' OR [event-name] != 'event_name'} (field never hidden).
#'
#' @param data_dict REDCap project data data_dictionary. By default, 
#' $data_dict is expected in the REDCap bundle option, as created by \code{rc_bundle}.
#' Otherwise, a data.frame containing the metadata must be supplied.
#' @param events Character. Vector of redcap event names
#'
#' @author Marcus Lehr
#' 
#' @export

rc_logic_check <- function(data_dict = getOption("redcap_bundle")$data_dict,
                           events = getOption("redcap_bundle")$event_data$unique_event_name) {

  validate_args(required = c('data_dict','events'),
                data_dict = data_dict, events = events)
  

    branching_logic = data_dict$branching_logic
    fields = data_dict$field_name
    
    validate_events(branching_logic, events, fields)
    validate_variables(branching_logic, fields)
    validate_event_logic(branching_logic, fields)
    
    # Probably only needed for =/!= 0 logic
    # validate_value_logic(branching_logic, fields)
}

# Validate event names --------------------------------------------------------------------

validate_events <- function(branching_logic, events, fields) {

  # Extract all events matching the pattern "[event-name] (=|!=) 'event_name'"
  eventCalls1 = stringr::str_extract_all(branching_logic, "(?<=\\[event-name\\]\\s?(=|!=)\\s?('|\"))(\\w+)(?=('|\"))")
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
    invalidIndex = stringr::str_detect(branching_logic, paste(invalidEvents, collapse = '|'))
    invalidIndex[is.na(invalidIndex)] = FALSE

    # Print results
    message("\nInvalid event names were found in the following fields:")
    print(fields[invalidIndex])
    message("Invalid event names:")
    print(invalidEvents)
  } else {
    message("\nNo invalid event names were found")
  }
}

# Validate variable names -------------------------------------------------

validate_variables <- function(branching_logic, fields) {

  # Extract varaible names used in branching logic by matching the pattern "[var_name]"
  # without capturing events by excluding bracketed words followed by "["
  varCalls = stringr::str_extract_all(branching_logic, "(?<=\\[)(\\w+)(?=\\]([^\\[]|$))")
  varCalls = unlist(
                lapply(varCalls, function(x) x[!is.na(x)] )
                )

  # Find invalid names
  invalidVars = unique(varCalls[!varCalls %in% fields])

  if (length(invalidVars) > 0) {

    # Locate invalid variables in data
    invalidIndex = stringr::str_detect(branching_logic, invalidVars)
    invalidIndex[is.na(invalidIndex)] = FALSE

    # Print results
    message("\nInvalid variable names were found in the following fields:")
    print(fields[invalidIndex])
    message("Invalid variable names:")
    print(invalidVars)
  } else {
    message("\nNo invalid variable names were found")
  }
}


# Check [event-name] logic -----------------------------------------------------

# Warn user of "[event-name] = 'event_name' AND [event-name] = 'event_name'" (field always hidden)
# or "[event-name] != 'event_name' OR [event-name] != 'event_name'" logic (field never hidden)

validate_event_logic <- function(branching_logic, fields) {

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
      print(fields[invalidIndex])
      logicErrors = logicErrors+1
    }
  }
  if (logicErrors == 0) message("\nNo [event-name] logic errors were found.")
}


# Check value logic ---------------------------------------------------

validate_value_logic <- function(branching_logic, fields) {
  
  # Find logic matching the pattern [variable] = digits, without quotes surrounding digits
  invalidIndex = which(
                    stringr::str_detect(branching_logic, "\\[\\w+\\]\\s?(=|!=)\\s?\\d+")
                  )
  
  # Print results
  if (length(invalidIndex)) {
    message("\nNumerical coding values must be quoted to work properly. Errors were found in the following fields:")
    print(fields[invalidIndex])
  } else {
    message("\nNo invalid numerical coding logic was found.")
  }
}
