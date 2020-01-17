#' @name rc_logicCheck
#'
#' @title Check branching logic for errors
#' @description  Check branching logic for common errors
#' @details This function will verify that all event and variable names used in the
#' branching logic exist in the project metadata. It will also warn of faulty logic
#' concerning \code{[event-name] = 'event_name'} conditions.
#'
#' @param bundle A bundle object created by \code{rc_exportBundle} containing project metadata.
#'
#' @author Marcus Lehr
#' @export

rc_logicCheck <- function(bundle) {

  if (is.null(bundle[["meta_data"]])) {
    message("[['meta_data']] not found in bundle object. Please add it using
            bundle[['meta_data']] = rc_exportBundle(rcon, meta_data = T)")
  } else {
    validate_events(bundle)
    validate_variables(bundle)
    validate_event_logic(bundle)
  }
}

# Validate event names --------------------------------------------------------------------

validate_events <- function(bundle) {

  branchingLogic = bundle[["meta_data"]]$branching_logic
  events = bundle[["events"]]$unique_event_name

  # Extract all events matching the pattern "[event-name] (=|!=) 'event_name'"
  eventCalls1 = stringr::str_extract_all(branchingLogic, "(?<=\\[event-name\\]\\s?(=|!=)\\s?('|\"))(\\w*)(?=('|\"))")
  # Extract all events matching the pattern "[event-name][var_name]"
  eventCalls2 = stringr::str_extract_all(branchingLogic, "(?<=\\[)(\\w*)(?=\\]\\[)")

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
    invalidIndex = stringr::str_detect(branchingLogic, invalidEvents)
    invalidIndex[is.na(invalidIndex)] = FALSE

    # Print results
    message("\nInvalid event names were found in the following fields:")
    print(bundle[["meta_data"]]$field_name[invalidIndex])
    message("Invalid event names:")
    print(invalidEvents)
  } else {
    message("\nNo invalid event names were found")
  }
}

# Validate variable names -------------------------------------------------

validate_variables <- function(bundle) {

  branchingLogic = bundle[["meta_data"]]$branching_logic
  vars = bundle[["meta_data"]]$field_name

  # Extract varaible names used in branching logic by mathcing the pattern "[var_name]"
  # without capturing events by excluding bracketed words followed by "["
  varCalls = stringr::str_extract_all(branchingLogic, "(?<=\\[)(\\w+)(?=\\]([^\\[]|$))")
  varCalls = unlist(
                lapply(varCalls, function(x) x[!is.na(x)] )
                )

  # Find invalid names
  invalidVars = unique(varCalls[!varCalls %in% vars])

  if (length(invalidVars) > 0) {

    # Locate invalid variables in data
    invalidIndex = stringr::str_detect(branchingLogic, invalidVars)
    invalidIndex[is.na(invalidIndex)] = FALSE

    # Print results
    message("\nInvalid variable names were found in the following fields:")
    print(bundle[["meta_data"]]$field_name[invalidIndex])
    message("Invalid variable names:")
    print(invalidVars)
  } else {
    message("\nNo invalid variable names were found")
  }
}


# Check [event-name] logic -----------------------------------------------------

# Warn user of "[event-name] = 'event_name' AND [event-name] = 'event_name'" (field always hidden)
# or "[event-name] != 'event_name' OR [event-name] != 'event_name'" logic (field never hidden)

validate_event_logic <- function(bundle) {

  branchingLogic = bundle[["meta_data"]]$branching_logic
  invalidLogic = data.frame(operator = c('=','!='),
                            gate = c('AND','OR'))
  logicErrors = 0
  hidden = c('hidden', 'displayed')
  for (i in 1:nrow(invalidLogic)) {
    # Find logic matching one of the above descriptions
    invalidIndex = stringr::str_detect(branchingLogic, stringr::regex(
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
              "This will result in the field always being ", hidden[i],
              ". Please review branching\nlogic in the following fields:")
      print(bundle[["meta_data"]]$field_name[invalidIndex])
      logicErrors = logicErrors+1
    }
  }
  if (logicErrors == 0) message("\nNo [event-name] logic errors were found.")
}
