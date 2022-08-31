
# format_variables --------------------------------------------------------------

#' @name format_variables
#' @importFrom chron times
#' 
#' @title Convert a REDCap Data Field to an R Vector
#' @description Converts a field exported from REDCap into a valid R vector
#' 
#' @param record_data A data frame of records returned by \code{rc_export} 
#' @param data_dict A data frame giving the data data_dictionary, as returned 
#'   by \code{exportMetaData}
#' @param factor_labels Logical, determines if checkbox, radio button, dropdown and yesno
#'   variables are labelled.
#' @param dates Logical, determines if date variables are converted to POSIXct format
#'   
#' @author Jeffrey Horner

format_variables <- function(record_data, data_dict, factor_labels = TRUE, 
                       dates = TRUE)
{ 
  for (i in seq_along(record_data))
  {
    field_base <- gsub(pattern = "___.+$",
                       replacement = "",
                       x = names(record_data)[i])
    
    
    
    field_text_type <- data_dict$text_validation_type_or_show_slider_number[data_dict$field_name == field_base]
    field_type <- data_dict$field_type[data_dict$field_name == field_base]
    
    #* If the variable isn't in the data data_dictionary (usually it's a field added by REDCap,
    #* such as redcap_event_name, instrument_complete, etc), give it a generic name to
    #* pass to switch.
    if (!length(field_type)) 
    {
      if (field_base %in% paste0(unique(data_dict$form_name),'_complete'))
      {
        field_type <- "form_complete"
      }
      else  
      {
        field_type <- "unrecognized field type"
      }
    }
    # autocomplete was added to the text_validation... column for
    # dropdown menus with the autocomplete feature.
    # field_type[is.na(field_type)] <- 
    #   data_dict$field_type[data_dict$field_name == field_base]
    field_type[field_type == "text" & 
                 !is.na(field_text_type)] <- field_text_type
    
    field_type <- gsub(pattern = "_(dmy|mdy|ymd)$", 
                       replacement = "_",
                       x = field_type)
    
    
    record_data[[i]] <- 
      switch(field_type,
             "date_" = 
               {
                 if (dates) 
                   as.POSIXct(record_data[[i]], format = "%Y-%m-%d") 
                 else 
                   record_data[[i]]
               },
             "datetime_" = 
               {
                 if (dates) 
                   as.POSIXct(record_data[[i]], format = "%Y-%m-%d %H:%M") 
                 else 
                   record_data[[i]]
               },
             "datetime_seconds_" = 
               {
                 if (dates) 
                   as.POSIXct(record_data[[i]], format = "%Y-%m-%d %H:%M:%S") 
                 else 
                   record_data[[i]]
               },
             "time_mm_ss" = 
               {
                 if (dates) 
                   chron::times(ifelse(!is.na(record_data[[i]]), 
                                       paste0("00:", record_data[[i]]), 
                                       record_data[[i]]), 
                                format=c(times="h:m:s"))
                 else 
                   record_data[[i]]
               },
             "time" = 
               {
                 if (dates)
                   chron::times(gsub("(^\\d{2}:\\d{2}$)", "\\1:00", record_data[[i]]), 
                                format=c(times="h:m:s"))
                 else 
                   record_data[[i]]
               },
             "float" = suppressWarnings(as.numeric(record_data[[i]])),
             "number" = suppressWarnings(as.numeric(record_data[[i]])),
             "calc" = suppressWarnings(as.numeric(record_data[[i]])),
             "int" = suppressWarnings(as.integer(record_data[[i]])), # I think this is a legacy option
             "integer" = suppressWarnings(as.integer(record_data[[i]])),
             "select" = 
               makeRedcapFactor(x = record_data[[i]],
                                coding = data_dict$select_choices_or_calculations[data_dict$field_name == field_base],
                                factor_labels = factor_labels, 
                                var_name = data_dict$field_name[data_dict$field_name == field_base]),
             "radio" = 
               makeRedcapFactor(x = record_data[[i]],
                                coding = data_dict$select_choices_or_calculations[data_dict$field_name == field_base],
                                factor_labels = factor_labels, 
                                var_name = data_dict$field_name[data_dict$field_name == field_base]),
             "dropdown" = 
               makeRedcapFactor(x = record_data[[i]],
                                coding = data_dict$select_choices_or_calculations[data_dict$field_name == field_base],
                                factor_labels = factor_labels, 
                                var_name = data_dict$field_name[data_dict$field_name == field_base]),
             "yesno" = makeRedcapFactor(x = record_data[[i]],
                                        coding = '1, Yes | 0, No',
                                        factor_labels = factor_labels, 
                                        var_name = data_dict$field_name[data_dict$field_name == field_base]),
             "truefalse" = 
               {
                 if (factor_labels) 
                   as.logical(record_data[[i]])
                 else
                   makeRedcapFactor(x = record_data[[i]],
                                    coding = '1, TRUE | 0, FALSE',
                                    factor_labels = factor_labels, 
                                    var_name = data_dict$field_name[data_dict$field_name == field_base])
               },
             "checkbox" = 
               {
                 var_name = names(record_data)[i]
                 suffix = gsub("^.+___", "", var_name)
                 
                 if (!suffix == var_name) # Checkbox is in wide-format
                   makeRedcapFactor(x = record_data[[i]],
                                    coding = data_dict$select_choices_or_calculations[data_dict$field_name == field_base],
                                    factor_labels = factor_labels,
                                    var_name = var_name,
                                    checkbox = T,
                                    suffix = suffix)
                 else
                   combined_checkbox_to_factor(x = record_data[[i]],
                                                coding = data_dict$select_choices_or_calculations[data_dict$field_name == field_base],
                                                factor_labels = factor_labels,
                                                var_name = var_name)
               },
             "form_complete" = 
               {
                 makeRedcapFactor(x = record_data[[i]],
                                  coding = "0, Incomplete | 1, Unverified | 2, Complete",
                                  factor_labels, 
                                  var_name = field_base)
               },
             record_data[[i]]
      ) # End switch
  } # End for loop
  record_data
}    

# makeRedcapFactor --------------------------------------------------------

makeRedcapFactor <- function(x, coding, factor_labels, var_name, checkbox = F, suffix = NULL)
{
  if (is.na(coding)){
    warning(sprintf("- No coding available for variable `%s`. Data is left in raw form.\n      This may indicate an problem in the Data data_dictionary.\n", var_name))
    return(x)
  }
  
  # Check if checkbox field condensed is condensed
  if (checkbox) {
    if (identical(suffix,var_name)) suffix = NULL
      
      # If not true it's wide format
      else x[x==0] = NA # Not reinstating 0's will cause import issues
  }
  
  
  coding = parse_field_choices(coding, suffix)
  
  if (nrow(coding) > 0) { # Don't remember why this is here
    
    # Determine data format
    if ( all(unique(na.omit(x)) %in% coding$numbers) ) raw = T else raw = F
    if ( all(unique(na.omit(x)) %in% coding$labels) ) labeled = T else labeled = F
    
    # Set factor codings
    if (factor_labels) labels = coding$labels
      else if (checkbox & labeled & grepl('___\\d+$',var_name)) labels = 1
      else labels = coding$numbers
    if (raw) levels = coding$numbers else if (labeled) levels = coding$labels
    
    
    # Format data
    if (raw | labeled) {
      x <- factor(x, levels=levels, labels=labels)
      attr(x,'redcapLabels') <- coding$labels
      attr(x,'redcapLevels') <- 
        suppressWarnings(tryCatch(as.integer(coding$numbers),
                                  warning = function(cond) coding$numbers))
    }
    else {
      warning(paste0("Invalid factor levels found in variable ",var_name,".
                     Factor labels will not be applied."))
      # Return generic factor
      x <- suppressWarnings(as.factor(x))
    }
  }
  else {
    # Return generic factor if choices can't be parsed
    warning(paste0("Factor choices cannot be parsed from data_dict for variable ",var_name,".
                   Data will be factored as is."))
    x <- suppressWarnings(as.factor(x))
  }
  return(x)
}

# parse_field_choices -----------------------------------------------------

parse_field_choices = function(coding, suffix=NULL) {
  
  # parses the string "0, Birth \\n 1, Death \\n 2, Unknown" into a
  # character vector for creating a factor
  coding <- unlist(strsplit(coding,"[\n|]"))
  if (length(coding) > 0) {
    
    # Structure choices data from data_dict
    coding <- regmatches(coding, regexpr(",", coding), invert = TRUE)
    coding <- do.call("rbind", coding)
    coding <- trimws(coding)
    
    # Apply col names just to make usage more readable
    coding = as.data.frame(coding)
    names(coding) = c('numbers','labels')
    
    if (!is.null(suffix)) { # For wide-format checkbox fields
      # Remove other coding options
      coding <- coding[coding$numbers == suffix,]
      # Change numeric code to 1
      coding$numbers = 1
      }
    }
  return(coding)
}

# combined_checkbox_to_factor --------------------------------------------

combined_checkbox_to_factor = function(x, coding, factor_labels, var_name, checkbox = T) {
  
  # Check if data has previously been formatted and split values as appropriate
  # Note attributes can be dropped easily. It's using and always splitting on commas would be more robust
  if ( 'redcapLabels' %in% names(attributes(x)) ) column_values = stringr::str_split(x, ';') 
    else column_values = stringr::str_split(x, ',')
    
  # convert to df
  column_values = list2df(column_values)
  
  parsed_coding = parse_field_choices(coding)
  # Secondary validation to ensure choices match data_dict
  if ( all(unique(na.omit(column_values)) %in% parsed_coding$numbers) |
       all(unique(na.omit(column_values)) %in% parsed_coding$labels) ) {
    # Pass cols through makeRedcapFactor
    column_values = apply(column_values, 2, function(x) makeRedcapFactor(x, coding, factor_labels, var_name, checkbox)) %>%
      # Paste all options together
          as.data.frame() %>% apply(1, function(x) {paste(na.omit(x),collapse = ';')}) # Using semicolon as it's less common and should be more reliable when string splitting
    
    # The above method adds blanks, convert them back to NAs
    column_values[column_values==''] = NA
    
    # Apply attributes
    attr(column_values,'redcapLabels') <- parsed_coding$labels
    attr(column_values,'redcapLevels') <- 
      suppressWarnings(tryCatch(as.integer(parsed_coding$numbers),
                                warning = function(cond) parsed_coding$numbers))
    return(column_values)
    
  } else {
    warning(paste0(var_name, ' cannot be formatted because the factors do not match the data dictionary.
                   Note that labelling of combined checkbox variables is not fully reversible.'))
    return(x)
  }
}

# list2df -----------------------------------------------------------------

# This function will covert lists with unequal numbers of items into a dataframe
list2df = function(list) {
  
  # Create an appropriately-sized empty matrix
  mat = matrix(NA,
               ncol = max(unlist(lapply(list, function(x) length(x)))),
               nrow = length(list))
  
  # Fill in the matrix using the list
  for (i in 1:length(list)) {
    x = list[[i]]
    for (j in 1:length(x)) {
      mat[i,j] = x[j]
    }
  }
  ## The whole frame can be filtered by keeping as a matrix, using grepl, then converting back to a df.
  
  as.data.frame(mat)
}

# get_column_labels -------------------------------------------------------

#' @name get_column_labels 
#' @title Checkbox Suffixes
#' 
#' @description Checkbox variables return one vector of data for each option defined
#'   in the variable.  The variables are returned with the suffix \code{___[option]}.
#'   \code{rc_export} needs these suffixes in order to retrieve all of the 
#'   variables and to apply the correct labels.
#'   
#' @param field_names The current field names of interest
#' @param data_dict The meta data data frame.

get_column_labels <- function(field_names, data_dict)
{
  name_suffix <- sapply(X = field_names, 
                        FUN = expand_checkbox_names, 
                        data_dict)
  
  label_suffix <- 
    sapply(X = field_names,
           FUN = expand_checkbox_choices,
           data_dict)
  
  labels = unlist(label_suffix)
  names(labels) = unlist(name_suffix)
  
  return(labels)
  
  # list(name_suffix = unlist(name_suffix),
  #      label_suffix = unlist(label_suffix))
}

#***********************************************
#* Unexported methods

#* Get full variable names (appends ___[option] to checkbox fields)
expand_checkbox_names <- function(field_name, data_dict) {
  #* If x is a checkbox variable
  if (data_dict$field_type[data_dict$field_name %in% field_name] == "checkbox"){
    #* Remove characters between "|" and ","; and between "|" and end of string.
    opts <- gsub(pattern = "(?<=,)(.*?)(?=([|]|$))", 
                 replacement = "", 
                 x = data_dict$select_choices_or_calculations[data_dict$field_name %in% field_name], 
                 perl = TRUE)
    #* Split by "|" then remove any commas or spaces
    opts <- strsplit(x = opts, 
                     split = "\\|")[[1]]
    opts <- gsub(pattern = ",| ", 
                 replacement = "", 
                 x = opts)
    #* Assemble labels
    field_name <- c(field_name, paste(field_name, opts, sep="___"))
  }
  return(field_name)
}

#* Get full variable label (appends ": [option label]" for checkboxes)
expand_checkbox_choices <- function(field_name, data_dict) {
  field_index = data_dict$field_name %in% field_name
  #* If field_name is a checkbox variable
  if (data_dict$field_type[field_index] == "checkbox"){
    #* Select choices
    opts <- data_dict$select_choices_or_calculations[field_index]
    #* Remove choice numbers, split, then remove spaces
    opts <- gsub("\\d,", "", opts)
    opts <- strsplit(x = opts,
                     split = "[|]")[[1]]
    opts <- gsub("(^ *| *$)", "", opts)
    #* Assemble labels
    c(data_dict$field_label[field_index], paste0(data_dict$field_label[field_index], ": ", opts))
  }
  else 
  {
    data_dict$field_label[field_index]
  }
}