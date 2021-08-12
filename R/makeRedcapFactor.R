makeRedcapFactor <- function(x, coding, factor_labels, var_name, checkbox = F, suffix = NULL)
{
  if (is.na(coding)){
    warning(sprintf("- No coding available for variable `%s`. Data is left in raw form.\n      This may indicate an problem in the Data data_dictionary.\n", var_name))
    return(x)
  }
  
  # Check if checkbox field condensed is condensed
  if (checkbox & identical(suffix,var_name)) 
    suffix = NULL
    
    # If not true it's wide format
    else x[x==0] = NA # Not reinstating 0's will cause import issues

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

condensed_checkbox_to_factor = function(x, coding, factor_labels, var_name, checkbox = T) {
  
  # Split on commas for prviously formatted cols
  if (any(grepl(',',x))) x = stringr::str_split(x, ',')
  else x = stringr::str_split(x, '')
  
  # convert to df
  x = list2df(x)
  
  # Pass cols through makeRedcapFactor
  x = apply(x, 1, function(x) makeRedcapFactor(x, coding, factor_labels, var_name, checkbox)) %>%
    # Paste all options together
        as.data.frame() %>% dplyr::summarise_all(~paste(na.omit(.),collapse = ',')) %>% 
        as.character()
  
  # The above method adds blanks. Remove them
  x[x==''] = NA
  
  # Apply attributes
  coding = parse_field_choices(coding)
  attr(x,'redcapLabels') <- coding$labels
  attr(x,'redcapLevels') <- 
    suppressWarnings(tryCatch(as.integer(coding$numbers),
                              warning = function(cond) coding$numbers))
  
  return(x)
}

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
