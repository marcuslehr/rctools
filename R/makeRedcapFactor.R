makeRedcapFactor <- function(x, coding, factors, var_name)
{
  if (is.na(coding)){
    warning(sprintf("- No coding available for variable `%s`. Data is left in raw form.\n      This may indicate an problem in the Data data_dictionary.\n", var_name))
    return(x)
  }
  # parses the string "0, Birth \\n 1, Death \\n 2, Unknown" into a
  # character vector for creating a factor
  coding <- unlist(strsplit(coding,"[\n|]"))
  if (length(coding) > 0) {
    
    # Structure choices data from data_dict
    coding <- regmatches(coding, regexpr(",", coding), invert = TRUE)
    coding <- do.call("rbind", coding)
    coding <- trimws(coding)
    
    # Determine data format
    if ( all(unique(na.omit(x)) %in% coding[,1]) ) raw = T else raw = F
    if ( all(unique(na.omit(x)) %in% coding[,2]) ) labeled = T else labeled = F
    
    # Set factor codings
    if (factors) labels = coding[,2] else labels = coding[,1]
    if (raw) levels = coding[,1] else if (labeled) levels = coding[,2] 
    
    
    if (raw | labeled) {
      x <- factor(x, levels=levels, labels=labels)
      class(x) <- c("redcapFactor", "factor")
      attr(x,'redcapLabels') <- coding[, 2]
      attr(x,'redcapLevels') <- 
        suppressWarnings(tryCatch(as.integer(coding[, 1]),
                                  warning = function(cond) coding[, 1]))
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
  x
}

# makeRedcapYN <- function(x, factors)
# {
#   if (factors)
#     x <- factor(x, 0:1, c("No", "Yes"))
#   
#   class(x) <- c("redcapFactor", class(x))
#   attr(x,'redcapLabels') <- c("No", "Yes")
#   attr(x,'redcapLevels') <- 0:1
#   x
# }

makeRedcapCheckbox <- function(x, suffix, coding, factors, checkboxLabels)
{
  # parses the string "0, Birth \\n 1, Death \\n 2, Unknown" into a
  # character vector for creating a factor
  coding <- unlist(strsplit(coding,"[\n|]"))
  if (length(coding) > 0) 
  {
    coding <- regmatches(coding, regexpr(",", coding), invert = TRUE)
    coding <- do.call("rbind", coding)
    coding <- trimws(coding)
    coding <- coding[coding[, 1] == suffix, ]
    
    
    use_labels <-
      if (!factors && !checkboxLabels)
        c("0", "1")
      else if (!factors && checkboxLabels)
        c("", coding[1])
      else if (factors && !checkboxLabels)
        c("Unchecked", "Checked")
      else if (factors && checkboxLabels) 
        c("", coding[2])
      
    
    if (!factors){
      if (checkboxLabels)
        x <- use_labels[x+1]
      # no else needed.  If checkboxLabels = FALSE, leave as 0/1
      
      class(x) <- c("redcapFactor", class(x))
    }
    else {
      if (!checkboxLabels)
        x <- factor(x,
                    levels = 0:1,
                    labels = c("Unchecked", "Checked"))
      else 
        x <- factor(x, 
                    levels = 0:1,
                    labels = use_labels)
      
      class(x) <- c("redcapFactor", "factor")
    }
    
    attr(x,'redcapLabels') <- use_labels
    attr(x,'redcapLevels') <- 0:1
    
  }
  else 
  {
    # Create integer since the meta data about choices are bungled.
    x <- suppressWarnings(as.integer(x))
  }
  x
}

