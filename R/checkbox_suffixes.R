#' @name checkbox_suffixes 
#' @title Checkbox Suffixes
#' 
#' @description Checkbox variables return one vector of data for each option defined
#'   in the variable.  The variables are returned with the suffix \code{___[option]}.
#'   \code{rc_export} needs these suffixes in order to retrieve all of the 
#'   variables and to apply the correct labels.
#'   
#' @param field_names The current field names of interest
#' @param data_dict The meta data data frame.

checkbox_suffixes <- function(field_names, data_dict)
{
  name_suffix <- sapply(X = field_names, 
                        FUN = manual_checkbox_suffixes, 
                        data_dict)

  label_suffix <- 
    sapply(X = field_names,
           FUN = manual_checkbox_label_suffixes,
           data_dict)
  
  labels = unlist(label_suffix)
  names(labels) = unlist(name_suffix)
  
  labels
  
  # list(name_suffix = unlist(name_suffix),
  #      label_suffix = unlist(label_suffix))
}

#***********************************************
#* Unexported methods

#* Get full variable names (appends ___[option] to checkboxes)
manual_checkbox_suffixes <- function(x, data_dict) {
  #* If x is a checkbox variable
  if (data_dict$field_type[data_dict$field_name %in% x] == "checkbox"){
    #* Remove characters between "|" and ","; and between "|" and end of string.
    opts <- gsub(pattern = "(?<=,)(.*?)(?=([|]|$))", 
                 replacement = "", 
                 x = data_dict$select_choices_or_calculations[data_dict$field_name %in% x], 
                 perl = TRUE)
    #* Split by "|" then remove any commas or spaces
    opts <- strsplit(x = opts, 
                     split = "\\|")[[1]]
    opts <- gsub(pattern = ",| ", 
                 replacement = "", 
                 x = opts)
    #* Assemble labels
    x <- paste(x, opts, sep="___")
  }
  x
}

#* Get full variable label (appends ": [option label]" for checkboxes)
manual_checkbox_label_suffixes <- function(x, data_dict) {
  #* If x is a checkbox variable
  if (data_dict$field_type[data_dict$field_name %in% x] == "checkbox"){
    #* Select choices
    opts <- data_dict$select_choices_or_calculations[data_dict$field_name %in% x]
    #* Remove choice numbers, split, then remove spaces
    opts <- gsub("\\d,", "", opts)
    opts <- strsplit(x = opts,
                     split = "[|]")[[1]]
    opts <- gsub("(^ *| *$)", "", opts)
    #* Assemble labels
    paste0(data_dict$field_label[data_dict$field_name %in% x], ": ", opts)
  }
  else 
  {
    data_dict$field_label[data_dict$field_name %in% x]
  }
}