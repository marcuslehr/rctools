#' @name fieldToVar
#' @importFrom chron times
#' 
#' @title Convert a REDCap Data Field to an R Vector
#' @description Converts a field exported from REDCap into a valid R vector
#' 
#' @param records A data frame of records returned by \code{rc_export} 
#' @param data_dict A data frame giving the data data_dictionary, as returned 
#'   by \code{exportMetaData}
#' @param factor_labels Logical, determines if checkbox, radio button, dropdown and yesno
#'   variables are labelled.
#' @param dates Logical, determines if date variables are converted to POSIXct format
#' 
#' @details This function is called internally by \code{rc_export}. It is not 
#' available to the user.
#'   
#' @author Jeffrey Horner

fieldToVar <- function(records, data_dict, factor_labels = TRUE, 
                       dates = TRUE)
{ 
  for (i in seq_along(records))
  {
    field_base <- gsub(pattern = "___.+$",
                       replacement = "",
                       x = names(records)[i])
    
    
    
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

    
    records[[i]] <- 
      switch(field_type,
             "date_" = 
               {
                 if (dates) 
                   as.POSIXct(records[[i]], format = "%Y-%m-%d") 
                 else 
                   records[[i]]
                },
             "datetime_" = 
               {
                 if (dates) 
                   as.POSIXct(records[[i]], format = "%Y-%m-%d %H:%M") 
                 else 
                   records[[i]]
               },
             "datetime_seconds_" = 
               {
                 if (dates) 
                   as.POSIXct(records[[i]], format = "%Y-%m-%d %H:%M:%S") 
                 else 
                   records[[i]]
               },
             "time_mm_ss" = 
               {
                 if (dates) 
                   chron::times(ifelse(!is.na(records[[i]]), 
                                       paste0("00:", records[[i]]), 
                                       records[[i]]), 
                                format=c(times="h:m:s"))
                 else 
                   records[[i]]
               },
             "time" = 
               {
                 if (dates)
                   chron::times(gsub("(^\\d{2}:\\d{2}$)", "\\1:00", records[[i]]), 
                                format=c(times="h:m:s"))
                 else 
                   records[[i]]
               },
             "float" = suppressWarnings(as.numeric(records[[i]])),
             "number" = suppressWarnings(as.numeric(records[[i]])),
             "calc" = suppressWarnings(as.numeric(records[[i]])),
             "int" = suppressWarnings(as.integer(records[[i]])), # I think this is a legacy option
             "integer" = suppressWarnings(as.integer(records[[i]])),
             "select" = 
               makeRedcapFactor(x = records[[i]],
                                coding = data_dict$select_choices_or_calculations[data_dict$field_name == field_base],
                                factor_labels = factor_labels, 
                                var_name = data_dict$field_name[data_dict$field_name == field_base]),
             "radio" = 
               makeRedcapFactor(x = records[[i]],
                                coding = data_dict$select_choices_or_calculations[data_dict$field_name == field_base],
                                factor_labels = factor_labels, 
                                var_name = data_dict$field_name[data_dict$field_name == field_base]),
             "dropdown" = 
               makeRedcapFactor(x = records[[i]],
                                coding = data_dict$select_choices_or_calculations[data_dict$field_name == field_base],
                                factor_labels = factor_labels, 
                                var_name = data_dict$field_name[data_dict$field_name == field_base]),
             "yesno" = makeRedcapFactor(x = records[[i]],
                                        coding = '1, Yes | 0, No',
                                        factor_labels = factor_labels, 
                                        var_name = data_dict$field_name[data_dict$field_name == field_base]),
             "truefalse" = 
              {
                if (factor_labels) 
                  as.logical(records[[i]])
                else
                  makeRedcapFactor(x = records[[i]],
                                   coding = '1, TRUE | 0, FALSE',
                                   factor_labels = factor_labels, 
                                   var_name = data_dict$field_name[data_dict$field_name == field_base])
              },
             "checkbox" = 
              {
                var_name = names(records)[i]
                suffix = gsub("^.+___", "", var_name)
                
                if (!suffix == var_name) # Checkbox is in wide-format
                  makeRedcapFactor(x = records[[i]],
                                   coding = data_dict$select_choices_or_calculations[data_dict$field_name == field_base],
                                   factor_labels = factor_labels,
                                   var_name = var_name,
                                   checkbox = T,
                                   suffix = suffix)
                else
                  condensed_checkbox_to_factor(x = records[[i]],
                                               coding = data_dict$select_choices_or_calculations[data_dict$field_name == field_base],
                                               factor_labels = factor_labels,
                                               var_name = var_name)
              },
             "form_complete" = 
             {
               makeRedcapFactor(x = records[[i]],
                                coding = "0, Incomplete | 1, Unverified | 2, Complete",
                                factor_labels, 
                                var_name = data_dict$field_name[data_dict$field_name == field_base])
             },
             records[[i]]
      ) # End switch
  } # End for loop
  records
}    



  

    
