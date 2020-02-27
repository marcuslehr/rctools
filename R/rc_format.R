#' @name rc_format
#'
#' @title Format records data
#' @description  Uses REDCap project metadata to format records data.
#' @details This function takes raw REDCap data and adds column labels, converts
#' columns to numeric/character/factor as appropriate, and applies factor and 
#' checkbox labels.
#' 
#' @param record_data Dataframe. Record data export from REDCap
#' @param data_dict Dataframe. REDCap project data data_dictionary. By default, a 
#' REDCap bundle option, as created by \code{rc_setup}, will be looked for.
#' Otherwise, a data.frame containing the metadata must be supplied.
#' @param completionField The REDCap variable which indicates whether or not a subject
#' has completed the study. This should be indicated by a 'Yes' or a '1' (i.e. a Yes/No
#' field in REDCap).
#' 
#' @param factors Logical.  Determines if categorical data from the database is 
#'   returned as numeric codes or labelled factors. See 'Checkbox Variables'
#'   for more on how this interacts with the \code{checkboxLabels} argument.
#' @param labels Logical.  Determines if the variable labels are applied to 
#'   the data frame.
#' @param dates Logical. Determines if date variables are converted to POSIXct 
#'   format during the download.
#' @param checkboxLabels Logical. Determines the format of labels in checkbox 
#'   variables.  If \code{FALSE} labels are applies as "Unchecked"/"Checked".  
#'   If \code{TRUE}, they are applied as ""/"[field_label]" where [field_label] 
#'   is the label assigned to the level in the data data_dictionary. 
#'   This option is only available after REDCap version 6.0.  See Checkbox Variables
#'   for more on how this interacts with the \code{factors} argument.
#'   
#' @section Checkbox Variables:
#' 
#' There are four ways the data from checkbox variables may be 
#' represented depending on the values of \code{factors} and 
#' \code{checkboxLabels}. The most common are the first and third 
#' rows of the table below.  When \code{checkboxLabels = TRUE}, either 
#' the coded value or the labelled value is returned if the box is 
#' checked, or an empty string if it is not.
#' 
#' \tabular{lll}{
#' \code{factors} \tab \code{checkboxLabels} \tab Output \cr
#' \code{FALSE}   \tab \code{FALSE}          \tab 0 / 1 \cr
#' \code{FALSE}   \tab \code{TRUE}           \tab "" / value \cr
#' \code{TRUE}    \tab \code{FALSE}          \tab Unchecked / Checked \cr
#' \code{TRUE}    \tab \code{TRUE}           \tab "" / label 
#' }
#' 
#' @importFrom magrittr '%>%'
#'
#' @author Benjamin Nutter
#' @author Marcus Lehr
#'
#' @export

rc_format <- function(record_data, data_dict = getOption("redcap_bundle")$data_dict, 
                      factors = TRUE, labels = TRUE, dates = TRUE,
                      checkboxLabels = FALSE)
{
  
  validate_args(required = c('record_data','data_dict'),
                record_data = record_data, data_dict = data_dict,
                factors = factors, labels = labels, dates = dates,
                checkboxLabels = checkboxLabels)

  
  #* for purposes of the export, we don't need the descriptive fields. 
  #* Including them causes errors in checkbox_suffixes
  data_dict <- data_dict[!data_dict$field_type %in% "descriptive",]
  
  
  record_data <- fieldToVar(records = record_data, 
                            data_dict = data_dict, 
                            factors = factors, 
                            dates = dates, 
                            checkboxLabels = checkboxLabels)
  
  if (labels){
    
    # Get field names
    fields = names(record_data)[names(record_data) %in% data_dict$field_name]
    
    # Currently generating labels for all fields
    col_labels = checkbox_suffixes(field_names = data_dict$field_name,
                                   data_dict = data_dict)
    
    # Apply column labels
    Hmisc::label(record_data) = as.list(col_labels[match(names(record_data),names(col_labels))])
    
    ## Deprecated labelling method
    # record_data[fields] <-
    #   mapply(nm = fields,
    #          lab = col_labels[fields],
    #          FUN = function(nm, lab){
    #            labelVector::set_label(record_data[[nm]], lab)
    #          },
    #          SIMPLIFY = FALSE)
  }
  
  record_data
}