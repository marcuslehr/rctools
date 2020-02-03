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
  
  if (is.null(data_dict)) 
    stop("Project metadata must be supplied. Please create a bundle object
       with rc_setup() or supply the metadata via a data.frame")
  
  #* Error Collection Object
  coll <- checkmate::makeAssertCollection()
  
  massert(~ factors + labels + dates + checkboxLabels,
          fun = checkmate::assert_logical,
          fixed = list(len = 1,
                       add = coll))
  
  massert(~ record_data + data_dict,
          fun = checkmate::assert_class,
          fixed = list(classes = "data.frame",
                       add = coll))
  
  checkmate::reportAssertions(coll)
  
  
  if (length(data_dict) != 18) warning("data_dict has an unexpected number of columns. Please supply metadata 
                                       exactly as produced by REDCap")
  
  col.names=c('field_name', 'form_name', 'section_header', 
              'field_type', 'field_label', 'select_choices_or_calculations', 
              'field_note', 'text_validation_type_or_show_slider_number', 
              'text_validation_min', 'text_validation_max', 'identifier', 
              'branching_logic', 'required_field', 'custom_alignment', 
              'question_number', 'matrix_group_name', 'matrix_ranking',
              'field_annotation')
  
  names(data_dict) <- col.names[1:length(col.names)]
  
  #* for purposes of the export, we don't need the descriptive fields. 
  #* Including them makes the process more error prone, so we'll ignore them.
  data_dict <- data_dict[!data_dict$field_type %in% "descriptive", ]  
  
  
  checkmate::reportAssertions(coll)
  
  
  record_data <- fieldToVar(records = record_data, 
                            data_dict = data_dict, 
                            factors = factors, 
                            dates = dates, 
                            checkboxLabels = checkboxLabels)
  
  if (labels){
    
    # Get field names
    field_names = names(record_data)
    
    # Currently generating labels for all fields
    col_labels = checkbox_suffixes(field_names = data_dict$field_name,
                                 data_dict = data_dict)
    
    # Apply column labels
    record_data[field_names] <-
      mapply(nm = field_names,
             lab = col_labels[field_names],
             FUN = function(nm, lab){
               labelVector::set_label(record_data[[nm]], lab)
             },
             SIMPLIFY = FALSE)
  }
  
  record_data
}