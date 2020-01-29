#' @name rc_format
#'
#' @title Format records data
#' @description  Uses REDCap project metadata to format records data.
#' @details This function takes raw REDCap data and adds column labels, converts
#' columns to numeric/character/factor as appropriate, and applies factor and 
#' checkbox labels.
#' 
#' @param record_data A raw data export from REDCap.
#' @param data_dict REDCap project data data_dictionary. By default, a 
#' REDCap bundle object, as created by \code{rc_setup}, will be looked for.
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
#' @param fields A character vector of fields to be returned.  If \code{NULL}, 
#'   all fields are returned.
#' @param forms A character vector of forms to be returned.  If \code{NULL}, 
#'   all forms are returned.
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
#' @export

rc_format <- function(record_data, data_dict = getOption("redcap_bundle")$data_dict, 
                              factors = TRUE, labels = TRUE, dates = TRUE,
                              checkboxLabels = FALSE, fields = NULL, forms=NULL,
                              ...)
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
  
  massert(~ fields + forms,
          fun = checkmate::assert_character,
          fixed = list(null.ok = TRUE,
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
  
  #* Check that all fields exist in the meta data
  if (!is.null(fields)) 
  {
    bad_fields <- fields[!fields %in% data_dict$field_name]
    if (length(bad_fields))
      coll$push(paste0("The following are not valid field names: ",
                       paste0(bad_fields, collapse = ", ")))
  }
  
  #* Check that all form names exist in the meta data
  if (!is.null(forms))
  {
    bad_forms <- forms[!forms %in% data_dict$form_name]
    if (length(bad_forms))
      coll$push(paste0("The following are not valid form names: ",
                       paste0(bad_forms, collapse = ", ")))
  }
  
  checkmate::reportAssertions(coll)
  
  
  record_data <- fieldToVar(records = record_data, 
                            data_dict = data_dict, 
                            factors = factors, 
                            dates = dates, 
                            checkboxLabels = checkboxLabels)
  
  redcap_fields = c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")
  
  if (labels){
  
    #* Create the vector of field names
    if (!is.null(fields)) #* fields were provided
    {
      # Redcap fields must be removed to prevent errors in checkbox_suffixes()
      field_names <- fields[!fields %in% redcap_fields] 
    }
    else if (!is.null(forms))
    {
      field_names <- data_dict$field_name[data_dict$form_name %in% forms]
    }
    else
      #* fields were not provided, default to all fields in record data.
      field_names <- data_dict$field_name[data_dict$field_name %in% names(record_data)]
    
    #* Expand 'field_names' to include fields from specified forms.    
    if (!is.null(forms)) 
    {
      field_names <- 
        unique(c(field_names, 
                 data_dict$field_name[data_dict$form_name %in% forms]))
    }
    
    suffixed <- checkbox_suffixes(fields = field_names,
                                  data_dict = data_dict)
    
    record_data[suffixed$name_suffix] <-
      mapply(nm = suffixed$name_suffix,
             lab = suffixed$label_suffix,
             FUN = function(nm, lab){
               labelVector::set_label(record_data[[nm]], lab)
             },
             SIMPLIFY = FALSE)
  }
  
  # Add Redcap fields back in to prevent removal during final subsetting
  field_names = c(field_names[1],
                redcap_fields[redcap_fields %in% names(record_data)],
                field_names[2:length(field_names)])
  
  record_data[field_names]
}
