#' @name getID
#'
#' @title Retrieve record_id field name
#' @description  Gets record_id field name from bundle, or data_dict 
#' if provided. Otherwise, the first column name will be used. This 
#' is an internal rctools function only.
#' 
#' @param record_data Dataframe. Record data exported from REDCap
#' @param data_dict Dataframe. REDCap project data data_dictionary
#' @param id_field Character. Field name corresponding to the 'record_id' field.
#' 
#' @author Marcus Lehr

getID <- function(record_data = NULL, 
                  data_dict = getOption("redcap_bundle")$data_dict, 
                  id_field = getOption("redcap_bundle")$id_field) {
  
  if (!is.null(id_field)) return(id_field)
  
  else if (!is.null(data_dict)) {
    # Grab record_id field name
    id_field = data_dict$field_name[1]
  }
  else if (!is.null(record_data) & any(names(record_data)=='record_id')) 
    id_field = 'record_id'
  
  else if (!is.null(record_data)) {
    warning("'record_id' field could not be found. It will be assumed to be the first column.")
    id_field = names(record_data)[1]
	}
	else stop("'record_id' field could not be found.")
  
  # Update bundle
  bundle = getOption('redcap_bundle')
  bundle$id_field = id_field
  options(redcap_bundle = bundle)
  return(id_field)
}
