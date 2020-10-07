#' @name rc_strip
#'
#' @title Remove empty rows and columns from a REDCap dataframe
#' @description Removes any columns which contain only \code{NA}. Likewise,
#' rows are removed in a similar fashion but any REDCap factor fields
#' (record_id, redcap event/repeat) are ignored.
#' 
#' @param record_data Dataframe. Record data exported from REDCap
#' @param only_rows Logical. When \code{TRUE}, only empty rows will
#' be removed.
#' @param only_columns Logical. When \code{TRUE}, only empty columns
#' will be removed.
#' 
#' @importFrom magrittr '%>%'
#' 
#' @author Marcus Lehr
#' 
#' @export


rc_strip <- function(record_data,
                     only_rows = FALSE, 
                     only_columns = FALSE,
                     id_field = getOption("redcap_bundle")$id_field) {
  
  validate_args(required = 'record_data',
                only_rows = only_rows, only_columns = only_columns,
                id_field = id_field)
  
  if (only_rows & only_columns) stop("`only_rows` and `only_columns` cannot both be TRUE.")
  
  if (!only_rows) {
    # Remove empty columns
    record_data = record_data %>% .[,apply(., 2, function(x) !all(is.na(x)))] 
  }
  
  if (!only_columns) {
   id_field = getID(record_data = record_data,
                   id_field = id_field)
    
    rc_factors = intersect(c(id_field,'redcap_event_name','redcap_repeat_instrument','redcap_repeat_instance'), 
                         names(record_data))
    # Remove empty rows
    record_data = dplyr::select(record_data, -all_of(rc_factors)) %>% 
                    apply(., 1, function(x) !all(is.na(x))) %>% 
                    record_data[.,] 
  }
  return(record_data)
}