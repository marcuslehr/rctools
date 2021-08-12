#' @name rc_strip
#'
#' @title Remove empty rows and columns from a REDCap dataframe
#' @description Removes any columns which contain only \code{NA}. Likewise,
#' rows are removed in a similar fashion but any REDCap factor fields
#' (record_id, redcap event/repeat) are ignored.
#' 
#' @param record_data Dataframe. Record data exported from REDCap
#' @param columns Logical. When \code{TRUE}, empty columns will
#' be removed.
#' @param rows Logical. When \code{TRUE}, empty rows will be removed.
#' @param id_field Character. The name of the record_id field for your REDCap project.
#' 
#' @importFrom magrittr '%>%'
#' 
#' @author Marcus Lehr
#' 
#' @export


rc_strip <- function(record_data,
                     columns = TRUE, 
                     rows = TRUE,
                     id_field = getOption("redcap_bundle")$id_field) {
  
  validate_args(required = 'record_data',
                record_data = record_data,
                columns = columns, rows = rows,
                id_field = id_field)
  
  if (!(columns|rows)) warning("`columns` and `rows` cannot both be FALSE.
                               No operations performed.")
  
  if (columns) {
    # Remove empty columns
    record_data = record_data %>% .[,apply(., 2, function(x) !all(is.na(x)))] 
  }
  
  if (rows) {
   id_field = getID(record_data = record_data,
                   id_field = id_field)
    
    rc_factors = intersect(c(id_field,'redcap_event_name','redcap_repeat_instrument','redcap_repeat_instance'), 
                         names(record_data))
    # Remove empty rows
    record_data = dplyr::select(record_data, -all_of(rc_factors)) %>% 
                    apply(., 1, function(x) !all(is.na(x))) %>% 
                    record_data[.,]
    # Reset row index
    rownames(record_data) = NULL
  }
  return(record_data)
}