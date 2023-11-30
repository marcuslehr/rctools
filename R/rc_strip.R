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
#' @param factor_cols Character. Column names to ignore when looking for empty rows.
#'   By default, the id_field and the REDCap event/repeat columns will be used.
#' @param id_field Character. The name of the record_id field for your REDCap project.
#' 
#' 
#' @author Marcus Lehr
#' 
#' @export


rc_strip <- function(record_data,
                     columns = TRUE, 
                     rows = TRUE,
                     factor_cols = NULL,
                     id_field = getOption("redcap_bundle")$id_field) {
  
  validate_args(required = 'record_data',
                record_data = record_data,
                columns = columns, rows = rows,
                factor_cols = factor_cols, id_field = id_field)
  
  if (!(columns|rows)) warning("`columns` and `rows` cannot both be FALSE.
                               No operations performed.")
  
  # Strip empty rows
  if (rows) {
   if (is.null(factor_cols)) {
     id_field = getID(record_data = record_data,
                      id_field = id_field)
    
     factor_cols = intersect(c(id_field,'redcap_event_name','redcap_repeat_instrument','redcap_repeat_instance'), 
                            names(record_data))
   }
    
    # Remove empty rows
    record_data = dplyr::ungroup(record_data) %>% 
                    dplyr::select(-dplyr::all_of(factor_cols)) %>% 
                    apply(., 1, function(x) !all(is.na(x))) %>% 
                    record_data[.,]
    # Reset row index
    rownames(record_data) = NULL
  }
  
  # Strip empty columns
  if (columns) {
    record_data = record_data %>% .[,apply(., 2, function(x) !all(is.na(x)))] 
  }
  return(record_data)
}
