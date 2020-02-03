#' @name rc_fill
#'
#' @title Fills in empty rows for categorical data
#' @description  Takes a data point which only occurs in a single event (row)
#' and copies that data to all rows for each participant. This is useful for 
#' filtering data. 
#' 
#' @param record_data Dataframe. Record data exported from REDCap
#' @param ... Variables to be filled. Variable names can be unquoted or quoted. 
#' @param group_by Variable(s) to group data by. The project's record_id field
#' is used by default. 
#' 
#' @importFrom magrittr '%>%'
#' 
#' @author Marcus Lehr
#' 
#' @export

rc_fill <- function(record_data, ..., 
                    group_by = getOption("redcap_bundle")$id_field) {
  
  #* Error Collection Object
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = record_data, 
                          classes = 'data.frame', 
                          add = coll)
  
  checkmate::assert_character(x = group_by,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
	
  dplyr::group_by_(record_data, paste(group_by, collapse = ',')) %>% 
                  tidyr::fill(., ..., .direction = "downup")
}

