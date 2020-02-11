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
  
  # Test for columns that contain more than one value per group
  cols = c(...)
  counts = list()
  for (col in cols) {
    x = suppressWarnings(
            dplyr::group_by_(record_data, group_by) %>% dplyr::select(all_of(col)) %>% 
            na.omit() %>% dplyr::summarise(n = dplyr::n())
    )
    counts[[col]] = max(x$n)
  }
  
  # Warn of any groups that contain >1 value
  counts = as.data.frame(counts)
  if (any(counts > 1)) {
    warning("The following fields contain more than one value per group. Verify that filling is appropriate.")
    print(names(counts[which(counts == max(counts))]))
  }
  
  # Fill columns
  dplyr::group_by_(record_data, paste(group_by, collapse = ',')) %>%
                  tidyr::fill(., ..., .direction = "downup")
}
