#' @name rc_fill
#'
#' @title Fills in empty rows for categorical data
#' @description  Takes a data point which only occurs in a single event (row)
#' and copies that data to all rows for each participant (or other grouping
#' variable assigned by \code{group_by}). This function is particularly useful
#' for making filtering operations more straight forward. 
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
  
  
  if(is.null(group_by)) group_by = getID(record_data = record_data)
  
  validate_args(required = c('record_data'),
                record_data = record_data,
                group_by = group_by)
  
  
  # Test for columns that contain more than one value per group
  cols = c(...)
  counts = list()
  for (col in cols) {
    # Replace blanks with NA
    record_data[record_data[[col]] == '',col] = NA
    
    # Count items per group in each column
    x = suppressMessages(
            dplyr::group_by(record_data, !!dplyr::sym(group_by)) %>% dplyr::select(all_of(col)) %>% 
            na.omit() %>% dplyr::summarise(n = dplyr::n())
        )
    counts[[col]] = max(x$n)
  }
  
  # Warn of any groups that contain >1 value
  counts = as.data.frame(counts)
  if (any(counts > 1)) {
    stop("Fields cannot contain more than one value per group. Please resolve conflicts in the following
         fields before filling:\n",paste0(names(counts[which(counts > 1)]), collapse = ', '))
  }
  
  # Fill columns
  dplyr::group_by_(record_data, paste(group_by, collapse = ',')) %>%
                  tidyr::fill(., ..., .direction = "downup") %>%
									dplyr::ungroup()
}
