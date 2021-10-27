#' @name data_frame_to_string
#'
#' @title Converts dataframe to csv string
#' 
#' @param data Dataframe

data_frame_to_string <- function(data) {
  paste0(
    utils::capture.output(
      utils::write.table(data, 
                         sep = ",",
                         col.names = TRUE,
                         row.names = FALSE,
                         na = '')
    ),
    collapse = "\n"
  )
}