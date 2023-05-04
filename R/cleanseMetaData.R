#' @name cleanseMetaData
#' @title Clean Meta Data of UTF Characters
#' 
#' @description There have been isolated cases observed where certain 
#'   characters in the data data_dictionary prevent it from being downloaded
#'   correctly.  In one case, the data data_dictionary could not be downloaded
#'   at all through the API.  It is suspected that these problematic 
#'   characters are a result of copying and pasting text out of word 
#'   processing programs.  The problematic characters are not necessarily 
#'   visible and their exact location can be difficult to identify.  As 
#'   a last resort, \code{cleanseMetaData} can read a meta data file 
#'   downloaded through the user interface, purge it of any UTF-8 characters,
#'   and write an alternate data data_dictionary that contains only ASCII 
#'   characters.  
#'   
#' @param data_dict_file \code{character(1)} the path to a meta data file 
#'   that has been downloaded using the REDCap user interface.
#' @param data_dict_clean \code{character(1)} the path of the file to which
#'   the cleaned meta data will be written.
#' @param overwrite \code{logical(1)} Permit the new file to overwrite an 
#'   existing file
#'
#' @author Benjamin Nutter
#' @export

cleanseMetaData <- function(data_dict_file, data_dict_clean,
                            overwrite = FALSE)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = data_dict_file,
                              len = 1,
                              add = coll)
  
  checkmate::assert_character(x = data_dict_clean,
                              len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (!file.exists(data_dict_file)){
    coll$push(sprintf("File not found: %s", data_dict_file))
  }
  
  if (file.exists(data_dict_clean) && !overwrite){
    coll$push(sprintf("File exists and overwrite is set to FALSE: %s", 
                      data_dict_clean))
  }
  
  if (data_dict_clean == data_dict_file){
    coll$push(sprintf("%s %s", 
                      "Sorry. I won't allow you to overwrite your file. ",
                      "Please use a different path for 'data_dict_clean."))
  }
  
  checkmate::reportAssertions(coll)
  
  if (file.exists(data_dict_clean) && overwrite){
    warning("Attempting to overwrite ", data_dict_clean)
  }
  
  dd <- readLines(data_dict_file)
  dd <- paste0(dd, collapse = "\n")
  dd <- iconv(dd, 
              from = "utf8", 
              to = "ASCII", 
              sub = "")
  
  dd <- utils::read.csv(text = dd, 
                        stringsAsFactors = FALSE)
  
  utils::write.csv(dd, 
                   data_dict_clean, 
                   row.names = FALSE, 
                   na = "")
}
