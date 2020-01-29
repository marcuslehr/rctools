#' @name exportMetaData
#' 
#' @title Export Meta Data from a REDCap Database
#' @description Retrieves the meta data for a REDcap database, including 
#' field names, labels, types, formulas, etc.  This file can be used to parse 
#' levels of factors, apply labels, and other data management tasks once the 
#' data are retrieved
#' 
#' @param url A url address to connect to the REDCap API
#' @param token A REDCap API token
#' @param fields A character vector of field names for which the metadata is to 
#'   be retrieved.  
#' @param forms A character vector of forms for which the metadata is to be
#'   retrieved. Note that if a form name is given, all of the fields on that form
#'   will be returned, regardless of whether it is included in \code{fields} or 
#'   not.  Be careful to use the form names in the second column of the data 
#'   data_dictionary, and not the display names shown on the webpage.
#' @param ... Arguments to be passed to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param drop_utf8 \code{logical(1)}. In some cases, UTF-8 characters can 
#'   pose problems for exporting the data data_dictionary.  Set this to \code{TRUE}
#'   to replace any UTF-8 characters with empty characters.
#' 
#' @details A record of this export is placed in the REDCap logging page, 
#' but the file that is exported is not stored in the database.
#' 
#' @section REDCap API Documentation:
#' This function allows you to export the metadata for a project
#' 
#' @section REDCap Version:
#' 5.8.2+ (and earlier, but we don't know how much earlier)
#' 
#' @section Known REDCap Limitations: 
#' The API doesn't respond to the \code{fields} and \code{forms} arguments.  It
#' always returns the full data data_dictionary.
#' 
#' @author Jeffrey Horner
#' 
#' @references
#' This functionality was originally developed by Jeffrey Horner in the \code{redcap} package.
#' \url{https://github.com/vubiostat/redcap}
#' 
#' Please refer to your institution's API documentation.
#' 
#' Additional details on API parameters are found on the package wiki at
#' \url{https://github.com/nutterb/redcapAPI/wiki/REDCap-API-Parameters}


exportMetaData <- function(url = getOption("redcap_bundle")$redcap_url,
                           token = getOption("redcap_token"),
                           fields=NULL, forms=NULL,
                           error_handling = getOption("redcap_error_handling"), 
                           ..., drop_utf8 = FALSE)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = url,
                          add = coll)

  checkmate::assert_character(x = token,
                          add = coll)
  
  massert(~ fields + forms,
          fun = checkmate::assert_character,
          fixed = list(null.ok = TRUE,
                       add = coll))
  
  checkmate::assert_logical(x = drop_utf8,
                            len = 1,
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  body <- list(token = token,
               content = "metadata",
               format = "csv",
               returnFormat = "csv")

  if (!is.null(fields)) body[['fields']] <- fields
  if (!is.null(forms)) body[['forms']] <- forms
 
  x <- httr::POST(url = url, 
                  body = body)

  if (x$status_code != 200) return(redcap_error(x, error_handling))
  
  x <- as.character(x)
  if (drop_utf8)
  {
    x <- iconv(x, "utf8", "ASCII", sub = "")
  }
  utils::read.csv(text = x, 
                  stringsAsFactors = FALSE, 
                  na.strings = "")
}
