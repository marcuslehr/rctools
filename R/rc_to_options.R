#' @name rc_to_options
#' @title Upload local bundle or token to options
#' @description a local REDCap bundle object to be uploaded
#' to the R session options.
#' @details 
#'
#' @param bundle A REDCap bundle object
#' @param token Path to a text file containing your REDCap API token
#' @param url Web address to REDCap API server

#' @author Marcus Lehr
#' 
#' @export

rc_to_options <- function(bundle = NULL, token = NULL, url = NULL) {
  
  validate_args(bundle = bundle, token = token, url = url)
  
  # Upload any provided args to options
  if (!is.null(bundle)) 
    options(redcap_bundle = bundle)
  if (!is.null(token)) 
    options(redcap_token = token)
  if (!is.null(url)) 
    options(redcap_url = url)
}
