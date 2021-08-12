#' @name rc_to_options
#' @title Upload local bundle or token to options
#' @description Uploads a local REDCap bundle object to the R session options.
#'
#' @param bundle A REDCap bundle object
#' @param token Path to a text file containing your REDCap API token

#' @author Marcus Lehr
#' 
#' @export

rc_to_options <- function(bundle = NULL, token = NULL) {
  
  # Checks
  validate_args(bundle = bundle, token = token)
  
  # Upload any provided args to options
  if (!is.null(bundle)) 
    options(redcap_bundle = bundle)
  if (!is.null(token)) 
    options(redcap_token = token)
}
