#' @name rc_bundle
#' @title Export REDCap project metadata
#' @description This function performs several API calls at once in order
#' to reduce the overall number of calls when using rctools. All project
#' metadata is exported and saved to a REDCap bundle option and/or object
#' for use with other rctools functions.
#' @details By default, this function exports all metadata. If any of the
#' metadata arguments are set to \code{TRUE}, then only the indicated data
#' will be exported. Additionally, by default the exported data will be
#' saved to the option "redcap_bundle", and other rctools functions will check
#' this option for metadata by default. The bundle can be accessed via 
#' \code{getOption("redcap_bundle")}. It will also be returned as
#' an object (by default) for ease of access. Saving the bundle object as an
#' RDS (see saveRDS()) or within your workspace image (see save.image()) is
#' useful to negate the need for additional API calls in future sessions.
#' 
#' For security purposes, the REDCap token will be saved only to the option
#' "redcap_token". This option, along with the "redcap_bundle" option, will
#' not persist across R sessions. Unlike the REDCap bundle data, saving a 
#' token within an object or workspace is not recommended. 
#'   
#' @param url A url address to connect to the REDCap API
#' @param token A REDCap API token
#' @param bundle A REDCap bundle object. If provided, the local bundle will be
#' uploaded to the R session options for use by other rctools functions.
#' @param create_option Logical. Indicates whether the REDCap bundle should be
#' saved to an option.
#' @param return_object Logical. Indicates whether the REDCap bundle should be
#' returned as an object.
#' @param data_dict Logical.  Indicates if the meta data (data data_dictionary) 
#'   should be exported.
#' @param users Logical. Indicates if the users table should be exported.
#' @param instruments Logical. Indicates if the instruments table should be exported.
#' @param event_data Logical. Indicates if the event names should be exported.
#' @param arms Logical. Indicates if the arms table should be exported.
#' @param mappings Logical. Indicates if the form-event mappings should 
#'   be exported.
#' @param version Logical. Indicates if the REDCap version number should be exported.  
#'   Only applicable in REDCap 6.0.0 and higher.
#' @param proj_info Logical. Indicates if the project information should be exported.
#' If the project is not longitudinal, the events, arms, and event-form mappings 
#' elements will be assigned character vectors instead of data frames.
#' @param dates Logical. If \code{TRUE} (Default), user expiration dates are converted to 
#'   \code{POSIXct} objects.
#' @param labels Logical.  If \code{TRUE} (Default), the user form permissions are 
#'   converted to labelled factors.
#'   
#' @author Marcus Lehr
#' @author Benjamin Nutter
#' 
#' @export

rc_bundle <- function(url,token, bundle = NULL,
                      create_option=TRUE, return_object=TRUE,
                      data_dict=FALSE, users=FALSE, instruments=FALSE,
                      event_data=FALSE, arms=FALSE, mappings=FALSE,
                      proj_info=FALSE, version=FALSE,
                      dates=TRUE, labels=TRUE
                      ){
  
  # If provided, upload bundle to options.
  # This is provided simply to reduce the amount of syntax users must remember
  if (!is.null(bundle)) {
    options(redcap_bundle = bundle)
    return(message("Bundle uploaded to options."))
  }
  
  ## Error checking. Cannot use validate_args() here
  coll <- checkmate::makeAssertCollection()
  
  massert(~ url + token,
          fun = checkmate::assert_character,
          fixed = list(len = 1,
                      add = coll)
          )

  massert(~ dates + labels + data_dict + users + instruments + 
            event_data + arms + mappings + version + proj_info,
          fun = checkmate::assert_logical,
          fixed = list(len = 1,
                       add = coll)
          )
  
  checkmate::reportAssertions(coll)
  
  # If no data types are specificed, default to TRUE for all
  if (!any(data_dict, users, instruments, event_data, arms, mappings, proj_info, version)) {
    data_dict=TRUE
    users=TRUE
    instruments=TRUE
    event_data=TRUE
    arms=TRUE
    mappings=TRUE
    proj_info=TRUE
    version=TRUE
  }
  
  # The dictionary and user data are used for 2 bundle objects.
  # Exporting them first avoids duplicate API calls
  if (data_dict) meta_data = exportMetaData(url, token)
	if (users) userData = exportUsers(url, token, 
                                    dates = dates,
                                    labels = labels)
  # Create the bundle
  bundle = 
    structure(
      list(
    		redcap_url = url,
        data_dict = if (data_dict) meta_data else NULL,
    		id_field = if(data_dict) meta_data[1,1] else NULL,
        users = if (users) userData$Users else NULL,
        form_perm = if (users) userData$Form_Permissions else NULL,
        instruments = if (instruments) exportInstruments(url, token) else NULL,
        event_data = if (event_data) exportEvents(url, token) else NULL,
        arms = if (arms) exportArms(url, token) else NULL,
        mappings = if (mappings) exportMappings(url, token) else NULL,
        proj_info = if (proj_info) exportProjectInformation(url, token) else NULL,
        version = if (version) exportVersion(url, token) else NULL
      ),
      class = c("redcapBundle", "redcapProject", "list")
    )
  
  # Save the data to options
  if (create_option) {
    options(redcap_bundle = bundle)
    options(redcap_token = token)
    message("Project metadata has been saved as an option. You can access it via getOption('redcap_bundle')
            and getOption('redcap_token')")
    
    if (return_object==F)
      message("Option data will not persist across R sessions. Consider saving bundle data as an object for
              use in future sessions via saveRDS() or save.image()")
  }
  
  if (return_object==T) return(bundle)
}
