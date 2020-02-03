#' @name rc_setup
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
#' @param create_option Logical. Indicates whether the REDCap bundle should be
#' saved to an option.
#' @param return_object Logical. Indicates whether the REDCap bundle should be
#' returned as an object.
#' @param data_dict Logical.  Indicates if the meta data (data data_dictionary) 
#'   should be exported.
#' @param users Logical. Indicates if the users table should be exported.
#' @param instruments Logical. Indicates if the instruments table should be exported.
#' @param events Logical. Indicates if the event names should be exported.
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

rc_setup <- function(url,token, create_option=TRUE, return_object=FALSE,
                      data_dict=FALSE, users=FALSE, instruments=FALSE,
                      events=FALSE, arms=FALSE, mappings=FALSE,
                      proj_info=FALSE, version=FALSE,
                      dates=TRUE, labels=TRUE
                      ){
  
  coll <- checkmate::makeAssertCollection()
  
  massert(~ url + token,
          fun = checkmate::assert_character,
          fixed = list(len = 1,
                      add = coll)
          )

  massert(~ dates + labels + data_dict + users + instruments + 
            events + arms + mappings + version + proj_info,
          fun = checkmate::assert_logical,
          fixed = list(len = 1,
                       add = coll)
          )
  
  checkmate::reportAssertions(coll)
  
  if (!any(data_dict, users, instruments, events, arms, mappings, proj_info, version)) {
    data_dict=TRUE
    users=TRUE
    instruments=TRUE
    events=TRUE
    arms=TRUE
    mappings=TRUE
    proj_info=TRUE
    version=TRUE
  }
  
  if (users) userData = exportUsers(url, token, 
                                    dates = dates,
                                    labels = labels)
  
  bundle = 
    structure(
      list(
    		redcap_url = url,
        data_dict = if (data_dict) exportMetaData(url, token) else NULL,
    		id_field = if(data_dict) bundle$data_dict[1,1] else NULL,
        users = if (users) userData$Users else NULL,
        form_perm = if (users) userData$Form_Permissions else NULL,
        instruments = if (instruments) exportInstruments(url, token) else NULL,
        events = if (events) exportEvents(url, token) else NULL,
        arms = if (arms) exportArms(url, token) else NULL,
        mappings = if (mappings) exportMappings(url, token) else NULL,
        proj_info = if (proj_info) exportProjectInformation(url, token) else NULL,
        version = if (version) exportVersion(url, token) else NULL
      ),
      class = c("redcapBundle", "redcapProject", "list")
    )
  
  options(redcap_token = token)
  
  if (create_option==T) {
    options(redcap_bundle = bundle)
    message("Project metadata has been saved as an option. You can access it via getOption('redcap_bundle')")
    if (return_object==F)
      message("Option data will not persist across R sessions, consider saving bundle data as an object for
              use in future sessions via saveRDS() or save.image()")
  }
  
  if (return_object==T) return(bundle)
}
