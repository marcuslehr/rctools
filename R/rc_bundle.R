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
#' @param token Path to a text file containing your REDCap API token
#' @param create_options Logical. Indicates whether the REDCap bundle, token, and
#' url should be uploaded to the R session options.
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
#'   
#' @author Marcus Lehr
#' @author Benjamin Nutter
#' 
#' @export

rc_bundle <- function(url,token,
                      create_options=TRUE, return_object=TRUE,
                      data_dict=FALSE, users=FALSE, instruments=FALSE,
                      event_data=FALSE, arms=FALSE, mappings=FALSE,
                      proj_info=FALSE, version=FALSE
                      ){
  
  ### Error checking ------------
  
  # Cannot use validate_args() here because of terminology devations
  coll <- checkmate::makeAssertCollection()
  
  ##--- token
  # Attempt to read token from file. Catch errors to handle token strings instead of paths
  invalid_path = F
  tryCatch(token <- readr::read_lines(token)[1],
           error = function(cond) invalid_path <<- T)
  
  # Check token format
  if (!grepl("^[[:alnum:]]{32}$", token)) invalid_format = T
  else invalid_format = F
  
  
  # File doesn't exist and the string isn't a token
  if (invalid_path & invalid_format) 
    coll$push("Please provide a valid path to the token file.")
  # File exists but doesn't contain a token
  else if (invalid_format)
    coll$push("REDCap tokens must be exactly 32 alpha-numeric characters.")
  
  # If pass then the string is a token. No else statement/assignment needed here
  # Edge case of 32 character invalid path could make it here also
  
  
  massert(~ url + token,
          fun = checkmate::assert_character,
          fixed = list(len = 1,
                      add = coll)
          )

  massert(~ data_dict + users + instruments + 
            event_data + arms + mappings + version + proj_info,
          fun = checkmate::assert_logical,
          fixed = list(len = 1,
                       add = coll)
          )
  
  checkmate::reportAssertions(coll)

# Assemble bundle ---------------------------------------------------------
  
  # If no data types are specified, default to TRUE for all
  if (!any(data_dict,users,instruments,event_data,arms,mappings,proj_info,version)) {
    data_dict=TRUE
    users=TRUE
    instruments=TRUE
    event_data=TRUE
    arms=TRUE
    mappings=TRUE
    proj_info=TRUE
    version=TRUE
  }
  
  # If the project isn't longitudinal, RC will throw an error when requesting certain data types
  if (event_data) {
    project_info = rc_api_call(url,token, 'project')
    
    if (!project_info$is_longitudinal) {
      event_data = F
      arms = F
      mappings = F
    }
  }
  
  # The dictionary and user data are used for 2 bundle objects.
  # Exporting them first avoids duplicate API calls
  if (data_dict) {
    meta_data = rc_api_call(url,token, 'metadata')
    
    # Check for corrupted data dictionaries
    if (!nrow(meta_data)) 
      warning("Data dictionary is corrupted. This is typically caused by non-ASCII 
              characters. It can be remedied by exporting the data dictionary from 
              the Redcap GUI and running cleanseMetaData()")
  }
	if (users) user_data = export_users(url,token)
  


# Assemble bundle ---------------------------------------------------------
  
  # Create the bundle
  bundle = 
    structure(
      list(
    		redcap_url = url,
        data_dict = if (data_dict) meta_data else NULL,
    		id_field = if(data_dict) meta_data$field_name[1] else NULL,
        users = if (users) user_data[[1]] else NULL,
        form_perm = if (users) user_data[[2]] else NULL,
        instruments = if (instruments) rc_api_call(url,token, 'instrument') else NULL,
        event_data = if (event_data) rc_api_call(url,token, 'event') else NULL,
        arms = if (arms) rc_api_call(url,token, 'arm') else NULL,
        mappings = if (mappings) rc_api_call(url,token, 'formEventMapping') else NULL,
        proj_info = if (exists('project_info')) project_info else if (proj_info) rc_api_call(url,token, 'project') else NULL,
        version = if (version) rc_api_call(url,token, 'version') else NULL
      ),
      class = c("redcapBundle", "list")
    )
  
  # Save the data to options
  if (create_options) {
    options(redcap_bundle = bundle)
    options(redcap_token = token)
    message("Project metadata has been saved to options as 'redcap_bundle', 'redcap_token'. 
            You can access these options via getOption()")
    
    if (return_object==F)
      message("Option data will not persist across R sessions. Consider saving bundle data as an object for
              use in future sessions via saveRDS() or save.image()")
  }
  
  if (return_object==T) return(bundle)
}


# Unexported functions ----------------------------------------------------

export_users = function(url,token) {
  # Return data as raw bc type_convert() can't do it's job after the fact
  user_data = rc_api_call(url,token, 'user', return_as = 'raw')
  
  # Specify column types
  col_types <- readr::cols(
    username                      = readr::col_character(),
    email                         = readr::col_character(),
    firstname                     = readr::col_character(),
    lastname                      = readr::col_character(),
    expiration                    = readr::col_date(),
    data_access_group             = readr::col_character(),
    data_access_group_id          = readr::col_character(),
    design                        = readr::col_logical(),
    user_rights                   = readr::col_logical(),
    data_access_groups            = readr::col_logical(),
    reports                       = readr::col_logical(),
    stats_and_charts              = readr::col_logical(),
    manage_survey_participants    = readr::col_logical(),
    calendar                      = readr::col_logical(),
    data_import_tool              = readr::col_logical(),
    data_comparison_tool          = readr::col_logical(),
    logging                       = readr::col_logical(),
    file_repository               = readr::col_logical(),
    data_quality_create           = readr::col_logical(),
    data_quality_execute          = readr::col_logical(),
    api_export                    = readr::col_logical(),
    api_import                    = readr::col_logical(),
    mobile_app                    = readr::col_logical(),
    mobile_app_download_data      = readr::col_logical(),
    record_create                 = readr::col_logical(),
    record_rename                 = readr::col_logical(),
    record_delete                 = readr::col_logical(),
    lock_records_all_forms        = readr::col_logical(),
    lock_records                  = readr::col_logical(),
    lock_records_customization    = readr::col_logical(),
    forms                         = readr::col_character()
  )
  
  # Convert data to data.frame and remove spec attritube as it may cause issues
  user_data = suppressMessages(readr::read_csv(user_data, col_types = col_types))
  attr(user_data, "spec") <- NULL
  
  # Restructure forms data
  FormPerm <- user_data %>%
    dplyr::select(username, forms) %>%
    tidyr::separate_rows(forms, sep = ",") %>%
    tidyr::separate(
      col     = forms,
      into    = c("form_name", "view"),
      sep     = ":",
      convert = FALSE
    )
  FormPerm <- user_data %>%
    dplyr::select(username, forms_export) %>%
    tidyr::separate_rows(forms_export, sep = ",") %>%
    tidyr::separate(
      col     = forms_export,
      into    = c("form_name", "export"),
      sep     = ":",
      convert = FALSE
    ) %>% dplyr::full_join(FormPerm,., by = c("username", "form_name"))
  
  user_data <- dplyr::select(user_data, -forms, -forms_export)
  
  
  # Format and label
  user_data$expiration <- as.POSIXct(user_data$expiration, format="%Y-%m-%d")
  FormPerm$view <- 
    factor(FormPerm$view, 
           levels = c(0, 2, 1, 3), 
           labels = c("No access", "Read only", 
                      "Edit records", "Edit survey responses")
    )
  FormPerm$export <- 
    factor(FormPerm$export, 
           levels = c(0, 2, 1), 
           labels = c("No access", "De-identified", "Full data set")
    )
  
  return(list(user_data,FormPerm))
}
