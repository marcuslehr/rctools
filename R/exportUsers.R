#' @name exportUsers
#' @title Export the Users for a Project
#' 
#' @description Retrieve a data frame giving the users, expiration dates,
#' and data access privileges for each user.
#'
#' @param url A url address to connect to the REDCap API
#' @param token A REDCap API token
#' @param dates Logical. Indicates if the expiration date is converted to a
#'   \code{POSIXct} object.
#' @param labels Logical. Indicates if the data export and form access rights are
#'   converted to factor objects.
#' @param ... Arguments to be passed to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#'
#' @details
#' For some reason I have yet to identify, some User Tables do not
#' export correctly. In some situations, the fields are all shifted one
#' column to the left and the form names are not always exported.
#' This seems to be more common in projects still in Development mode.
#' I have seen one instance of a project in Production where one user had
#' one more column given than any other user.  If you notice this behavior,
#' please report it to me as it may help me narrow down the source of the problem
#' 
#' @section REDCap API Documentation (6.5.0):
#' This function allows you to export the users for a project
#' 
#' @section REDCap Version:
#' 5.8.2 (Perhaps earlier) 
#' 
#' @section Known REDCap Limitations:
#' None
#' 
#' @return 
#' Returns a data frame. The number of columns in the data frame will depend on your 
#' version of REDCap.
#' \itemize{
#'   \item{\code{username }}{User name}
#'   \item{\code{email }}{The user's e-mail address}
#'   \item{\code{firstname }}{The user's first name}
#'   \item{\code{lastname }}{The user's last name}
#'   \item{\code{expiration }}{The expiration date of the user's access to the project}
#'   \item{\code{data_access_group }}{The data access group the user is assigned to}
#'   \item{\code{data_export }}{The user's data export rights. 0=no access, 
#'     2=De-Identified, 1=Full Data Set}
#'   \item{\code{mobile_app }}{(6.5.0+) Flag for if the user has permissions for the 
#'     mobile application}
#'   \item{\code{mobile_app_download_data }}{(6.5.0+) Flag for if the user may download
#'     data from the mobile app}
#' }
#' 
#' The data frame will have one additional column for each form giving the user's 
#' form-level permissions in the project.  0=no access, 2=read only, 
#' 1=view records/responses and
#' edit records (survey responses are read-only), 3 = edit survey responses
#'
#' @author Benjamin Nutter
#' @author Marcus Lehr
#'
#' @references
#' Please refer to your institution's API documentation.
#'
#' Additional details on API parameters are found on the package wiki at
#' \url{https://github.com/nutterb/redcapAPI/wiki/REDCap-API-Parameters}
#' 
#' Adopted code from \code{redcap-users-export} in the [REDCapR](https://github.com/OuhscBbmc/REDCapR)
#' package by Will Beasley to reshape form permissions data. 


exportUsers <- function(url = getOption("redcap_bundle")$redcap_url,
token = getOption("redcap_token"),
 dates=TRUE, labels=TRUE, ...,
                        error_handling = getOption("redcap_error_handling")){
  
  coll <- checkmate::makeAssertCollection()
  
  massert(~ url + token,
          fun = checkmate::assert_class,
          classes = list(url = "character", token = "character"),
          null.ok = list(url = FALSE, token = FALSE),
          fixed = list(add = coll))
  
  massert(~ dates + labels,
          fun = checkmate::assert_logical,
          fixed = list(len = 1,
                       add = coll))
  
  error_handling <- checkmate::matchArg(x = error_handling,
                                        choices = c("null", "error"))
  
  checkmate::reportAssertions(coll)
  
  #* parameters for the Users File Export
  body <- list(token = token, 
               content = 'user', 
               format = 'csv', 
               returnFormat = 'csv')
  
  #* Export Users file and convert to data frame
  x <- httr::POST(url = url, 
                  body = body)
  
  if (x$status_code != 200) redcap_error(x, error_handling)
  
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
    data_export                   = readr::col_character(),
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
  
  x <- readr::read_csv(
                file      = x$content,
                col_types = col_types
              )
  attr(x, "spec") <- NULL
  
  users <- dplyr::select(x, -forms)
  
  FormPerm <- x %>%
    dplyr::select(.data$username, .data$forms) %>%
    tidyr::separate_rows(.data$forms, sep = ",") %>%
    tidyr::separate(
              col     = forms,
              into    = c("form_name", "permission"),
              sep     = ":",
              convert = FALSE
            )
                             
  #* convert expiration date to POSIXct class
  if (dates) users$expiration <- as.POSIXct(users$expiration, format="%Y-%m-%d")
  
  #* convert data export and form editing privileges to factors
  if (labels){
    users$data_export <- 
      factor(x$data_export, 
             levels = c(0, 2, 1), 
             labels = c("No access", "De-identified", "Full data set")
             )
    
    FormPerm$permission <- 
      factor(FormPerm$permission, 
             levels = c(0, 2, 1, 3), 
             labels = c("No access", "Read only", 
                        "Edit records", "Edit survey responses")
             )
  }
  
  list(
    Users            = users,
    Form_Permissions = FormPerm
  )
}

utils::globalVariables(c("form_access", "form", "access"))