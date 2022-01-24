#' @name validateImport
#' @importFrom chron times
#' 
#' @title Validate Data Frames for Import
#' @description Validates the variables in a data frame prior to attempting 
#'   an import to REDCap
#'   
#' @param data Data frame being prepared for import to REDCap.
#' @param data_dict REDCap database meta data.
#' @param logfile A character string giving the filepath to which the 
#'   results of the validation are printed.  If \code{""}, the results 
#'   are printed in the console.
#'   
#' @details
#' \code{validateImport} is called internally by \code{rc_import} and is 
#' not available to the user.
#' 
#' Each variable is validated by matching they type of variable with the type 
#' listed in the REDCap database.  
#' 
#' Although the log messages will indicate a preference for dates to be in 
#' mm/dd/yyyy format, the function will accept mm/dd/yy, yyyy-mm-dd, 
#' yyyy/mm/dd, and yyyymmdd formats as well.  When possible, pass dates as 
#' Date objects or POSIXct objects to avoid confusion.  Dates are also compared 
#' to minimum and maximum values listed in the data data_dictionary.  Records where 
#' a date is found out of range are allowed to import and a message 
#' is printed in the log.
#' 
#' For continuous/numeric variables, the values are checked against the 
#' minimum and maximum allowed in the data data_dictionary. Records where a value 
#' is found out of range are allowed to import and a message is printed 
#' in the log.
#' 
#' ZIP codes are tested to see if they fit either the 5 digit or 
#' 5 digit + 4 format.  When these conditions are not met, the data point is 
#' deleted and a message printed in the log.
#' 
#' YesNo fields permit any of the values 'yes', 'no', '0', '1' to be imported 
#' to REDCap with 0=No, and 1=Yes.  The values are converted to lower case 
#' for validation, so any combination of lower and upper case values 
#' will pass (ie, the data frame is not case-sensitive).
#' 
#' TrueFalse fields will accept 'TRUE', 'FALSE', 0, 1, and logical values 
#' and are also not case-sensitive.
#' 
#' Radio and dropdown fields may have either the coding in the data 
#' data_dictionary or the labels in the data data_dictionary. The validation will use 
#' the meta data to convert any matching values to the appropriate coding 
#' before importing to REDCap.  Values that cannot be reconciled are 
#' deleted with a message printed in the log.  These variables
#' are not case-sensitive.
#' 
#' Checkbox fields require a value of "Checked", "Unchecked", "0", or "1".  
#' These are currently case sensitive.  Values that do not match these are 
#' deleted with a warning printed in the log.
#' 
#' Phone numbers are required to be 10 digit numbers.  The phone number is 
#' broken into three parts: 1) a 3 digit area code, 2) a 3 digit exchange code, 
#' and 3) a 4 digit station code.  The exchange code must start with a number 
#' from 2-9, followed by 0-8, and then any third digit.  
#' The exchange code starts with a number from 2-9, followed by any two 
#' digits. The station code is 4 digits with no restrictions.
#' 
#' E-mail addresses are considered valid when they have three parts. The first 
#' part comes before the @@ symbol, and may be number of characters from 
#' a-z, A-Z, a period, underscore, percent, plus, or minus.  The second part
#' comes after the @@, but before the period, and may consist of any 
#' number of letters, numbers, periods, or dashes.  Finally, the string ends 
#' with a period then anywhere from 2 to 6 letters.
#' 
#' @author Benjamin Nutter
#' 
#' @references
#' See the REDCap Help and FAQ page's section on 'Text Validation Types'
#' 
#' Validating e-mail addresses
#' \url{http://www.regular-expressions.info/email.html}
#' 

validateImport <- function(data, data_dict, logfile = "")
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_data_frame(x = data,
                               add = coll)
  
  checkmate::assert_data_frame(x = data_dict,
                               add = coll)
  
  checkmate::assert_character(x = logfile,
                              len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  for (i in seq_along(data))
  {
    field_name <- names(data)[i]
    field_meta <- sub(pattern = "___[a-z,A-Z,0-9,_]+", 
                      replacement = "", 
                      x = field_name)
    meta_index <- which(data_dict$field_name == field_meta)
    field_type <- data_dict[meta_index, "field_type"]
    
    if (length(field_type)){
      if (field_type == "text" &&
          !is.na(data_dict[meta_index, 
                    "text_validation_type_or_show_slider_number"])){
        field_type <- data_dict[meta_index, 
                                "text_validation_type_or_show_slider_number"]
      }
    }
    
    field_min <- data_dict[meta_index,
                           "text_validation_min"]
    field_max <- data_dict[meta_index,
                           "text_validation_max"]
    
    field_choice <- data_dict[meta_index,
                              "select_choices_or_calculations"]
    
    if (!length(field_type))
    {
      if (field_name %in% paste0(unique(data_dict$form_name),'_complete'))
      {
        field_type <- "form_complete"
      }
      else
      {
        field_type <- "redcap_internal_type"
      }
    }
    
    if (field_type %in% c("float", "integer", "number", "number_1dp"))
      field_type <- "numeric"
  
    data[[field_name]] <- 
      switch(
        EXPR = field_type,
        "redcap_internal_type" = data[[field_name]],
        "form_complete" = 
          validate_import_form_complete(x = data[[field_name]],
                                        field_name = field_name,
                                        logfile = logfile),
        "date_dmy" = 
          validate_import_date(x = data[[field_name]],
                               field_name = field_name,
                               field_min = field_min,
                               field_max = field_max,
                               logfile = logfile),
        "date_mdy" = 
          validate_import_date(x = data[[field_name]],
                               field_name = field_name,
                               field_min = field_min,
                               field_max = field_max,
                               logfile = logfile),
        "date_ymd" = 
          validate_import_date(x = data[[field_name]],
                               field_name = field_name,
                               field_min = field_min,
                               field_max = field_max,
                               logfile = logfile),
        "datetime_dmy" = 
          validate_import_datetime(x = data[[field_name]],
                                   field_name = field_name,
                                   field_min = field_min,
                                   field_max = field_max,
                                   logfile = logfile),
        "datetime_mdy" = 
          validate_import_datetime(x = data[[field_name]],
                                   field_name = field_name,
                                   field_min = field_min,
                                   field_max = field_max,
                                   logfile = logfile),
        "datetime_ymd" = 
          validate_import_datetime(x = data[[field_name]],
                                   field_name = field_name,
                                   field_min = field_min,
                                   field_max = field_max,
                                   logfile = logfile),
        "datetime_seconds_dmy" = 
          validate_import_datetime_seconds(x = data[[field_name]],
                                           field_name = field_name,
                                           field_min = field_min,
                                           field_max = field_max,
                                           logfile = logfile),
        "datetime_seconds_mdy" = 
          validate_import_datetime_seconds(x = data[[field_name]],
                                           field_name = field_name,
                                           field_min = field_min,
                                           field_max = field_max,
                                           logfile = logfile),
        "datetime_seconds_ymd" = 
          validate_import_datetime_seconds(x = data[[field_name]],
                                           field_name = field_name,
                                           field_min = field_min,
                                           field_max = field_max,
                                           logfile = logfile),
        "time" = 
          validate_import_time(x = data[[field_name]],
                               field_name = field_name,
                               field_min = field_min,
                               field_max = field_max,
                               logfile = logfile),
        "time_mm_ss" = 
          validate_import_time_mm_ss(x = data[[field_name]],
                                     field_name = field_name,
                                     field_min = field_min,
                                     field_max = field_max,
                                     logfile = logfile),
        "numeric" = 
          validate_import_numeric(x = data[[field_name]],
                                  field_name = field_name,
                                  field_min = field_min,
                                  field_max = field_max,
                                  logfile = logfile),
        "zipcode" = 
          validate_import_zipcode(x = data[[field_name]],
                                  field_name = field_name,
                                  logfile = logfile),
        "yesno" = 
          validate_import_yesno(x = data[[field_name]],
                                field_name = field_name,
                                logfile = logfile),
        "truefalse" = 
          validate_import_truefalse(x = data[[field_name]],
                                    field_name = field_name,
                                    logfile = logfile),
        "select" = 
          validate_import_select_dropdown_radio(x = data[[field_name]],
                                                field_name = field_name,
                                                field_choice = field_choice,
                                                logfile = logfile),
        "dropdown" = 
          validate_import_select_dropdown_radio(x = data[[field_name]],
                                                field_name = field_name,
                                                field_choice = field_choice,
                                                logfile = logfile),
        "radio" = 
          validate_import_select_dropdown_radio(x = data[[field_name]],
                                                field_name = field_name,
                                                field_choice = field_choice,
                                                logfile = logfile),
        "checkbox" = 
          validate_import_checkbox(x = data[[field_name]],
                                   field_name = field_name,
                                   field_choice = field_choice,
                                   logfile),
        "email" = 
          validate_import_email(x = data[[field_name]],
                                field_name = field_name,
                                logfile = logfile),
        "phone" = 
          validate_import_phone(x = data[[field_name]],
                                field_name = field_name,
                                logfile = logfile),
        data[[field_name]]
      )
  }
  
  invisible(data)
}


# Import methods ----------------------------------------------------------

# validate_import_form_complete -------------------------------------

validate_import_form_complete <- function(x, field_name, logfile)
{
  x <- as.character(x)
  x <- gsub(pattern = "Incomplete", 
            replacement = "0", 
            x = x)
  x <- gsub(pattern = "Unverified", 
            replacement = "1", 
            x = x)
  x <- gsub(pattern = "Complete", 
            replacement = "2", 
            x = x)
  
  w <- which(!grepl("[0-2]", x) & !is.na(x)) # NAs getting flagged inappropriately here
  x[w] <- NA # If overwrite is on in rc_import invalid values will erase current values
  
  print_validation_message(
    field_name, 
    indices = w,
    message = paste0("Values(s) must be one of: 0, 1, 2, ",
                     "Incomplete, Unverified, or Complete.\n", 
                     "Value not imported"),
    logfile = logfile)
  
  x
}

# validate_import_date ----------------------------------------------

validate_import_date <- function(x, field_name, field_min, field_max, logfile)
{
  if (!inherits(x, "Date") && !inherits(x, "POSIXct"))
  {
    suppressWarnings(
      x <- lubridate::parse_date_time(x = x,
                                      orders = c("ymd", "ymd HMS",
                                                 "mdy", "mdy HMS",
                                                 "dmy", "dmy HMS"))
    )
  }
  
  w_low <- which(as.POSIXct(x) < as.POSIXct(field_min, origin = "1970-01-01"))
  print_validation_message(
    field_name,
    indices = w_low,
    message = paste0("Value(s) are before the stated minimum date: ",
                     format(field_min, format = "%Y-%m-%d")),
    logfile = logfile
  )
  
  w_high <- which(as.POSIXct(x) > as.POSIXct(field_max, origin = "1970-01-01"))
  print_validation_message(
    field_name,
    indices = w_high,
    message = paste0("Values(s) are after the stated maximum date: ",
                     format(field_max, format = "%Y-%m-%d")),
    logfile = logfile
  )
  
  # Note natural NAs
  w_na = which(is.na(x))
  
  # Apply formatting. This will create NAs when the value cannot be coerced
  x <- format(x, format = "%Y-%m-%d")
  
  # Identify new NAs after formatting
  w <- setdiff(which(is.na(x)), w_na) # Same issue as above, invalid values will erase when overwrite = T
  
  print_validation_message(
    field_name, 
    indices = w,
    message = paste0("Value(s) must have POSIXct class, Date class, ",
                     "or character class in ymd, mdy, or dmy format (with optional HMS).\n", 
                     "Value not imported"),
    logfile = logfile)
  
  x
}

# validate_import_datetime ------------------------------------------

validate_import_datetime <- function(x, field_name, field_min, field_max, logfile)
{
  if (!inherits(x, "Date") && !inherits(x, "POSIXct"))
  {
    suppressWarnings(
      x <- lubridate::parse_date_time(x = x,
                                      orders = c("ymd", "ymd HMS",
                                                 "mdy", "mdy HMS",
                                                 "dmy", "dmy HMS"))
    )
  }
  
  w_low <- which(as.POSIXct(x) < as.POSIXct(field_min, origin = "1970-01-01"))
  print_validation_message(
    field_name,
    indices = w_low,
    message = paste0("Value(s) are before the stated minimum date: ",
                     format(field_min, format = "%Y-%m-%d %H:%M")),
    logfile = logfile
  )
  
  w_high <- which(as.POSIXct(x) > as.POSIXct(field_max, origin = "1970-01-01"))
  print_validation_message(
    field_name,
    indices = w_high,
    message = paste0("Values(s) are after the stated maximum date: ",
                     format(field_max, format = "%Y-%m-%d %H:%M")),
    logfile = logfile
  )
  
  x <- format(x, format = "%Y-%m-%d %H:%M")
  
  w <- which(is.na(x)) # Same issue as above, natural NAs will be flagged and invalid values will erase when overwrite = T
  
  print_validation_message(
    field_name, 
    indices = w,
    message = paste0("Value(s) must have POSIXct class, Date class, ",
                     "or character class in ymd, mdy, or dmy format (with optional HMS).\n", 
                     "Value not imported"),
    logfile = logfile)
  
  x
}

# validate_import_datetime_seconds ----------------------------------

validate_import_datetime_seconds <- function(x, field_name, field_min, field_max, logfile)
{
  if (!inherits(x, "Date") && !inherits(x, "POSIXct"))
  {
    suppressWarnings(
      x <- lubridate::parse_date_time(x = x,
                                      orders = c("ymd", "ymd HMS",
                                                 "mdy", "mdy HMS",
                                                 "dmy", "dmy HMS"))
    )
  }
  
  w_low <- which(as.POSIXct(x) < as.POSIXct(field_min))
  print_validation_message(
    field_name,
    indices = w_low,
    message = paste0("Value(s) are before the stated minimum date: ",
                     format(field_min, format = "%Y-%m-%d %H:%M:%S")),
    logfile = logfile
  )
  
  w_high <- which(as.POSIXct(x) > as.POSIXct(field_max))
  print_validation_message(
    field_name,
    indices = w_high,
    message = paste0("Values(s) are after the stated maximum date: ",
                     format(field_max, format = "%Y-%m-%d %H:%M:%S")),
    logfile = logfile
  )
  
  x <- format(x, format = "%Y-%m-%d %H:%M:%S")
  
  w <- which(is.na(x)) # Same issue as above, natural NAs will be flagged and invalid values will erase when overwrite = T
  
  print_validation_message(
    field_name, 
    indices = w,
    message = paste0("Value(s) must have POSIXct class, Date class, ",
                     "or character class in ymd, mdy, or dmy format (with optional HMS).\n", 
                     "Value not imported"),
    logfile = logfile)
  
  x
}

# validate_import_time ----------------------------------------------

validate_import_time <- function(x, field_name, field_min, field_max, logfile)
{
  x <- as.character(x)
  
  w_invalid <- !grepl("^(\\d{2}:\\d{2}:00|\\d{2}:\\d{2})$", x)
  x[w_invalid] <- NA
  
  count_minute <- function(t)
  {
    if (is.na(t)) return(NA)
    t <- strsplit(t, ":")
    t <- unlist(t)
    t <- as.numeric(t)
    t[1] * 60 + t[2]
  }
  
  total_min <- vapply(x, count_minute, numeric(1))
  
  print_validation_message(
    field_name,
    indices = which(total_min < count_minute(field_min)),
    message = paste0("Value(s) are before the stated minimum time: ",
                     field_min),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(total_min > count_minute(field_max)),
    message = paste0("Value(s) are after the stated maximum time: ",
                     field_max),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(w_invalid),
    message = paste0("Value(s) must be of class `time` or in one of hh:mm:ss or hh:mm formats.\n",
                     "Values not imported",
                     field_min),
    logfile = logfile
  )
  
  substr(x, 1, 5)
}

# validate_import_time_mm_ss ----------------------------------------

validate_import_time_mm_ss <- function(x, field_name, field_min, field_max, logfile)
{
  x <- as.character(x)
  
  x[grepl("^\\d{2}:\\d{2}:\\d{2}$", x)] <- 
    sub("^\\d{2}:", "", x[grepl("^\\d{2}:\\d{2}:\\d{2}$", x)])
  
  w_invalid <- !grepl("^\\d{2}:\\d{2}$", x)
  x[w_invalid] <- NA
  
  count_second <- function(t)
  {
    if (is.na(t)) return(NA)
    t <- strsplit(t, ":")
    t <- unlist(t)
    t <- as.numeric(t)
    t[1] * 60 + t[2]
  }
  
  total_sec <- vapply(x, count_second, numeric(1))
  
  print_validation_message(
    field_name,
    indices = which(total_sec < count_second(field_min)),
    message = paste0("Value(s) are before the stated minimum time: ",
                     field_min),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(total_sec > count_second(field_max)),
    message = paste0("Value(s) are after the stated maximum time: ",
                     field_max),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(w_invalid),
    message = paste0("Value(s) must be of class `time` or in one of hh:mm:ss or hh:mm formats.\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_numeric -------------------------------------------

validate_import_numeric <- function(x, field_name, field_min, field_max, logfile)
{
  ## It's probably better to do this manually
  # # Attempt to remove non-numeric data
  # x = stringr::str_extract(x, "\\d+\\.?\\d*")
  
  # Check for values not coercible to numeric
  suppressWarnings(num_check <- as.numeric(x))
  w <- which(is.na(num_check) & !is.na(x))
  
  suppressWarnings({
    if (!is.numeric(x)) x <- as.numeric(x)
    field_min <- as.numeric(field_min)
    field_max <- as.numeric(field_max)
  })
  
  print_validation_message(
    field_name,
    indices = which(x < field_min),
    message = paste0("Value(s) are less than the stated minimum: ",
                     field_min),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(x > field_max),
    message = paste0("Value(s) are greater than the stated maximum: ",
                     field_max),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be numeric or coercible to numeric.\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_zipcode -------------------------------------------

validate_import_zipcode <- function(x, field_name, logfile)
{
  x <- as.character(x)
  w <- which(!grepl("(\\d{5}|\\d{5}-\\d{4})", x) & !is.na(x))
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be in the format `12345` or `12345-1234`.\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_yesno ---------------------------------------------

validate_import_yesno <- function(x, field_name, logfile)
{
  x <- as.character(x)
  x <- tolower(x)
  w <- which(!x %in% c("no", "yes", "0", "1") & !is.na(x))
  
  x <- gsub("no", "0", x)
  x <- gsub("yes", "1", x)
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be one of `0`, `1`, `No`, or `Yes` (Ignoring case).\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_truefalse -----------------------------------------

validate_import_truefalse <- function(x, field_name, logfile)
{
  x <- as.character(x)
  x <- tolower(x)
  w <- which(!x %in% c("true", "false", "0", "1", "no", "yes") & !is.na(x))
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be one of logical or one of `0`, `1`, `No`, `Yes`, `False`, or `True` (Ignoring case).\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x[w] <- NA
  
  x <- gsub("(true|yes)", 1, x)
  x <- gsub("(false|no)", 0, x)
  x
}

# validate_import_select_dropdown_radio -----------------------------

validate_import_select_dropdown_radio <- function(x, field_name, field_choice, logfile)
{
  x <- as.character(x)
  mapping <- strsplit(field_choice, "[|]")
  mapping <- unlist(mapping)
  mapping <- stringr::str_split_fixed(mapping, ", ", 2)
  mapping <- trimws(mapping)
  
  #* Return labeled values to coded values
  for (i in seq_len(nrow(mapping))){
    x[x==mapping[i, 2]] <- mapping[i, 1]  
  }
  
  w <- which(!x %in% mapping[, 1] & !is.na(x))
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be one of '",
                     paste0(mapping[, 1], collapse = "', '"), "', '",
                     paste0(mapping[, 2], collapse = "', '"),
                     "'.\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_checkbox ------------------------------------------

validate_import_checkbox <- function(x, field_name, field_choice, logfile)
{
  x <- trimws(tolower(as.character(x)))
  
  #* Select the labeled string from the options as a valid input for the import.
  checkChoice <- trimws(stringr::str_split_fixed(unlist(strsplit(field_choice, "[|]")), ", ", 2))
  checkChoice <- checkChoice[checkChoice[, 1] == unlist(strsplit(field_name, "___"))[2], ]
  
  w <- which(!x %in% c("Checked", "Unchecked", "0", "1", checkChoice, "") & !is.na(x))
  
  x <- gsub("checked", "1", x)
  x <- gsub("unchecked", "0", x)
  x[x %in% checkChoice] <- 1
  x[x == ""] <- 0
  x[!x %in% c("0", "1")] <- NA
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be one of '0', '1', 'Checked', 'Unchecked', '",
                     checkChoice, "', '' (ignoring case).\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_email ---------------------------------------------

validate_import_email <- function(x, field_name, logfile)
{
  x <- as.character(x)
  w <- which(!grepl("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+[.][A-Za-z]{2,6}$", x) & !is.na(x))
  
  print_validation_message(
    field_name = field_name,
    indices = w,
    message = paste0("Value(s) are not valid e-mail addresses.\n",
                     "Values not imported.")
  )
  
  x[w] <- NA
  
  x
}

# validate_import_phone ---------------------------------------------

validate_import_phone <- function(x, field_name, logfile)
{
  x <- as.character(x)
  x <- gsub("[[:punct:][:space:]]", "", x)
  
  w_long <- which(nchar(x) != 10 & !is.na(x))
  
  w_invalid <- which(grepl("^[2-9][0-8][0-9][2-9][0-9]{6}$", x))
  
  print_validation_message(
    field_name = field_name,
    indices = w_long,
    message = paste0("Value(s) are not 10 digit phone numbers.\n",
                     "Values not imported.")
  )
  
  print_validation_message(
    field_name = field_name,
    indices = w_long,
    message = paste0("Value(s) are not valid North American phone numbers.\n",
                     "Values not imported.")
  )
  
  x[w_long | w_invalid] <- NA
  x
}

# print_validation_message ------------------------------------------

print_validation_message <- function(field_name, indices, message, logfile)
{
  if (length(indices))
  {
    message <- 
      paste0("------------------------------------\n",
             "Field Name: `", field_name, "`\n",
             "Row Indices: ", paste0(indices, collapse = ", "), "\n",
             message, "\n\n")
    
    if (logfile == "")
    {
      message(message)
    }
    else  
    {
      write(message, 
            file = logfile,
            append = TRUE)
    }
  }
}
