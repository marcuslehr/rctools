#' @name rc_api_call
#' 
#' @title Execute a call to the REDCap API
#' @description This is a generic API function. It is intended to provide a more 
#' flexible interface to the API than \code{rc_import} and \code{rc_export} while 
#' still improving usability. It is also a barebones type function by natures 
#' and does not have the same level of checks and other embellishments as 
#' other package functions.
#' 
#' @param url URL of the REDCap API
#' @param token Project API token
#' 
#' @param content API endpoint. The options are 'arm', 'dag', 'event', 'exportFieldNames',
#' 'file', 'formEventMapping', 'generateNextRecordName', 'instrument', 'log', 
#' 'metadata', 'participantList', 'pdf', 'project', 'project_settings', 'project_xml',
#' 'record', 'repeatingFormsEvents', 'report', 'surveyLink', 'surveyReturnCode',
#' 'surveyQueueLink', 'user', 'userDagMapping', 'userRole', 'userRoleMapping', and 'version'
#' @param action Action to perform when calling the endpoint. Either 'import',
#' 'export', 'delete', 'rename', or 'switch'
#' @param return_as Determines how the server reponse will be returned from this 
#' function. This is mostly useful for returning the raw response when needed. 
#' Options are 'raw', 'text', and 'dataframe'. 'raw' returns raw byte data, 'text' 
#' converts it to character, and 'dataframe' returns a data.frame.
#' @param ... Additional arguments to be passed to the API
#' 
#' @param arms Vector of arm numbers
#' @param fields Vector of field names
#' @param forms Vector of form names
#' @param events Vector of event names
#' @param records Vector of record IDs
#' @param dags Vector of data access groups
#' 
#' @param beginTime Filter for log exports. Format is 'YYYY-MM-DD HH:MM'
#' @param endTime Filter for log exports. Format is 'YYYY-MM-DD HH:MM'
#' @param csvDelimiter Operator to delimit csv exports with. For exporting records
#'   and reports. Options are ',', 'tab', ';', '|', and '^'
#' @param dag Specifies a data access group to switch to
#' @param data Data to import
#' @param dateRangeBegin Optional date filter for records exports. Use format 'YYYY-MM-DD HH:MM:SS'
#' @param dateRangeEnd Optional date filter for records exports. Use format 'YYYY-MM-DD HH:MM:SS'
#' @param event Event name. For file import/export
#' @param exportCheckboxLabel Logical. Will use the choice label or NA to fill 
#'   fields instead of Checked or Unchecked. Requires rawOrLabel to be set to 
#'   'true', otherwise does nothing.
#' @param exportDataAccessGroups Logical
#' @param exportSurveyFields Logical
#' @param file_field Field name for file import/export
#' @param file Path to file for import
#' @param filterLogic Optional logic filter for record exports. Use REDCap
#'  style syntax- ie. similar to branching logic, calculations, etc.
#' @param forceAutoNumber Logical. For importing records
#' @param logtype Unknown
#' @param overWriteBehavior Determines whether import data will overwrite existing
#'   data. Options are 'normal or 'overwrite'. Pay careful attention to NA values
#'   when using this option, as they will overwrite existing values. In normal
#'   mode, existing values cannot be replaced with another value or NA.
#' @param rawOrLabel For choice fields such as radio and drop-down. 'raw' will
#'   fill the data using the choice numbers while 'label' will use the choice labels.
#' @param rawOrLabelHeaders Determines whether to use field names or field labels
#'   for column names
#' @param record Specify record ID for file import/export or log filtering
#' @param report_id ID number of report for export. May be supplied as either 
#'   character or numeric.
#' @param returnContent Specify response type when importing records. Options are
#'   'count', 'ids', 'auto_ids', and 'nothing'. 'count' returns the number of 
#'   records which were updated. 'ids' returns a list of updated IDs. 'auto_ids'
#'   appears to be the same as 'count'? 'nothing' returns nothing.
#' @param returnMetadataOnly Logical. For 'project_xml' export.
#' @param type For record imports/exports. Options are 'flat' and 'eav'
#' @param user Specify a user for log exports
#' @param format Specify format of imported or exported data. Options are 'csv',
#'   'json', or 'xml'
#' @param returnFormat Specify format for errors. Options are 'csv', 'json', or 'xml'
#' @param encode Encoding format to be passed to \code{httr::POST()}. Options are
#'   'form', 'multipart', or 'identity'
#' 
#' @author Marcus Lehr
#' @export


rc_api_call <- function(url = getOption("redcap_bundle")$redcap_url,
                        token = getOption("redcap_token"),
                        content='version', action='export', 
                        return_as='dataframe', ...,
                        
                        
                        arms=NULL, fields=NULL, forms=NULL, 
                        events=NULL, records=NULL, dags=NULL,
                        
                        beginTime='',
                        csvDelimiter='',
                        dag='',
                        data=NULL, # Cannot use '' default. Empty frames will be imported even when action='export'
                        dateRangeBegin='',
                        dateRangeEnd='',
                        endTime='',
                        event='',
                        exportCheckboxLabel='false',
                        exportDataAccessGroups='false',
                        exportSurveyFields='false',
                        file_field='',
                        file='path/to/file',
                        filterLogic='',
                        forceAutoNumber='false',
                        logtype='',
                        overWriteBehavior='normal',
                        rawOrLabel='raw',
                        rawOrLabelHeaders='raw',
                        record='',
                        report_id='',
                        returnContent='count',
                        returnMetadataOnly='false',
                        type='flat',
                        user='',
                        
                        format='csv',
                        returnFormat='csv',
                        encode='form'
                        ){

# Construct body for POST() -----------------------------------------------

  # Check for directionality conflict
  if (action=='export' & !is.null(data)) stop('Error: Data supplied to export call')
  
  # Perform token coercion.
  validate_args(c('url','token'), url=url, token=token)
  
  # Initial assembly of body args
  body = list(content=content, token=token,
              action=action,
              beginTime=beginTime,
              csvDelimiter=csvDelimiter,
              dag=dag,
              data=data,
              dateRangeBegin=dateRangeBegin,
              dateRangeEnd=dateRangeEnd,
              endTime=endTime,
              event=event,
              exportCheckboxLabel=exportCheckboxLabel,
              exportDataAccessGroups=exportDataAccessGroups,
              exportSurveyFields=exportSurveyFields,
              field=file_field,
              filterLogic=filterLogic,
              forceAutoNumber=forceAutoNumber,
              format=format,
              logtype=logtype,
              overWriteBehavior=overWriteBehavior,
              rawOrLabel=rawOrLabel,
              rawOrLabelHeaders=rawOrLabelHeaders,
              record=record,
              report_id=report_id,
              returnContent=returnContent,
              returnFormat=returnFormat,
              returnMetadataOnly=returnMetadataOnly,
              type=type,
              user=user
  )
  
  # Expand body to include provided selections
  body = c(body, list(...))
  if (!is.null(fields)) body[['fields']] <- paste0(fields, collapse=",")
  if (!is.null(forms)) body[['forms']] <- paste0(forms, collapse=",")
  if (!is.null(events)) body[['events']] <- paste0(events, collapse=",")
  if (!is.null(records)) body[['records']] <- paste0(records, collapse=",")
  if (!is.null(arms)) body[['arms']] <- paste0(arms, collapse=",")
  if (!is.null(dags)) body[['dags']] <- paste0(dags, collapse=",")
  
  # Make modifications for specific API endpoints (defined by content argument)
  if (action=='import') {
    switch(content,
           'file' = {
             body[['file']] = httr::upload_file(file)
             encode = 'multipart'
             },
           'project' = {content = 'project_settings'},
           'record' = {
						 if ('data.frame' %in% class(data)) 
							 body[['data']] = data_frame_to_string(data)
						 else
							 stop("Error: Please provide records in the form of a data.frame") 
             }
           )
  }
  

# Call API and parse response ---------------------------------------------

  # API call
  response <- httr::POST(url, body = body, encode = encode)
  
  # Check for http errors
  if (response$status_code != 200) stop(as.character(response))
  
  # Parse response
  if (action=='import') { 
    if (content=='file') return("Upload successful") # NULL returned for content in this case
    if (content=='record') switch(returnContent,
           'count' = message(paste0('Number of records updated: ', as.character(response))),
           'auto_ids' = message(paste0('Number of records updated: ', as.character(response))),
           'nothing' = return(), 
           'ids' = message(paste0("Records updated: ", 
                          paste(utils::read.csv(text=as.character(response))$id,
                                collapse=', ')))
           )
  }
    else if (action=='export') {
      # Check for data
      if (length(response$content)<=1) stop("No data were returned.")
  
      # Extract data
      if (content=='version') return_as = 'text'
      if (return_as=='text') return(as.character(response)) # httr::content(response,content_as)
        else if (return_as=='raw') return(response$content)
        else if (return_as=='dataframe')
          return(utils::read.csv(text = as.character(response),
                                 stringsAsFactors = FALSE,
                                 na.strings = ""))
        # Using httr::content() returns the blue '0s' text in front of everything, even when using suppressMessages()
        # suppressMessages(as.data.frame( httr::content(response,content_as)))
        else stop("Argument return_as must be one of 'raw', 'text', or 'dataframe'")
    }
}

# Unexported functions --------------------------------------

# Dataframes need to be converted to a csv string for import
data_frame_to_string <- function(data) {
  paste0(
    utils::capture.output(
      utils::write.table(data, 
                         sep = ",",
                         col.names = TRUE,
                         row.names = FALSE,
                         na = '') # Need to double check how RC interprets this vs NA
    ),
    collapse = "\n"
  )
}