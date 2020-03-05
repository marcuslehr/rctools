#' @name validate_args
#'
#' @title Perform error checking on function arguments
#' @description  This will validate all supplied function arguments using the
#' \code{checkmate} package. All function arguments within the \code{rctools}
#' package should be contained in this function. To add new arguments, add the
#' argument to this function, then append the appropriate \code{vars} list in
#' this function's body. 
#' 
#' This is an internal function only.
#' 
#' @param required Character. Vector of argument names which are required by
#' the calling function.
#' 
#' @param report_id Numeric, length == 1
#' @param batch.size Numeric, length == 1
#' @param sd_threshold Numeric, length == 1
#' 
#' @param url Character, length == 1
#' @param token Character, length == 1; 32 alpha-numeric characters
#' @param id_field Character, length == 1
#' @param logfile Character, length == 1
#' @param completion_field Character, length == 1
#' 
#' @param records Character vector
#' @param fields Character vector
#' @param forms Character vector
#' @param events Character vector
#' @param colClasses Character vector
#' @param group_by Character vector
#' @param var_roots Character vector
#' 
#' @param overwriteBehavior Character, defined inputs, length == 1
#' @param returnContent Character, defined inputs, length == 1
#' @param error_handling Character, defined inputs, length == 1
#' 
#' @param survey Logical, length == 1 
#' @param dag Logical, length == 1
#' @param form_complete_auto Logical, length == 1
#' @param format Logical, length == 1
#' @param factors Logical, length == 1
#' @param labels Logical, length == 1
#' @param dates Logical, length == 1
#' @param checkboxLabels Logical, length == 1
#' @param returnData Logical, length == 1
#' @param plot Logical, length == 1
#' @param unfiltered Logical, length == 1
#' @param long_format Logical, length == 1
#' @param table Logical, length == 1
#' @param numeric_only Logical, length == 1
#' 
#' @param record_data Data.frame; contains record_id and redcap_event_name columns
#' @param data_dict Data.frame, ncol == 18
#' @param users Data.frame
#' @param form_perm Data.frame
#' @param instruments Data.frame
#' @param event_data Data.frame
#' @param arms Data.frame
#' @param mappings Data.frame
#' @param proj_info Data.frame
#' 
#' @param bundle List; redcapBundle
#' @param fields_list List
#' 
#' @param sex_var Character (length == 1) or NA
#' 
#' @author Marcus Lehr

validate_args <- function(required = NULL,
                          
                          # Numerical, len=1
                          report_id = NULL,
                          batch.size = NULL,
                          sd_threshold = NULL,
                          
                          # Character, len=1
                          url = NULL,
                          token = NULL,
                          id_field = NULL,
                          logfile = NULL,
                          completion_field = NULL,
                          
                          # Character
                          records = NULL,
                          fields = NULL,
                          forms = NULL,
                          events = NULL,
                          colClasses = NULL,
                          group_by = NULL,
                          var_roots = NULL,
                          
                          # Match Args
                          overwriteBehavior = NULL,
                          returnContent = NULL,
													error_handling = NULL,
                          
                          # Logical
                          survey = NULL,
                          dag = NULL,
                          form_complete_auto = NULL,
                          format = NULL,
                          factors = NULL,
                          labels = NULL,
                          dates = NULL,
                          checkboxLabels = NULL,
                          returnData = NULL,
                          plot = NULL,
                          unfiltered = NULL,
                          long_format = NULL,
													table = NULL,
													numeric_only = NULL,
                          
                          # Data.frame
                          record_data = NULL,
                          data_dict = NULL,
                          users = NULL,
                          form_perm = NULL,
                          instruments = NULL,
                          event_data = NULL,
                          arms = NULL,
                          mappings = NULL,
                          proj_info = NULL,
                          
                          # List
                          bundle = NULL,
                          fields_list = NULL,
                          
                          # Special
                          sex_var = NULL
) {

# Self Checks -------------------------------------------------------------
  
  # Create error collection object
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(required, null.ok = T, add = coll)
  
  # Report
  checkmate::reportAssertions(coll)
  

# Numerical, len=1-------------------------------------------------------------------------

  # Input vars
  vars = c('report_id','batch.size','sd_threshold')
		
		# Make formula
		massert_formula = stats::formula(paste('~',paste(vars,collapse = ' + ')))
		
		# Generate null.ok list
		null.ok = as.list(!vars %in% required)
		names(null.ok) = vars
		
		#Assert
		massert(massert_formula, 
						checkmate::assert_numeric,
						null.ok = null.ok,
						fixed = list(len = 1,
												 add = coll))

# Character, len=1 -------------------------------------------------------------------------

  # Input vars
  vars = c('url','token','id_field','logfile','completion_field')
  
		# Make formula
		massert_formula = stats::formula(paste('~',paste(vars,collapse = ' + ')))
		
		# Generate null.ok list
		null.ok = as.list(!vars %in% required)
		names(null.ok) = vars
		
		# Assert
		massert(massert_formula,
						checkmate::assert_character,
						null.ok = null.ok,
						fixed = list(len = 1,
												 add = coll))

# Character ---------------------------------------------------------------
  
  # Generate var list
  vars = c('records','fields','forms','events','colClasses','group_by','var_roots')
		
		# Make formula
		massert_formula = stats::formula(paste('~',paste(vars,collapse = ' + ')))
		
		# Generate null.ok list
		null.ok = as.list(!vars %in% required)
		names(null.ok) = vars
		
		# Assert
		massert(massert_formula,
						checkmate::assert_character,
						null.ok = null.ok,
						fixed = list(add = coll))

# Match Args ------------------------------------------------------

  # Generate var list
  vars = c('overwriteBehavior','returnContent', 'error_handling')
	
	if (any(vars %in% required)) {
	
		# Make formula
		massert_formula = stats::formula(paste('~',paste(vars,collapse = ' + ')))
		
		# Assert
		massert(massert_formula,
						checkmate::matchArg,
						choices = list(overwriteBehavior = c('normal','overwrite'),
													 returnContent = c('count', 'ids', 'nothing'),
													 error_handling = c('null','error')),
						fixed = list(several.ok = T,
												 add = coll))
	}

# Logical ------------------------------------------------------------------

  # Generate var list
  vars = c('survey','dag','form_complete_auto','format','factors','labels','dates',
           'checkboxLabels','returnData','plot','unfiltered','long_format','table',
					 'numeric_only')
		
		# Make formula
		massert_formula = stats::formula(paste('~',paste(vars,collapse = ' + ')))
		
		# Generate null.ok list
		null.ok = as.list(!vars %in% required)
		names(null.ok) = vars
		
		# Assert
		massert(massert_formula,
						checkmate::assert_logical,
						null.ok = null.ok,
						fixed = list(len = 1,
												 add = coll))  

# Data.frame --------------------------------------------------------------

  # Generate var list
  vars = c('record_data','data_dict','users','form_perm','instruments','event_data',
						'arms','mappings','proj_info')
		
		# Make formula
		massert_formula = stats::formula(paste('~',paste(vars,collapse = ' + ')))
		
		# Generate null.ok list
		null.ok = as.list(!vars %in% required)
		names(null.ok) = vars
		
		# Assert
		massert(massert_formula,
						checkmate::assert_class,
						null.ok = null.ok,
						fixed = list(classes = 'data.frame',
												 add = coll))

# List --------------------------------------------------------------------

  # Generate var list
  vars = c('bundle','fields_list')
		
		# Make formula
		massert_formula = stats::formula(paste('~',paste(vars,collapse = ' + ')))
		
		# Generate null.ok list
		null.ok = as.list(!vars %in% required)
		names(null.ok) = vars
		
		# Assert
		massert(massert_formula,
						checkmate::assert_list,
						null.ok = null.ok,
						fixed = list(add = coll))

# Special Checks ----------------------------------------------------------

##--- token
  if (!is.null(token))
    if (!grepl("[[:alnum:]]{32}",token))
				coll$push("REDCap tokens must be exactly 32 alpha-numeric characters.")
  
##--- sex_var
	if (!is.null(sex_var))
		if (!is.na(sex_var))
			checkmate::assert_character(sex_var,
																	len = 1,
																	null.ok = !sex_var %in% required,
																	add = coll)
  
##--- bundle
  if (!is.null(bundle)) {
    checkmate::assert_class(bundle, classes = 'redcapBundle', add = coll)
		
		bundle_names = c("redcap_url","data_dict","id_field","users","form_perm","instruments",
										 "event_data","arms","mappings","proj_info","version")
		
		if (length(bundle) != 11 | 
				any(!bundle_names %in% names(bundle)))
			warning("Please supply a bundle exactly as produced by rc_setup()")
  }
  
##--- record_data
  if (!is.null(record_data)) {
    id_field = suppressWarnings(getID(record_data))
    if (!id_field %in% names(record_data) |
        !'redcap_event_name' %in% names(record_data))
      coll$push("Record_data must contain the record_id and 'redcap_event_name' columns.")
  }
  
##--- data_dict validations
  if (!is.null(data_dict)) {
		# If data_dict has been exported via REDCap GUI and imported with read.csv/read_csv,
		# rename columns names with those of REDCap API export
		data_dict_api_names = c('field_name','form_name','section_header','field_type','field_label',
			'select_choices_or_calculations','field_note','text_validation_type_or_show_slider_number',
			'text_validation_min', 'text_validation_max', 'identifier','branching_logic', 'required_field',
			'custom_alignment','question_number', 'matrix_group_name', 'matrix_ranking','field_annotation')
												
		data_dict_read.csv_names = c("ï..Variable...Field.Name","Form.Name","Section.Header",
			"Field.Type","Field.Label","Choices..Calculations..OR.Slider.Labels","Field.Note",
			"Text.Validation.Type.OR.Show.Slider.Number","Text.Validation.Min","Text.Validation.Max",
			"Identifier.","Branching.Logic..Show.field.only.if....","Required.Field.","Custom.Alignment",
			"Question.Number..surveys.only.","Matrix.Group.Name","Matrix.Ranking.","Field.Annotation")
			
		data_dict_read_csv_names = c("Variable / Field Name","Form Name","Section Header",
			"Field Type","Field Label","Choices, Calculations, OR Slider Labels","Field Note",
			"Text Validation Type OR Show Slider Number","Text Validation Min","Text Validation Max",
			"Identifier?","Branching Logic (Show field only if...)","Required Field?","Custom Alignment",
			"Question Number (surveys only)","Matrix Group Name","Matrix Ranking?","Field Annotation")
		
		if (identical(names(data_dict)[2:18], 
		              data_dict_read.csv_names[2:18]) | # For some reason the first field breaks this condition when
																										# calling from the function envir. Removing just the 'ï' 
																										# doesn't work
				identical(names(data_dict), data_dict_read_csv_names) &
				length(data_dict) == 18) {
		  names(data_dict) = data_dict_api_names
			data_dict <<- data_dict
			}
		else if (any(!data_dict_api_names %in% names(data_dict)))
      coll$push("Please supply a data dictionary exactly as produced by REDCap
			or via a REDCap bundle, as created by rc_setup()")
			
		# Check for bad fields
		if (!is.null(fields)){
      # Generate list of fields plus checkbox fields
			fields_valid = names(checkbox_suffixes(field_names = data_dict$field_name,
																						 data_dict = data_dict))
			
			# Add redcap and _complete fields
      fields_valid = c(fields_valid,
                       'redcap_event_name','redcap_repeat_instrument','redcap_repeat_instance',
											 sprintf("%s_complete", unique(data_dict$form_name))) %>% 
											stats::na.omit() # Not sure this is necessary			
        
      # Find any fields not in the meta data
      fields_bad = setdiff(fields, fields_valid)
      
      if (length(fields_bad) > 0)
        coll$push(paste0("The following are not valid field names: ",
                    paste0(fields_bad, collapse = ", ")))
    }
		
		# Check for bad forms
		if (!is.null(forms)) {
            forms_bad <- forms[!forms %in% data_dict$form_name]
            if (length(forms_bad) > 0)
              coll$push(paste0("The following are not valid form names: ",
                          paste0(forms_bad, collapse = ", ")))
          }
    
    ## These are the col names exported from RC, changing shouldn't be necessary.
		## Also, changing from here would require exporting to the parent environment
    # names(data_dict) <- data_dict_names
  }
	else if (!is.null(fields) | !is.null(forms))
		warning("The supplied fields or forms cannot be validated without the project
		data dictionary. Please supply it directly or via a REDCap bundle, as created by
		rc_setup()")
	
	
##--- event_data validations
	# Check for bad events
	if (!is.null(event_data)) {
		if (!is.null(events)) {
      events_list <- event_data$unique_event_name
      events_bad <- events[!events %in% events_list]
      if (length(events_bad) > 0)
        coll$push(paste0("The following are not valid event names: ",
                    paste0(events_bad, collapse = ", ")))
		}
  }
  else if (!is.null(events))
		warning("The supplied events list cannot be validated without the project
		events data. Please supply it directly or via a REDCap bundle, as created by
		rc_setup()")

# Report ------------------------------------------------------------------

  # Final report
  checkmate::reportAssertions(coll)
}

