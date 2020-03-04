#' @name rc_pool
#'
#' @title Pools columns for aggregated analysis of fields with like data
#' @description For each variable root provided, all column names in the 
#' record_data containing that root will be pooled into a single column and
#' appended to the end of the dataframe. Roots provided via \code{var_roots} 
#' will only be matched to columns containing numeric data to reduce false matches.
#' An attribute describing the columns pooled will be appended to the dataframe,
#' which can be accessed via the command 
#' 
#' Additionally, exact (i.e. full name) matching can be performed with the 
#' \code{fields_list} argument. Fields provided in this argument will be searched
#' for in all columns. If both arguments are provided, \code{fields_list} will be 
#' applied first. 
#' 
#' Furthermore, if the columns selected to be pooled contain more than one 
#' data point per row, the first data point will be used. In this case, pooling is 
#' likely inappropriate and the pooled columns should be reviewed. To see which columns
#' have been pooled, run the command \code{attributes([YOUR_DATA_FRAME])$pooled_vars}
#' on the returned dataframe. If for some reason pooling is still desirable and all data
#' points should be kept, use \code{long_format = TRUE} for more aggressive pooling.
#' 
#' @details The intention of this function is to correct for inefficient
#' REDCap project design where the same data measurement has been assigned to
#' multiple variables. For example, if the variables "visit_1_weight" and 
#' "visit_2_weight" have been used to collect weight at different visits rather
#' than re-using the same variable, they can be pooled into a single column using 
#' the \code{var_root} "weight". This is often desirable for analysis.
#' 
#' 
#' @param record_data Dataframe. Records data export from REDCap. For the
#' purposes of this function, only quantitative data will be kept. 
#' @param var_roots Character. Strings to search for within column names of
#' record_data. For each variable root provided, all column names containing the
#' root will be pooled into a single column. In order to prevent inappropriate
#' matches, the search space will be restricted to columns which appear to contain
#' numerical data. 
#' @param fields_list List. A list in the format \code{list(new_column = c("old","column","names"))}.
#' Unlike \code{var_roots}, the column names provided here will be matched exactly.
#' In addition, if both \code{var_roots} and \code{fields_list} are provided, 
#' \code{fields_list} will be applied first.
#' @param long_format Logical. Determines whether the returned dataframe will
#' be in long or wide format. Default is \code{FALSE}. This option is useful for more
#' aggressive pooling- particularly when there are same-row data points that will be
#' lost in wide format.
#' @param id_field Character. Field name corresponding to the 'record_id' field.
#' 
#' @importFrom magrittr '%>%'
#' 
#' @author Marcus Lehr
#' 
#' @export


rc_pool <- function(record_data, var_roots = NULL, fields_list = NULL,
                         long_format = FALSE,
                         id_field = getOption("redcap_bundle")$id_field) {
  
  validate_args(required = c('record_data'),
                record_data = record_data,
                var_roots = var_roots,
                fields_list = fields_list,
                long_format = long_format,
                id_field = id_field)
  
  if (is.null(var_roots) & is.null(fields_list))
    stop("At least one of either var_roots or fields_list must be provided.")
  
  # Get ID field names
  id_field = getID(record_data, id_field)
  rc_fields = c('redcap_event_name','redcap_repeat_instrument','redcap_repeat_instance')
  meltVars = c(id_field, rc_fields)[c(id_field, rc_fields) %in% names(record_data)]
  
  # Instantiate list to log the affected fields
  fields_changed = list()
  
  if (!is.null(fields_list)) {
    
    # Drop fields not found in record_data
    fields_bad = lapply(fields_list, function(x) x[!x %in% names(record_data)]) %>% unlist()
    
    # Notify of bad field names
    if (length(fields_bad) > 0) {
      message("The following supplied fields were not found in record_data and will be dropped:\n",
              paste(fields_bad, collapse = ', '))
      fields_list = lapply(fields_list, function(x) setdiff(x, fields_bad))
    }
  }
  
  if (long_format) {
    
    # Melt data
    molten_data = suppressWarnings(
                    reshape2::melt(record_data, id.vars = meltVars, na.rm = T) %>% droplevels()
                  )
    
    if (!is.null(fields_list)) {
      
      for (f in 1:length(fields_list)) {
        # Get column names
        cols = levels(molten_data$variable)[grepl(paste0('^',fields_list[[f]],'$', collapse = '|'), 
                                                          levels(molten_data$variable))]
                                                    
        if (length(cols) > 0) {
          # Note the variables which will be changed
          fields_changed[[names(fields_list)[f]]] = cols
          
          # Change variable names
          levels(molten_data$variable)[grepl(paste0('^',fields_list[[f]],'$', collapse = '|'), 
                                             levels(molten_data$variable))] = names(fields_list)[f]
        }
      }
    }
    
    if (!is.null(var_roots)) {
      
      # If search terms are used as names in fields_list, the record of changed columns will be overwritten
      if (!is.null(fields_list)) 
        if (any(var_roots %in% names(fields_list))) 
          stop("Search terms specified in var_roots cannot exist in names(fields_list)")
      
      # Select numeric column names. I don't think this function makes sense with other data
      # types and this restricts the search space.
      num_cols = names(numeric_only(record_data, long_format = F, drop_message = F))
      
      for (r in var_roots) {
        # Get column names
        cols = num_cols[grepl(r, num_cols)]
        
        if (length(cols) > 0) {
          # Note the variables which will be changed
          fields_changed[[r]] = cols
          
          # Change variable names
          levels(molten_data$variable)[grepl(paste0('^',fields_changed[[r]],'$', collapse = '|'), 
                                             levels(molten_data$variable))] = r
        }
      }
    }
    
    # Append record of pooled vars
    attr(molten_data, 'pooled_vars') = fields_changed
    message("An attribute describing the variables pooled has been appended to the dataframe. 
            To view, use attributes(record_data)$pooled_vars")
    
    return(molten_data)
  }
  
  else {
    
    if (!is.null(fields_list)) {
      
      for (f in names(fields_list)) {
        # Get column names
        cols = fields_list[[f]]
        cols = cols[cols %in% names(record_data)]
        
        if (length(cols) > 0) {
          
          # Note variables to be changed
          fields_changed[[f]] = cols
          
          # Warn about multiple data points in rows
          if (any(record_data[cols] %>% is.na() %>% rowSums() < length(record_data[cols]) - 1))
            warning("Same row data points were found within columns: ", paste(cols, collapse = ', '),
                    "\nData from the first column will override other columns. If all data points must be kept,
                    use long_format = TRUE")
          
          # Pool
          record_data = dplyr::mutate(record_data, !!f := dplyr::coalesce(!!!as.list(record_data[,cols])))
          
          # Remove old columns
          record_data = dplyr::select(record_data, -all_of(cols))
        }
      }
    }
    
    if (!is.null(var_roots)) {
      
      # Select numeric data. I think it only makes sense with numeric columns 
      # and other types can cause issues
      num_cols = names(numeric_only(record_data, long_format = F, drop_message = F))
      
      # Assemble combined variables
      for (r in var_roots) {
        
        # Get matching column names
        cols = num_cols[grepl(r, num_cols)]
        
        if (length(cols)>0) {
          # Validate cols are all the same class?
          
          # Note variables to be changed
          fields_changed[[r]] = cols
          
          # Warn about multiple data points in rows
          if (any(record_data[cols] %>% is.na() %>% rowSums() < length(record_data[cols]) - 1))
            warning("Same row data points were found within columns: ", paste(cols, collapse = ', '),
                    "\nData from the first column will override other columns. If all data points must be kept,
                    use long_format = TRUE")
          
          # Pool
          record_data = dplyr::mutate(record_data, !!r := dplyr::coalesce(!!!as.list(record_data[,cols])))
           
          # Remove old columns
          record_data = dplyr::select(record_data, -all_of(cols))
        }
        else message("No columns containing root '", paste(r), "' were found.")
      }
    }
    
    # Append record of pooled vars
    attr(record_data, 'pooled_vars') = fields_changed
    message("An attribute describing the variables pooled has been appended to the dataframe. 
            To view, use attributes(record_data)$pooled_vars")
    
    return(record_data)
  }
}
