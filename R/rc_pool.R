#' @name rc_pool
#'
#' @title Pools columns for aggregated analysis of fields with like data
#' @description For each variable root provided, all column names in the 
#' record_data containing that root will be pooled into a single column and
#' appended to the end of the dataframe. To see which columns have been pooled, 
#' run the command \code{attributes([YOUR_DATA_FRAME])$pooled_vars} on the 
#' returned dataframe.
#' 
#' If desired, the search space of \code{var_roots} can be restricted to only
#' columns containing numeric data to reduce false matches. Additionally, exact 
#' (i.e. full name) matching can be performed with the \code{fields_list} argument. 
#' Fields provided in this argument will be searched for in all columns. If both 
#' arguments are provided, \code{fields_list} will be applied first. 
#' 
#' Furthermore, if the columns selected to be pooled contain more than one 
#' data point per row, the first data point will be used. In this case, pooling is 
#' likely inappropriate and the pooled columns should be reviewed. However, if for 
#' some reason pooling is still desirable and all data points should be kept, 
#' use \code{make_repeat = TRUE} for more aggressive pooling.
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
#' root will be pooled into a single column. Regular expressions may be used.
#' @param fields_list List. A list in the format \code{list(new_column = c("old","column","names"))}.
#' Unlike \code{var_roots}, the column names provided here will be matched exactly.
#' In addition, if both \code{var_roots} and \code{fields_list} are provided, 
#' \code{fields_list} will be applied first.
#' @param make_repeat Logical. Determines whether the pooled columns will be converted
#' into repeat instruments. Default is \code{FALSE}. This option is useful for when 
#' there are same-row data points within columns to be pooled. In the future, this
#' will be implemented automatically on an as-needed basis.
#' @param id_field Character. Field name corresponding to the 'record_id' field.
#' 
#' @importFrom magrittr '%>%'
#' 
#' @author Marcus Lehr
#' 
#' @export


rc_pool <- function(record_data, var_roots = NULL, fields_list = NULL,
                    make_repeat = FALSE,
                    id_field = getOption("redcap_bundle")$id_field) {
  
  validate_args(required = c('record_data'),
                record_data = record_data,
                var_roots = var_roots,
                fields_list = fields_list,
                make_repeat = make_repeat,
                numeric_only = numeric_only,
                id_field = id_field)
  
  if (is.null(var_roots) & is.null(fields_list))
    stop("At least one of either var_roots or fields_list must be provided.")
  
  # Get ID field names
  id_field = getID(id_field = id_field,
                   record_data = record_data)
  rc_fields = c('redcap_event_name','redcap_repeat_instrument','redcap_repeat_instance')
  meltVars = c(id_field, rc_fields)[c(id_field, rc_fields) %in% names(record_data)]
  
  # Instantiate list to log the affected fields
  fields_changed = data.frame()
  
  # Check for errors in fields_list
  if (!is.null(fields_list)) {
    
    # Drop fields not found in record_data
    fields_bad = lapply(fields_list, function(x) x[!x %in% names(record_data)]) %>% unlist()
    
    # Notify of bad field names
    if (length(fields_bad) > 0) {
      message("The following supplied fields were not found in record_data and will be dropped:\n",
              paste(fields_bad, collapse = ', '))
      fields_list = lapply(fields_list, function(x) setdiff(x, fields_bad))
      
      # If search terms are used as names in fields_list, the record of changed columns will be overwritten
      if (!is.null(var_roots))
        if (any(var_roots %in% names(fields_list))) 
          stop("Search terms specified in var_roots cannot exist in names(fields_list)")
    }
  }
  
  # Repeat method
  if (make_repeat) {
    
    # Add missing (repeat) columns. There must be a cleaner way to do this..
    if (!all(rc_fields %in% names(record_data))) {
      rc_cols = matrix(ncol = length(setdiff(rc_fields,names(record_data)))) %>% as.data.frame()
      names(rc_cols) = setdiff(rc_fields,names(record_data))
      record_data = cbind(record_data,rc_cols) %>% dplyr::select(!!meltVars,dplyr::everything())
    }
    
    # Empty columns will be of type logical and cause join conflicts
      record_data = dplyr::mutate_at(record_data, dplyr::vars(contains('repeat')), as.character)
    
    if (!is.null(fields_list)) {
      
      for (f in 1:length(fields_list)) {
        # Get column names
        cols = intersect(fields_list[[f]], names(record_data))
        
        if (length(cols) > 0) {
          
          # Note the variables which will be changed
          fields_changed = rbind(fields_changed, data.frame(pooled_vars = names(fields_list)[f]),rc_vars = cols)
          
          # Collect data to be converted, remove empty rows
          pool_data = record_data[c(meltVars, cols)] %>% dplyr::filter_at(cols,dplyr::any_vars(!is.na(.)))
          
          # Melt and fill in repeat columns
          pool_data = reshape2::melt(pool_data, id.vars = meltVars, na.rm = T)
          pool_data$redcap_repeat_instrument = names(fields_list[f])
          # levels(pool_data$variable) = 1:length(levels(pool_data$variable))
          pool_data = pool_data %>% dplyr::mutate(redcap_repeat_instance = as.character(variable)) %>% 
                        dplyr::rename(!!names(fields_list[f]) := value) %>% dplyr::select(-variable)
          
          # Add back to data and remove original columns
          record_data = record_data %>% dplyr::select(-!!cols) %>% dplyr::full_join(pool_data, by = meltVars) %>% 
                          dplyr::arrange_at(meltVars[1:2])
        }
      }
    }
    
    if (!is.null(var_roots)) {
      
      for (r in var_roots) {
        # Get column names
        cols = stringr::str_subset(names(record_data), r)
          
          
          if (length(cols) > 0) {
            
            # Note the variables which will be changed
            fields_changed = rbind(fields_changed, data.frame(pooled_vars = r, rc_vars = cols))
            
            # Collect data to be converted, remove empty rows
            pool_data = record_data[c(meltVars, cols)] %>% dplyr::filter_at(cols,dplyr::any_vars(!is.na(.)))
            
            # Melt and fill in repeat columns
            pool_data = reshape2::melt(pool_data, id.vars = meltVars, na.rm = T)
            pool_data$redcap_repeat_instrument = r
            # levels(pool_data$variable) = 1:length(levels(pool_data$variable))
            pool_data = pool_data %>% dplyr::mutate(redcap_repeat_instance = as.character(variable)) %>% 
                            dplyr::rename(!!r := value) %>% dplyr::select(-variable)
            
            # Add back to data and remove original columns
            record_data = record_data %>% dplyr::select(-!!cols) %>% dplyr::full_join(pool_data, by = meltVars) %>% 
                            dplyr::arrange_at(meltVars[1:2])
          }
      }
    }
    
    # Append record of pooled vars
    attr(record_data, 'pooled_vars') = fields_changed
    message("An attribute describing the variables pooled has been appended to the dataframe. 
            To view, use attributes(record_data)$pooled_vars")
    
    return(record_data)
  }
  
  # Wide format method
  else {
    
    if (!is.null(fields_list)) {
      
      for (f in names(fields_list)) {
        # Get column names
        cols = fields_list[[f]]
        cols = cols[cols %in% names(record_data)]
        
        if (length(cols) > 0) {
          
          # Note variables to be changed
          fields_changed = rbind(fields_changed, data.frame(pooled_vars = f,rc_vars = cols))
          # fields_changed[[f]] = cols
          
          # If selected columns are different classes, convert to character
          col_types = sapply(record_data[cols], function(x) paste(class(x),collapse = ' ')) %>% unlist()
          col_types = stringr::str_replace_all(col_types, "\\s?labelled\\s?", '') # Remove labelled flag in case of incomplete labelling
          
          if (length(unique(col_types)) > 1){
            message("Classes of columns matching the root ", r, " are non-identical. They will be coerced to character.")
            record_data[cols] = sapply(record_data[cols], function(x) x = as.character(x))
          }
          
          # Warn about multiple data points in rows
          if (any(record_data[cols] %>% is.na() %>% rowSums() < length(record_data[cols]) - 1))
            warning("Same row data points were found within columns: ", paste(cols, collapse = ', '),
                    "\nData from the first column will override other columns. If all data points must be kept,
                    use make_repeat = TRUE")
          
          # Pool
          record_data = dplyr::mutate(record_data, !!f := dplyr::coalesce(!!!as.list(record_data[,cols])))
          
          # Remove old columns. Explicitly add new col at end to prevent accident removal 
          # (ie when contained within 'cols')
          record_data = dplyr::select(record_data, -all_of(cols), all_of(f))
        }
      }
    }
    
    if (!is.null(var_roots)) {
      
      for (r in var_roots) {
        
        # Get column names
        cols = stringr::str_subset(names(record_data), r)
        
        if (length(cols) > 0) {
          
          # Note variables to be changed
          fields_changed = rbind(fields_changed, data.frame(pooled_vars = r,rc_vars = cols))
          # fields_changed[[r]] = cols
          
          # If selected columns are different classes, convert to character
          col_types = sapply(record_data[cols], function(x) paste(class(x),collapse = ' ')) %>% unlist()
          col_types = stringr::str_replace_all(col_types, "\\s?labelled\\s?", '') # Remove labelled flag in case of incomplete labelling
          
          if (length(unique(col_types)) > 1){
            message("Classes of columns matching the root ", r, " are non-identical. They will be coerced to character.")
            record_data[cols] = sapply(record_data[cols], function(x) x = as.character(x))
          }
          
          # Warn about multiple data points in rows
          if (any(record_data[cols] %>% is.na() %>% rowSums() < length(record_data[cols]) - 1))
            warning("Same row data points were found within columns: ", paste(cols, collapse = ', '),
                    "\nData from the first column will override other columns. If all data points must be kept,
                    use make_repeat = TRUE")
          
          # Pool
          record_data = dplyr::mutate(record_data, !!r := dplyr::coalesce(!!!as.list(record_data[,cols])))
          
          # Remove old columns. Explicitly add new col at end to prevent accident removal 
          # (ie when contained within 'cols')
          record_data = dplyr::select(record_data, -all_of(cols), all_of(r))
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
