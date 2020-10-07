#' @name rc_outliers_plot
#'
#' @title Plot data from rc_outliers
#' @description  Creates a grid of plots for each variable passed from
#' \code{rc_outliers}. *NOTE* In order for this function to be useful, the
#' argument \code{unfiltered} should be set to \code{FALSE} when running 
#' \code{rc_outliers}. Outliers are colored in red and horizontal indicator
#' lines for +/- 3 standard deviations are included. If a \code{sex_var} is
#' included, a list object will be returned containing a plot for each sex.
#' 
#' @param outlier_data Dataframe. A long format data frame, as created by 
#' \code{rc_outliers} using the argument \code{unfiltered = T}.
#' @param variable_var Character. Name of the column containing variable names.
#' @param value_var Character. Name of the column containing numeric data to
#' be plotted. 
#' @param outlier_var Character. Name of the column containing logical (T/F)
#' data indicating which values are outliers. 
#' @param sex_var String. Name of variable indicating the sex of subjects. If
#' included, a list object containing separate plots for each sex will be returned.
#' @param id_field Character. Field name corresponding to the 'record_id' field.
#' 
#' @author Marcus Lehr
#' 
#' @export


rc_outliers_plot <- function(outlier_data, 
                             variable_var = 'variable', value_var = 'value', 
                             outlier_var = 'outlier', sex_var = NA, 
													   id_field = getOption("redcap_bundle")$id_field) {

  # Checks ------------------------------------------------------------------
  
  id_field = getID(record_data = outlier_data,
                   id_field = id_field)
  
  ## Need to make sure this has been implemented in validate_args
  # validate_args(required = c('outlier_data', 'value_var', 'outlier_var'),
  #               outlier_data = outlier_data,
  #               variable_var = variable_var,
  #               value_var = value_var,
  #               outlier_var = outlier_var,
  #               sex_var = sex_var,
  #               id_field = id_field)
  
  # Move into validate_args?
  missing_fields = !c(variable_var,value_var,outlier_var) %in% names(outlier_data)
  
  if (any(missing_fields)) {
    
    # Remove unnecessary data and get column types
    outlier_data = dplyr::select(outlier_data, -contains('redcap'))
    col_types = sapply(outlier_data, class) 
    col_types[[id_field]] = NA # Ignore id_field
    if (!is.na(sex_var)) col_types[[sex_var]] = NA # Ignore sex field
    
    # Try to find missing columns and rename
    if (missing_fields[1]) {
      col_pos = which(grepl('character|factor', col_types))
      if (length(col_pos)==1) names(outlier_data)[col_pos] = variable_var
    }
    if (missing_fields[2]) {
      col_pos = which(grepl('numeric|integer|double', col_types))
      if (length(col_pos)==1) names(outlier_data)[col_pos] = value_var
    }
    if (missing_fields[3]) {
      col_pos = which(grepl('logical', col_types))
      if (length(col_pos)==1) names(outlier_data)[col_pos] = outlier_var
    }
    
    # Recheck for missing columns and exit if so
    missing_fields = !c(variable_var,value_var,outlier_var) %in% names(outlier_data)
    if (any(missing_fields))
      stop("Please provide a data.frame as produced by rc_outliers(record_data, filtered = FALSE).
            Otherwise, the following columns must be included:
              ID column
              'variable' - Containing variable/field names
              'value' - Containing values of the associated variables
              'outlier' - Logical TRUE/FALSE indicating whether value is an outlier")
  }
  
  # Prep vars --------------------------------------------------------------
  
  # Select relevant data.
  outlier_data = outlier_data %>% dplyr::ungroup() %>% 
                  dplyr::select_at(stats::na.omit(c(id_field, sex_var, variable_var, value_var, outlier_var))) %>%
                  dplyr::filter(!is.na(!!dplyr::sym(value_var))) # Remove NAs to avoid issues with mean/sd functions
  
  # Change NAs to FALSE in outlier column. Otherwise points won't be plotted
  outlier_data[[outlier_var]][is.na(outlier_data[[outlier_var]])] = F
  
  # Convert IDs to character to treat as categorical
  outlier_data[[id_field]] = as.character(outlier_data[[id_field]])
  
  # Grab list of sexes
  if (is.na(sex_var)) sexes = ''
  else sexes = levels(as.factor(outlier_data[[sex_var]]))
  
  # Get list of variables
  vars = as.character(unique(outlier_data[[variable_var]]))
  
  # List of layers applied to all plots
  gglayers = list(ggplot2::geom_point(),
                  ggplot2::scale_color_manual(breaks = c("FALSE","TRUE"), values = c("black","red")),
                  # scale_color_gradient2(low = "red", mid = "white", high = "red", limits = c(-3,3), na.value = 'red'),
                  ggplot2::theme_bw(),
                  ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                                  axis.text.x=ggplot2::element_blank(),
                                  legend.position = "none",
                                  panel.grid = ggplot2::element_blank(),
                                  panel.border = ggplot2::element_blank(),
                                  panel.background = ggplot2::element_rect(fill = "#F0F0F0"),
                                  axis.ticks.x = ggplot2::element_blank()
                            )
             )

# Plot loop ---------------------------------------------------------------

  plots = list()
  return_data = list()
  for (i in 1:length(sexes)) {
    for (j in 1:length(vars)) {
      
      # Filter data
      pData = dplyr::filter(outlier_data, !!dplyr::sym(variable_var) == vars[j])
      if (!is.na(sex_var)) pData = pData[pData[[sex_var]] %in% sexes[i],]
      
      #Create plot
      plots[[as.character(i)]][[vars[j]]] = 
        ggplot2::ggplot(pData, ggplot2::aes_string(x = id_field, y = value_var, color = outlier_var)) +
        ggplot2::ylab(vars[j]) + 
        gglayers +
        
        # Standard deviation lines
        ggplot2::geom_hline(yintercept = mean(pData[[value_var]]), color = '#ffffff', linetype= 3, alpha= 0.25)+
        
        ggplot2::geom_hline(yintercept = mean(pData[[value_var]]) + 1*stats::sd(pData[[value_var]]),
                            color = '#ff8080', linetype= 3, alpha= 0.25) +
        ggplot2::geom_hline(yintercept = mean(pData[[value_var]]) - 1*stats::sd(pData[[value_var]]),
                            color = '#ff8080', linetype= 3, alpha= 0.25) +
        
        ggplot2::geom_hline(yintercept = mean(pData[[value_var]]) + 2*stats::sd(pData[[value_var]]),
                            color = '#ff4040', linetype= 3, alpha= 0.25) +
        ggplot2::geom_hline(yintercept = mean(pData[[value_var]]) - 2*stats::sd(pData[[value_var]]),
                            color = '#ff4040', linetype= 3, alpha= 0.25) +
        
        ggplot2::geom_hline(yintercept = mean(pData[[value_var]]) + 3*stats::sd(pData[[value_var]]),
                            color = '#ff0000', linetype= 3, alpha= 0.25) +
        ggplot2::geom_hline(yintercept = mean(pData[[value_var]]) - 3*stats::sd(pData[[value_var]]), 
                            color = '#ff0000', linetype= 3, alpha= 0.25)
    }
    
    #Return plots
    if (!is.na(sex_var))
      return_data[[sexes[i]]] = gridExtra::grid.arrange(grobs = plots[[i]], top = paste(sexes[i], 'data'))
    else
      return(gridExtra::grid.arrange(grobs = plots[[i]]))
  }
  return(return_data)
}
