#' @name plot_outliers
#'
#' @title Plot data from rc_outliers
#' @description  Creates a grid of plots for each variable passed from
#' \code{rc_outliers}. Outliers are colored in red and horizontal indicator
#' lines for +/- 3 standard deviations are made. This is an internal
#' function only.
#' 
#' @param outlier_data Dataframe. As created by \code{rc_outliers}. Will be
#' used to color outliers in red. If unfiltered, it will be used in place of
#' record_data.
#' @param id_field Character. Field name corresponding to the 'record_id' field.
#' @param sex_var String. Name of variable indicating the sex of subjects. If
#' included, separate plots for each sex will be made.
#' 
#' @author Marcus Lehr


plot_outliers <- function(outlier_data, id_field, sex_var = NA) {
  
# Prep vars --------------------------------------------------------------
  
  # Convert IDs to character
  outlier_data[[id_field]] = as.character(outlier_data[[id_field]])
  
  # Grab list of sexes
  if (is.na(sex_var)) sexes = ''
  else sexes = levels(outlier_data[[sex_var]])
  
  vars = as.character(unique(outlier_data$variable))
  
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

  scPlotsRaw <- list()
  for (i in 1:length(sexes)) {
    for (j in 1:length(vars)) {
      
      # Filter data
      pData = dplyr::filter(outlier_data, variable == vars[j])
      if (!is.na(sex_var)) pData = pData[pData[[sex_var]] == sexes[i],]
      
      #Create plot
      scPlotsRaw[[as.character(i)]][[vars[j]]] = 
        ggplot2::ggplot(pData, ggplot2::aes(x = hnrcid, y = value, color = outlier)) +
        ggplot2::ylab(vars[j]) + 
        gglayers +
        
        # Standard deviation lines
        ggplot2::geom_hline(yintercept = mean(pData$value), color = '#ffffff', linetype= 3, alpha= 0.25)+
        
        ggplot2::geom_hline(yintercept = mean(pData$value) + 1*sd(pData$value), color = '#ff8080', linetype= 3, alpha= 0.25)+
        ggplot2::geom_hline(yintercept = mean(pData$value) - 1*sd(pData$value), color = '#ff8080', linetype= 3, alpha= 0.25)+
        
        ggplot2::geom_hline(yintercept = mean(pData$value) + 2*sd(pData$value), color = '#ff4040', linetype= 3, alpha= 0.25)+
        ggplot2::geom_hline(yintercept = mean(pData$value) - 2*sd(pData$value), color = '#ff4040', linetype= 3, alpha= 0.25)+
        
        ggplot2::geom_hline(yintercept = mean(pData$value) + 3*sd(pData$value), color = '#ff0000', linetype= 3, alpha= 0.25)+
        ggplot2::geom_hline(yintercept = mean(pData$value) - 3*sd(pData$value), color = '#ff0000', linetype= 3, alpha= 0.25)
    }
    #Display plots.
    gridExtra::grid.arrange(grobs = scPlotsRaw[[i]], top = paste(sexes[i], 'data'))
  }
}


# Recycle Bin -------------------------------------------------------------

  # if (!is.null(outlier_data$outlier)) record_data = outlier_data
  # else if (!is.null(record_data))
  #   # Retrieve numeric data from records
  #   record_data = numeric_data(record_data, data_dict, 
  #                            sex_var, fields)
  # else stop("Either REDCap records data or unfiltered outlier data, as created by rc_outliers(),
  #           must be provided.")
  # 
  # if (!is.null(outlier_data)) {
  #   # Remove form_name column if present
  #   try(dplyr::select(outlier_data, -form_name), silent = T)
  #   # Recreate outlier column
  #   record_data$outlier = do.call(paste0, record_data) %in% do.call(paste0, outlier_data)
  # }

# if (!is.null(outlier_data$outlier)) 
#   p = ggplot2::ggplot(pData, ggplot2::aes(x = hnrcid, y = value, color = outlier))
# else p = ggplot2::ggplot(pData, ggplot2::aes(x = hnrcid, y = value))