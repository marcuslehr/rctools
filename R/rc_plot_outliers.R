#' @name rc_plot_outliers
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
#' @param sex_var String. Name of variable indicating the sex of subjects. If
#' included, a list object containing separate plots for each sex will be returned.
#' @param id_field Character. Field name corresponding to the 'record_id' field.
#' 
#' @author Marcus Lehr
#' 
#' @export


rc_plot_outliers <- function(outlier_data, sex_var = NA, 
													    id_field = getOption("redcap_bundle")$id_field) {
  
# Prep vars --------------------------------------------------------------
  
  if (!'outlier' %in% names(outlier_data))
    stop("outlier_data must be an unfiltered data.frame, as produced by rc_outliers()
          using 'unfiltered = TRUE'")
  
  # Select relevant data.
  outlier_data = outlier_data %>% dplyr::ungroup() %>% 
                  dplyr::select_at(stats::na.omit(c(id_field, sex_var, 'variable', 'value', 'outlier'))) %>% 
                  stats::na.omit() # NA values will cause errors when making sd lines
  
  # Convert IDs to character to treat as categorical
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

  plots = list()
  return_data = list()
  for (i in 1:length(sexes)) {
    for (j in 1:length(vars)) {
      
      # Filter data
      pData = dplyr::filter(outlier_data, variable == vars[j])
      if (!is.na(sex_var)) pData = pData[pData[[sex_var]] == sexes[i],]
      
      #Create plot
      plots[[as.character(i)]][[vars[j]]] = 
        ggplot2::ggplot(pData, ggplot2::aes_string(x = id_field, y = 'value', color = 'outlier')) +
        ggplot2::ylab(vars[j]) + 
        gglayers +
        
        # Standard deviation lines
        ggplot2::geom_hline(yintercept = mean(pData$value), color = '#ffffff', linetype= 3, alpha= 0.25)+
        
        ggplot2::geom_hline(yintercept = mean(pData$value) + 1*stats::sd(pData$value), color = '#ff8080', linetype= 3, alpha= 0.25)+
        ggplot2::geom_hline(yintercept = mean(pData$value) - 1*stats::sd(pData$value), color = '#ff8080', linetype= 3, alpha= 0.25)+
        
        ggplot2::geom_hline(yintercept = mean(pData$value) + 2*stats::sd(pData$value), color = '#ff4040', linetype= 3, alpha= 0.25)+
        ggplot2::geom_hline(yintercept = mean(pData$value) - 2*stats::sd(pData$value), color = '#ff4040', linetype= 3, alpha= 0.25)+
        
        ggplot2::geom_hline(yintercept = mean(pData$value) + 3*stats::sd(pData$value), color = '#ff0000', linetype= 3, alpha= 0.25)+
        ggplot2::geom_hline(yintercept = mean(pData$value) - 3*stats::sd(pData$value), color = '#ff0000', linetype= 3, alpha= 0.25)
    }
    #Return plots
    if (!is.na(sex_var))
      return_data[[sexes[i]]] = gridExtra::grid.arrange(grobs = plots[[i]], top = paste(sexes[i], 'data'))
    else
      return(gridExtra::grid.arrange(grobs = plots[[i]]))
  }
  return(return_data)
}
