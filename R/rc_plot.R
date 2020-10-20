#' @name rc_plot
#'
#' @title Plot long format data
#' @description  Creates a grid of plots for each variable passed from
#' \code{rc_outliers}. *NOTE* In order for this function to be useful, the
#' argument \code{unfiltered} should be set to \code{FALSE} when running 
#' \code{rc_outliers}. Outliers are colored in red and horizontal indicator
#' lines for +/- 3 standard deviations are included. If a \code{sex_var} is
#' included, a list object will be returned containing a plot for each sex.
#' 
#' @param molten_data Dataframe. A long format data frame, as created by 
#' \code{rc_outliers} using the argument \code{unfiltered = T}.
#' @param title Character. Title of the resulting plot.
#' @param plot_type Character. One of 'standard', 'qq', or 'hist'
#' @param outlier_var Character. Name of the column containing logical (T/F)
#' data indicating which values are outliers. Change to NA when not in use.
#' @param wrap_var Character. Name of the column containing variable names.
#' @param y Character. Name of the column containing numeric data to
#' be plotted. 
#' @param x Character. Field name corresponding to the 'record_id' field.
#' 
#' @author Marcus Lehr
#' @export


rc_plot <- function(molten_data,
                    title = NA,
                    plot_type = 'standard',
                    outlier_var = 'outlier',
                    wrap_var = 'variable', 
                    y = 'value', 
                    x = getOption("redcap_bundle")$id_field) {

  # Checks ------------------------------------------------------------------
  
  x = getID(record_data = molten_data,
                   id_field = x)
  
  ## Need to update validate_args
  # validate_args(required = c('molten_data', 'y', 'outlier_var'),
  #               molten_data = molten_data,
  #               wrap_var = wrap_var,
  #               y = y,
  #               outlier_var = outlier_var,
  #               title = title,
  #               x = x,
  #               plot_type = plot_type)
  
  
  # Prep vars --------------------------------------------------------------
  
  # Select relevant data.
  molten_data = molten_data %>% dplyr::ungroup() %>% dplyr::arrange_at(outlier_var) %>% # Make sure outlier points are plotted on top
                  # dplyr::select_at(stats::na.omit(c(x, y, wrap_var, outlier_var))) %>%
                  dplyr::filter(!is.na(!!dplyr::sym(y))) # Remove NAs to avoid issues with mean/sd functions
  
  # Change NAs to FALSE in outlier columns. Otherwise points won't be plotted
  if (!is.na(outlier_var) & plot_type!='hist')
    molten_data = molten_data %>% dplyr::mutate_at(outlier_var, ~ ifelse(is.na(.),F,.))
  
  # This was an attempt to show diff colors for each outlier combo
  {
  ## Make a new column indicating which outlier type the point is (will create duplicate rows where needed)
  # if (length(outlier_var)>1) {
  #   out_data = reshape2::melt(molten_data, measure.vars = outlier_var, 
  #                                 value.name = 'out', variable.name = 'out_type')
  #   molten_data = dplyr::filter(out_data, out==T) %>% dplyr::select(-out) %>% 
  #                   dplyr::right_join(molten_data) #%>% 
  #                   # dplyr::mutate(out_type = ifelse(is.na(out_type),F,out_type))
  # }
  ## Make a color palette to feed to scale_color_manual()
  # molten_data = mutate(molten_data, hex_codes = rgb(outlier, qq_out, out_of_range))
  # color_palette = unique(molten_data$hex_codes) %>% as.character()
  # names(color_palette) = unique(molten_data$hex_codes) %>% as.character()
  } 
  
  # Convert IDs to character to treat as categorical
  molten_data[[x]] = as.character(molten_data[[x]])
  
  # Get list of variables
  vars = as.character(unique(molten_data[[wrap_var]]))
  
  # Add qq data
  if (plot_type=='qq') {
    molten_data = molten_data %>% dplyr::group_by_at(wrap_var) %>% dplyr::arrange_at(y) %>% 
                    dplyr::mutate(probs = (1:dplyr::n())/(dplyr::n()+1),
                                  # norm_quants = stats::qnorm(probs, mean(!!dplyr::sym(y)), stats::sd(!!dplyr::sym(y)))
                                   )
    molten_data$norm_quants = NA
    for (v in vars) {
      sel_vector = molten_data[[wrap_var]]==v
      
      molten_data$norm_quants[sel_vector] = 
        stats::qnorm(molten_data$probs[sel_vector], 
                     mean(molten_data[[y]][sel_vector]),
                     stats::sd(molten_data[[y]][sel_vector]))
    }
  }
    
  
# Plot loop ---------------------------------------------------------------

  plots = list()
  
  # List of layers applied to all plots
  # Could potentially use tidy dots to allow users to add more layers
  gglayers = list(ggplot2::scale_color_manual(breaks = c("FALSE","TRUE"), values = c("black","red")),
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
  
  if (plot_type!='hist') {
    if (is.na(outlier_var)) gglayers = c(list(ggplot2::geom_point()), gglayers)
    else gglayers = c(list(ggplot2::geom_point(ggplot2::aes_string(color = paste0(outlier_var, collapse = '|')))),
                      gglayers)
  }
  
  
  if (plot_type=='qq') {
    for (v in vars) {
      pData = dplyr::filter(molten_data, !!dplyr::sym(wrap_var) == v)
      
      plots[[v]] =
        ggplot2::ggplot(pData, 
                        ggplot2::aes_string(x = 'norm_quants', y = y
                                            )
                        ) +
         gglayers +
         ggplot2::geom_abline(slope = 1, intercept = 0) +
         ggplot2::ylab(v)
    }
  } 
  else if (plot_type=='hist') {
    for (v in vars) {
      pData = dplyr::filter(molten_data, !!dplyr::sym(wrap_var) == v)
      
      plots[[v]] =
        ggplot2::ggplot(pData, ggplot2::aes_string(x = y)) +
        ggplot2::geom_histogram() +
        ggplot2::ylab(v) +
        gglayers +
        ggplot2::theme(axis.text.x = ggplot2::element_text(),
                       axis.ticks.x = ggplot2::element_line())
    }
  }
  else {
    for (v in vars) {
      
      # Filter data
      pData = dplyr::filter(molten_data, !!dplyr::sym(wrap_var) == v)
      
      #Create plot
      plots[[v]] =
        ggplot2::ggplot(pData, ggplot2::aes_string(x = x, y = y)) +
        ggplot2::ylab(v) + 
        gglayers +
        
        # Standard deviation lines
        ggplot2::geom_hline(yintercept = mean(pData[[y]]), 
                            color = '#ffffff', linetype= 3, alpha= 0.25)+
        
        ggplot2::geom_hline(yintercept = c(mean(pData[[y]]) + 1*stats::sd(pData[[y]]),
                                           mean(pData[[y]]) - 1*stats::sd(pData[[y]])),
                            color = '#ff8080', linetype= 3, alpha= 0.25) +
        
        ggplot2::geom_hline(yintercept = c(mean(pData[[y]]) + 2*stats::sd(pData[[y]]),
                                           mean(pData[[y]]) - 2*stats::sd(pData[[y]])),
                            color = '#ff4040', linetype= 3, alpha= 0.25) +
        
        ggplot2::geom_hline(yintercept = c(mean(pData[[y]]) + 3*stats::sd(pData[[y]]),
                                           mean(pData[[y]]) - 3*stats::sd(pData[[y]])), 
                            color = '#ff0000', linetype= 3, alpha= 0.25)
    }
  }
  
  # # Attempt to add common legend to grid.arrange plot. Not working
  ## facet_wrap() handles this appropriately
  # #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  # g_legend<-function(a.gplot){
  #   tmp <- ggplot_gtable(ggplot_build(a.gplot))
  #   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  #   legend <- tmp$grobs[[leg]]
  #   return(legend)}
  # 
  # legend<-g_legend(plots[[4]])
  
  #Return plot
  gridExtra::grid.arrange(grobs = plots, top = title)
}
