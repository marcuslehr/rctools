#' @name rc_plot
#'
#' @title Plot long format data
#' @description  Creates a grid of plots for each variable in long format
#' data. This function is intended to have data passed from \code{rc_outliers},
#' plotting the record_id field on the x-axis and values on the y-axis. 
#' Outliers are colored in red and horizontal indicator lines for +/- 3 
#' standard deviations are included by default. Aside from the standard
#' plots, normal QQ and histograms can also be created using \code{plot_type}.
#' 
#' @param long_data Dataframe. A long format data frame, as created by 
#' \code{rc_outliers}. At minimum there should be a record_id column, a
#' variable name column, and a value column. Wide-to-long conversion functions 
#' include reshape2::melt(), tidyr::gather(), and tidyr::pivot_longer().
#' @param title Character. Title of the resulting plot.
#' @param plot_type Character. One of 'standard', 'qq', or 'hist'
#' @param outlier_var Character. Name of the column containing logical (T/F)
#' data indicating which values are outliers.
#' @param wrap_var Character. Name of the column containing variable names.
#' @param y Character. Name of the column containing numeric data to
#' be plotted. 
#' @param x Character. Field name corresponding to the 'record_id' field.
#' @param legend_position Character. Determines the position of the plot
#' legend. One of 'none', 'top', 'bottom', 'left' or 'right'.
#' @param sd_guides Logical. Determines if standard deviation lines are
#' applied to plots. Only applies to \code{plot_type = 'standard'}.
#' 
#' @author Marcus Lehr
#' @export


rc_plot <- function(long_data,
                    title = NULL,
                    plot_type = c('standard','qq','hist'),
                    outlier_var = NA,
                    wrap_var = 'variable', 
                    y = 'value', 
                    x = getOption("redcap_bundle")$id_field,
                    legend_position = 'none',
                    sd_guides = TRUE) {

  # Checks ------------------------------------------------------------------
  
  x = getID(record_data = long_data,
                   id_field = x)
									 
	plot_type = plot_type[1]
  
  validate_args(required = c('long_data', 'y'),
                long_data  = long_data,
                title = title,
                plot_type = plot_type,
                outlier_var = outlier_var,
                wrap_var = wrap_var, 
                y = y, 
                x = x,
                legend_position = legend_position,
                sd_guides = sd_guides
                )
  
  
  # Prep data --------------------------------------------------------------
  
  # Select relevant data.
  long_data = long_data %>% dplyr::ungroup() %>% 
                  # dplyr::select_at(stats::na.omit(c(x, y, wrap_var, outlier_var))) %>%
                  dplyr::filter(!is.na(!!dplyr::sym(y))) # Remove NAs to avoid issues with mean/sd functions
  
  # Change NAs to FALSE in outlier columns. Otherwise those points won't be plotted
  if (!is.na(outlier_var) & plot_type!='hist')
    long_data = long_data %>% dplyr::mutate_at(outlier_var, ~ ifelse(is.na(.),F,.)) %>% # Substitute any NAs with F
                    dplyr::arrange_at(outlier_var) # Make sure outlier points are plotted on top
  
  # This was an attempt to show diff colors for each outlier combo
  {
  ## Make a new column indicating which outlier type the point is (will create duplicate rows where needed)
  # if (length(outlier_var)>1) {
  #   out_data = reshape2::melt(long_data, measure.vars = outlier_var, 
  #                                 value.name = 'out', variable.name = 'out_type')
  #   long_data = dplyr::filter(out_data, out==T) %>% dplyr::select(-out) %>% 
  #                   dplyr::right_join(long_data) #%>% 
  #                   # dplyr::mutate(out_type = ifelse(is.na(out_type),F,out_type))
  # }
  ## Make a color palette to feed to scale_color_manual()
  # long_data = mutate(long_data, hex_codes = rgb(outlier, qq_out, out_of_range))
  # color_palette = unique(long_data$hex_codes) %>% as.character()
  # names(color_palette) = unique(long_data$hex_codes) %>% as.character()
  } 
  
  # Convert IDs to character to treat as categorical
  long_data[[x]] = as.character(long_data[[x]])
  
  # Add qq data
  if (plot_type=='qq') {
    long_data = long_data %>% dplyr::group_by_at(wrap_var) %>% dplyr::arrange_at(y) %>% 
                    dplyr::mutate(probs = (1:dplyr::n())/(dplyr::n()+1),
                                  # Not working, see loop below
                                  # norm_quants = stats::qnorm(probs, mean(!!dplyr::sym(y)), stats::sd(!!dplyr::sym(y)))
                                   )
    long_data$norm_quants = NA
    for (v in unique(long_data[[wrap_var]])) {
      sel_vector = long_data[[wrap_var]]==v
      
      long_data$norm_quants[sel_vector] = 
        stats::qnorm(long_data$probs[sel_vector], 
                     mean(long_data[[y]][sel_vector]),
                     stats::sd(long_data[[y]][sel_vector]))
    }
  }
    
  
# Plot layers ---------------------------------------------------------------

  plots = list()
  
  # List of layers applied to all plots
  # Could potentially use tidy dots to allow users to add more layers
  gglayers = list(ggplot2::scale_color_manual(breaks = c("FALSE","TRUE"), values = c("black","red")),
                  ggplot2::theme_minimal(),
                  ggplot2::theme(#axis.title.x=ggplot2::element_blank(),
                                 axis.text.x=ggplot2::element_blank(),
                                 axis.ticks.x = ggplot2::element_blank(),
                                 legend.position = legend_position,
                                 panel.grid = ggplot2::element_blank(),
                                 panel.border = ggplot2::element_blank(),
                                 panel.background = ggplot2::element_rect(fill = "#F0F0F0", color = NA)
                  )
  )
  
  if (plot_type!='hist') {
    if (is.na(outlier_var)) gglayers = c(list(ggplot2::geom_point()), gglayers)
    else gglayers = c(list(ggplot2::geom_point(ggplot2::aes_string(color = paste0(outlier_var, collapse = '|')))),
                      gglayers)
  }
  

# Make plots -------------------------------------------------------------------

  
  if (plot_type=='qq') {
    return(
      ggplot2::ggplot(long_data, 
                      ggplot2::aes_string(x = 'norm_quants', y = y)) +
        ggplot2::geom_abline(slope = 1, intercept = 0) +
        ggplot2::facet_wrap(wrap_var, scales = 'free') +
        ggplot2::ggtitle(title) +
        gglayers
    )
  }
  
  else if (plot_type=='hist') {
    return(
    ggplot2::ggplot(long_data, ggplot2::aes_string(x = y)) +
      ggplot2::geom_histogram() +
      ggplot2::facet_wrap(wrap_var, scales = 'free') +
      ggplot2::ggtitle(title) +
      gglayers +
      ggplot2::theme(axis.text.x = ggplot2::element_text(),
                     axis.ticks.x = ggplot2::element_line())
    )
  }
  
  else {
    plot = ggplot2::ggplot(long_data, ggplot2::aes_string(x = x, y = y)) +
            ggplot2::facet_wrap(wrap_var, scales = 'free') +
            ggplot2::ggtitle(title) + 
            gglayers
      
    if (sd_guides) {
      std_devs = suppressMessages(
                long_data %>% dplyr::group_by_at(wrap_var) %>% 
                dplyr::summarize(mean = mean(!!dplyr::sym(y)), 
                                 sd = sd(!!dplyr::sym(y)))
                )
      
      plot = plot +
        # Standard deviation lines. Need to be done one-by-one because of aes()
        # Mean
        ggplot2::geom_hline(ggplot2::aes(yintercept = mean), data = std_devs,
                            color = '#ffffff', linetype= 3, alpha= 0.25) +
        # 1 SD lines
        ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = mean+1*sd), data = std_devs,
                            color = '#ff8080', linetype= 3, alpha= 0.25) +
        ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = mean-1*sd), data = std_devs,
                            color = '#ff8080', linetype= 3, alpha= 0.25) +
        # 2 SD lines
        ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = mean+2*sd), data = std_devs,
                            color = '#ff4040', linetype= 3, alpha= 0.25) +
        ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = mean-2*sd), data = std_devs,
                            color = '#ff4040', linetype= 3, alpha= 0.25) +
        # 3 SD lines
        ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = mean+3*sd), data = std_devs,
                            color = '#ff0000', linetype= 3, alpha= 0.25) +
        ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = mean-3*sd), data = std_devs,
                            color = '#ff0000', linetype= 3, alpha= 0.25)
    }
    return(plot)
  }
}
