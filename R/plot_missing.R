#' @name plot_missing
#'
#' @title Plot data from rc_missing
#' @description  Creates a pie chart for each variable passed from 
#' \code{rc_missing}. These plots are useful for investigating the
#' structure of missing data. This function can be called from the
#' \code{plot} argument in \code{rc_missing}. Also try the \code{table}
#' argument in \code{rc_missing} for further investigation. 
#' 
#' This is an internal function only.
#' 
#' @param missing_data Dataframe. A frame of long format missing data, as 
#' created by \code{rc_missing}.
#' 
#' @author Marcus Lehr
 

plot_missing <- function(missing_data) {
  
  # Make column counts and other data for plots
  counts = list()
  for (col in names(missing_data)) {
    
    counts[[col]] =
      dplyr::group_by_(missing_data, col) %>% dplyr::summarise(count = dplyr::n()) %>% 
      dplyr::mutate(prop = (count/sum(count))*100, 
                    ypos = 100 - cumsum(prop) + prop/2,
                    angle = (cumsum(prop)-prop/2)*3.6-90) %>% 
      as.data.frame()
  }
  
  # # Could use this to color variable plot by form. Would require data_dict
  # counts[['variable']] = dplyr::left_join(counts[['variable']], instrVarMap) %>% dplyr::select(form_name, everything())
  
  # Create plots
  pieCharts = list()
  for (i in 1:length(counts)){
    pieCharts[[i]] = ggplot2::ggplot(counts[[i]], ggplot2::aes(x='x', y=prop, fill=counts[[i]][[1]])) +
      ggplot2::geom_bar(width = 1, stat = 'identity', color = 'white') +
      ggplot2::geom_text(ggplot2::aes(x = 1.1, y = ypos, label = counts[[i]][[1]]), 
                         color = 'white', angle = counts[[i]]$angle) +
      ggplot2::coord_polar('y') +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = 'none') +
      ggplot2::labs(title = paste0(names(counts)[i], ' distribution in missing data, plot ',
                                   i,' of ',length(counts)))
    
    gridExtra::grid.arrange(pieCharts[[i]])
  }
}
