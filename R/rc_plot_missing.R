#' @name rc_plot_missing
#'
#' @title Plot data from rc_missing()
#' @description  Creates a pie chart for each variable passed from 
#' \code{rc_missing()} and returns list containing the plots. *NOTE* The 
#' data passed from \code{rc_missing()} must be in long format. These 
#' plots are useful for investigating the structure of missing data.  
#' 
#' @param missing_data Dataframe. A frame of long format missing data, as 
#' created by \code{rc_missing}.
#' 
#' @author Marcus Lehr
#' 
#' @export
 

rc_plot_missing <- function(missing_data, return_data = FALSE) {
  
  # Remove repeat columns if empty
  missing_data = rc_strip(missing_data)
  
  # Make column counts and other data for plots
  counts = list()
  for (var in names(missing_data)) {
    
    counts[[var]] = missing_data %>% 
      dplyr::group_by(!!dplyr::sym(var)) %>%
      dplyr::summarise(count = dplyr::n()) %>% 
      dplyr::mutate(prop = (count/sum(count))*100, 
                    ypos = 100 - cumsum(prop) + prop/2,
                    angle = (cumsum(prop)-prop/2)*3.6-90) %>% 
      as.data.frame()
  }
  
  # Create plots. Subplot methods have been avoided as they are often unreadable when scaled down.
  pieCharts = list()
  for (i in 1:length(counts)) {
    var = names(counts)[i]
    pieCharts[[i]] = ggplot2::ggplot(counts[[i]], ggplot2::aes(x='x', y=prop, 
                                                               fill=!!dplyr::sym(var))) +
      ggplot2::geom_bar(width = 1, stat = 'identity', color = 'white') +
      ggplot2::geom_text(ggplot2::aes(x = 1.1, y = ypos, label = !!dplyr::sym(var)), 
                         color = 'white', angle = counts[[i]]$angle) +
      ggplot2::coord_polar('y') +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = 'none') +
      ggplot2::labs(title = paste0(var, ' distribution in missing data, plot ',
                                   i,' of ',length(counts)))
  }
  
  if (return_data) {
    # Convert to single dataframe
    return(
      lapply(counts, function(count_data){
        var = names(count_data)[1]
        count_data %>% 
          dplyr::rename(value=!!dplyr::sym(var)) %>% 
          dplyr::mutate(variable=var) %>% 
          dplyr::select(variable, value, count) # Plotting params not needed
      }) %>% do.call(rbind,.) %>% `rownames<-`(NULL)  
    )
  } else 
      return(pieCharts)
}
