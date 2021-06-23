## -----------------------------------------------------------------------------
## plotModel
## -----------------------------------------------------------------------------


#' @title plotModel
#' @description Plots class trajectories of an MplusObject
#' @param mplus_model MplusObject containing results
#' @param figure_caption Character vector containing text to be added as a caption
#'     to the ggplot object
#' @return A ggplot object
#' @export
#' @import tidyverse
#' @import glue
#' @import rhdf5
#' @import readr
#' @import ggplot2
plotModel <- function(mplus_model, figure_caption = 'default') {

  # Get the filepath of this model
  path_datafile <- mplus_model[["results"]][["input"]][["data"]][["file"]]
  path_dir <- stringr::str_remove(path_datafile, ".dat")
  path_gh5 <- paste0(path_dir, '.gh5')
  
  # Extract the estimated means to df
  est_means <- as.data.frame(t(mplus.get.estimated_means(path_gh5, 'process1')))

  # Get class variable
  class_vars <- c()
  for (i in 1:nrow(est_means)) {
    n_cls <- mplus_model[["results"]][["class_counts"]][["mostLikely"]][["count"]][[i]]
    class_vars <- c(class_vars, glue('{i} (N={n_cls})'))
  }
  est_means$Class <- class_vars
  
  # Get the timepoints for the plot
  plot_info_split <- stringr::str_split(mplus_model[["PLOT"]], "\n")[[1]]
  series_info <-  plot_info_split[[grep('SERIES', plot_info_split)]]
  series_info_split <- stringr::str_split(series_info, ' ')[[1]]
  
  list_timepoints <- c()
  for (i in series_info_split) {
    num_tmp <- suppressWarnings(readr::parse_number(i))
    if (!is.na(num_tmp)) {
      print(num_tmp)
      list_timepoints <- c(list_timepoints, num_tmp)
    }
  }
  
  # Rename variables to correspond to timepoints
  for (i in 1:length(list_timepoints)) {
      names(est_means)[i] <- list_timepoints[i]
  }
  
  # Shift to long
  est_means_long <- est_means %>% 
    tidyr::pivot_longer(cols = as.character(list_timepoints), 
                 names_to = 'Month', values_to = 'SANS') %>%
    plyr::mutate(Month = factor(as.numeric(Month)))
  est_means_long$Class <- as.factor(est_means_long$Class)
  est_means_long$Month <- as.numeric(levels(est_means_long$Month))[est_means_long$Month]
  
  # Create plot
  est_class_means <- ggplot2::ggplot(data = est_means_long, aes(x = Month, y = SANS, group = Class)) + 
    geom_line(aes(color=Class)) + 
    geom_point(aes(color=Class, shape = Class)) +
    labs(
      caption = figure_caption,
      y = 'Mean SANS Global Score') +
    theme(
      axis.title.y = element_text(vjust = 4, size = 12),
      axis.title.x = element_text(vjust = -2, size = 12),
      plot.caption = element_text(hjust = 0.5, vjust = -2, size = 15),
      plot.margin = unit(c(.5, 0, .5, .5), "cm"),
      legend.text = element_text(size = 12),
      legend.position="top",
    )
  
  return(est_class_means)
  
}