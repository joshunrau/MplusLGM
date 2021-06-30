#' @title plotModel
#' @description Plots class trajectories of an MplusObject
#' @param mplus_model An MplusObject containing results
#' @param x_axis_label A character vector containing text for x-axis
#' @param y_axis_label A character vector containing text for y-axis
#' @param geom_line2 Specify an additional geom for ggplot
#' @param geom_point2 Specify an additional geom for ggplot
#' @param figure_caption A character vector containing text to be added as a caption
#' @return A ggplot object
#' @export
#' @import tidyverse
#' @import glue
#' @import rhdf5
#' @import readr
#' @import ggplot2
plotModel <- function(
  mplus_model,
  x_axis_label = 'time', 
  y_axis_label = 'variable', 
  figure_caption = NULL,
  geom_line2 = NULL,
  geom_point2 = NULL
  ) {

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
    class_vars <- c(class_vars, glue::glue('{i} (N={n_cls})'))
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
      list_timepoints <- c(list_timepoints, num_tmp)
    }
  }
  
  # Rename variables to correspond to timepoints
  for (i in 1:length(list_timepoints)) {
      names(est_means)[i] <- list_timepoints[i]
  }
  
  # Shift to long
  est_means_long <- est_means %>% 
    tidyr::pivot_longer(
      cols = as.character(list_timepoints), 
      names_to = 'Time', 
      values_to = 'Variable') %>%
    plyr::mutate(Time = factor(as.numeric(Time)))
  est_means_long$Class <- as.factor(est_means_long$Class)
  est_means_long$Time <- as.numeric(levels(est_means_long$Time))[est_means_long$Time]
  
  # Create plot
  est_class_means <- ggplot2::ggplot() + 
    geom_line(data = est_means_long, aes(x = Time, y = Variable, group = Class, color=Class)) + 
    geom_point(data = est_means_long, aes(x = Time, y = Variable, group = Class, color=Class, shape = Class))
  
  if (!is.null(geom_line2)) {
    est_class_means <- est_class_means + geom_line2
  }
  
  if (!is.null(geom_point2)) {
    est_class_means <- est_class_means + geom_point2
  }
  
  est_class_means <- est_class_means +
    labs(caption = figure_caption) +
    xlab(x_axis_label) +
    ylab(y_axis_label) +
    theme(
      text = element_text(size=15),
      axis.title.y = element_text(vjust = 4),
      axis.title.x = element_text(vjust = -2),
      plot.caption = element_text(hjust = 0.5, vjust = -2),
      plot.margin = unit(c(.5, 0, .5, .5), "cm"),
      legend.position="top",
    )
  
  return(est_class_means)
  
}
