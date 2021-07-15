#' @title runGCM
#' @description Run a Growth Curve Model
#' @param df A data frame containing all user variables and the ID variable
#' @param usevar A character vector containing variables to be used in Mplus
#'     for analysis
#' @param timepoints A vector containing the timepoints corresponding 
#'     to the elements in the usevar vector
#' @param working_dir The directory where the results folder will be created
#' @return An MplusObject
#' @import tidyverse
#' @import glue
#' @export
runGCM <- function(
  df,
  usevar,
  timepoints,
  working_dir = getwd()
) {
  
  # Input validation
  stopifnot(
    is.data.frame(df), 
    is.character(usevar), 
    is.vector(timepoints),
    length(usevar) == length(timepoints)
  )
  
  # Generate list of user variables @ timepoints
  vars_timepoints <- c()
  for (i in 1:length(usevar)) {
    vars_timepoints <- c(vars_timepoints, glue::glue('{usevar[[i]]}@{timepoints[[i]]}'))
  }
  
  # Split into maximum 60 chars per line
  vars_timepoints <- .splitLength(vars_timepoints)
  vars_timepoints <- glue::glue('{glue::glue_collapse(vars_timepoints, sep = " ")};')
  
  # Create GCM MplusObject
  gcm_model <- MplusAutomation::mplusObject(
    TITLE = 'Growth Curve Model',
    MODEL = 
      c('i s |', 
        vars_timepoints),
    OUTPUT = 
      c('sampstat standardized tech1;'),
    PLOT = .getPlot(usevar),
    rdata = subset(df, select = usevar)
  )
  
  # Create model directory
  model_dir <- glue::glue('{working_dir}/Results/GCM')
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }
  
  # Run the model
  gcm_results <- MplusAutomation::mplusModeler(
    object = gcm_model, 
    dataout = glue::glue('{model_dir}/mplus.dat'), 
    modelout = glue::glue('{model_dir}/GCM.inp'),
    run = 1,
    writeData = 'always', 
    hashfilename = FALSE
  )
  
  return(gcm_results)
  
}

#' @title plotGCM
#' @description Plot a Growth Curve Model
#' @export
#' @import tidyverse
plotGCM <- function(gcm_model, timepoints) {
  
  est_means <- gcm_model[["results"]][["gh5"]][["means_and_variances_data"]][["y_estimated_means"]][["values"]]
  est_means <- as.data.frame(t(est_means))
  
  # Convert variable names to timepoints names
  for (i in 1:length(timepoints)) {
    names(est_means)[i] <- timepoints[i]
  }
  
  # Convert to long form
  est_means_long <- est_means %>% 
    pivot_longer(
      cols = everything(),
      names_to = "Time", values_to = "Variable") %>%
    mutate(Time = factor(as.numeric(Time)))
  
  est_means_long$Time <- as.numeric(levels(est_means_long$Time))[est_means_long$Time]
  
  # Create plot
  gcm_plot <- ggplot2::ggplot() + 
    geom_line(data = est_means_long, aes(x = Time, y = Variable)) + 
    geom_point(data = est_means_long, aes(x = Time, y = Variable))
  
  return(gcm_plot)
  
}