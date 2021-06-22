## -----------------------------------------------------------------------------
## runModel
## -----------------------------------------------------------------------------

#' @title runModel
#' @description Provides a method to run a model with a given class structure
#'     and polynomial order, testing that the logliklihood value has replicated
#'     at least twice within the model, as well as in the model with half the
#'     number of starts.
#' @param df A dataframe containing all user variables and the ID variable.
#' @param usevar A character vector containing variables to be used in Mplus
#'     for analysis.
#' @param timepoints A vector containing the timepoints corresponding 
#'     to the elements in the usevar vector.
#' @param idvar A character vector containing the ID variable in the dataframe.
#' @param classes A numeric value representing the number of classes in the model.
#' @param overall_polynomial An numeric representing the polynomial order for 
#'     the overall model. Note that only linear, quadratic, and cubic models are 
#'     supported.
#' @param model_type A character vector representing the type of mixture model 
#'     to create. Available options are "GBTM", "LCGA1", "LCGA2", and "LCGA3".
#' @param max_starts A numeric representing the maximum level of starting values
#'     to use when attempting to replicate the logliklihood before raising an 
#'     error.
#' @return An MplusObject containing results
#' @example 
#' data(SampleData)
#' runModel(
#'     df = SampleData,
#'     usevar = c('var1', 'var2', 'var3', 'var4', 'var5'), 
#'     timepoints = c(1, 2, 3, 4, 5), 
#'     idvar = 'id', 
#'     classes = 3, 
#'     overall_polynomial = 3, 
#'     model_type = GBTM)
#' @export
#' @import tidyverse
#' @import MplusAutomation
runModel <- function(df, usevar, timepoints, idvar, classes, overall_polynomial, 
                     model_type, working_dir = getwd(), max_starts = 4000) {
  
  # Set initial number of starting values at 500
  starts <- 500
  
  # Initiate list to contain model results and a count of the models run
  list_log <- list()
  count_log <- 1
  
  # Attempt to replicate the LL until the maximum number of starts
  while (starts <= max_starts) {
    
    # Get MplusObject for specified model
    model <- getMplusObject(df, usevar, timepoints, idvar, classes, starts, 
                            overall_polynomial, model_type)
    
    # Create model directory to contain specified model if does not exist
    model_dir <- .createModelDirectory(
      working_dir, model_type, overall_polynomial, classes)
    
    # Get name of the model
    model_name <- str_remove(model[["TITLE"]], '\n')
    
    # Run model and add it to the list of models
    list_log[[count_log]] <- mplusModeler(
      model, glue('{model_dir}/{model_name}.dat'), 
      glue('{model_dir}/{model_name}.inp'), run = 1,
      writeData = 'always', hashfilename = FALSE)
    
    # If  more than one model in list (i.e., starts != 500, check if replicated)
    if (starts != 500) {
      
      # Read the output file and find separator sentence
      file <- readLines(glue('{model_dir}/{model_name}.out'))
      line <- grep("Final stage loglikelihood values at local maxima, seeds, and initial stage start numbers:", file)
      
      # Get the lines that contain the LL values, splitting the string into a vector of several strings
      vector1 <- strsplit(file[line+2], " ")[[1]]
      vector2 <- strsplit(file[line+3], " ")[[1]]
      
      # Get the LL values from these vectors
      LL1 <- vector1[12]
      LL2 <- vector2[12]
      
      # If the best LL has been replicated in this model, check across models
      if (LL1 == LL2) {
        
        # Extract LL from MplusObjects
        log_m1 <- SummaryTable(list_log[[count_log - 1]], keepCols = 'LL', sortBy = 'LL')
        log_m2 <- SummaryTable(list_log[[count_log]], keepCols = 'LL', sortBy = 'LL')
        
        # If these values are equal, return the last model run
        if (log_m1 == log_m2) {
          return(list_log[[count_log]])
        }
      }
      
    }
    
    # If LL is not replicated double starts and count += 1
    starts <- starts * 2
    count_log <- count_log + 1
    
  }
} 


#' @title createModelDirectory
#' @description Creates the specified directory if it does not exist.
#' @param working_dir The current working directory
#' @param model_type A character vector representing the type of mixture model 
#'     to create. Available options are "GBTM", "LCGA1", "LCGA2", and "LCGA3".
#' @param overall_polynomial An numeric representing the polynomial order for 
#'     the overall model. Note that only linear, quadratic, and cubic models are 
#'     supported.
#' @param classes A numeric value representing the number of classes in the model.
#' @return A character vector containing the model path
#' @export
#' @import glue
.createModelDirectory <- function(working_dir, model_type, 
                                  overall_polynomial, classes) {
  
  results_dir <- glue('{working_dir}/Results')
  model_dir <- glue('{results_dir}/{model_type}/P={overall_polynomial}/K={classes}')
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }
  
  return(model_dir)
}
