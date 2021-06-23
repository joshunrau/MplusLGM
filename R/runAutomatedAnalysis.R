## -----------------------------------------------------------------------------
## runAutomatedAnalysis
## -----------------------------------------------------------------------------


#' @title runAutomatedAnalysis
#' @description Identifies the optimal model based on predefined rules using
#'     the BIC and LRT tests, to be used for exploratory analyses.
#' @param df A dataframe containing all user variables and the ID variable.
#' @param usevar A character vector containing variables to be used in Mplus
#'     for analysis.
#' @param timepoints A vector containing the timepoints corresponding 
#'     to the elements in the usevar vector.
#' @param idvar A character vector containing the ID variable in the dataframe.
#' @param min_k The minimum class structure of the model
#' @param max_k The maximum class structure of the model
#' @param max_poly The maximum polynomial order of the model
#' @param working_dir The working directory in which the results folder will be
#' @export
runAutomatedAnalysis <- function(df, usevar, timepoints, idvar, working_dir = getwd(),
                                 min_k = 1, max_k = 6, max_poly = 3) {
  
  # Input validation
  stopifnot(max_k <= 6, max_poly <= length(timepoints))
  
  # # Run GBTM for K = min - max
  list_gbtm_results <- list()
  for (k in min_k:max_k) {
    list_gbtm_results[[k]] <- runModel(
      df, usevar, timepoints, idvar, k, max_poly, 'GBTM', working_dir, 4000)
  }
  
  # Select best model based on BIC and LRT
  best_gbtm <- selectBestModel(list_gbtm_results, method = 'BIC_LRT')
  best_gbtm_cls <- parse_number(best_gbtm[["results"]][["input"]][["variable"]][["classes"]])
  
  # Determine best set of residual variance restrictions with k = best_gbtm_cls
  list_rv_results <- list(best_gbtm)
  cnt <- 2
  for (m in c('LCGA1', 'LCGA2', 'LCGA3')) {
    list_rv_results[[cnt]] <- runModel(
      df, usevar, timepoints, idvar, best_gbtm_cls, max_poly, m, working_dir, 4000)
    cnt <- cnt + 1
  }
  
  # Select best model based on BIC
  best_rv <- selectBestModel(list_rv_results, method = 'BIC')
  best_rv_model <- stringr::str_split(best_rv[['TITLE']], '_')[[1]][[1]]
  
  # Attempt to refine polynomial order
  
  
}