## -----------------------------------------------------------------------------
## getFitIndices
## -----------------------------------------------------------------------------


#' @title getFitIndices
#' @description Returns the fit indices for a list of MplusObjects
#' @param list_models A list of MplusObjects
#' @return A summary of models in list as df
#' @export
#' @import MplusAutomation
#' @import stringr
getFitIndices <- function(list_models) {
  
  # Iterate through all models in list to extract results from path
  model_res_list <- list()
  count <- 1
  
  for (model in list_models) {
    
    # Get the filepath of this model's output
    path_datafile <- model[["results"]][["input"]][["data"]][["file"]]
    path_dir <- stringr::str_remove(path_datafile, ".dat")
    path_out <- paste0(path_dir, '.out')
    
    # Read this model and append it to list
    model_res <- MplusAutomation::readModels(path_out, what = 'summaries')
    model_res_list[[count]] <- model_res
    count <- count + 1
  }
  
  # Create table of these models and return
  models_sum <- MplusAutomation::SummaryTable(
    model_res_list, 
    keepCols = c("Title", "Parameters", 'LL', "BIC", "Entropy",
                 "T11_LMR_Value", "T11_LMR_PValue"))
  
  return(models_sum)
  
}