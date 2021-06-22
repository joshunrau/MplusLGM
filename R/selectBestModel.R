## -----------------------------------------------------------------------------
## selectBestModel
## -----------------------------------------------------------------------------


#' @title selectBestModel
#' @description Allows for the selection of the best MplusObject
#' @param list_models A list containing MplusObjects
#' @param method The method to use to select the best model
#' @return An MplusObject
#' @export
selectBestModel <- function(list_models, method = 'BIC') {
  
  # Input validation
  stopifnot(is.list(list_models), method %in% c('BIC'))
  
  # Assume the best model is the first one
  best_model <- list_models[[1]]
  best_bic <- best_model[["results"]][["summaries"]][["BIC"]]
  
  # Attempt to replace the best model
  for (test_model in list_models) {
    
    # Get BIC from this model
    test_bic <- test_model[["results"]][["summaries"]][["BIC"]]
    
    # If it is less than the best model, replace it
    if (best_bic > test_bic) {
      best_model <- test_model
      best_bic <- test_bic
    }
  }
  
  return(best_model)
  
}