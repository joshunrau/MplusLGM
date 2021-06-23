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
  
  # Fit GBTM models and select optimal model based on BIC and LRT
  list_gbtm_results <- fitGBTM(df, usevar, timepoints, idvar, working_dir, min_k, max_k, max_poly)
  best_gbtm <- selectBestModel(list_gbtm_results, method = 'BIC_LRT')
  best_gbtm_cls <- readr::parse_number(best_gbtm[["results"]][["input"]][["variable"]][["classes"]])
  
  # Determine best set of residual variance restrictions, selecting best model based on BIC
  list_rv_results <- fitLCGA(df, usevar, timepoints, idvar,  best_gbtm_cls, 
                             working_dir, max_poly, best_gbtm)
  best_rv_model <- selectBestModel(list_rv_results, method = 'BIC')
  
  # Attempt to refine polynomial order
  final_model <- refinePolynomial(best_rv_model, df, usevar, idvar, working_dir) 
  
  return(final_model)
  
}


#' @title fitGBTM
#' @description Fits GBTM models from a specified minimum class to a maximum class
#'     using the runModel function
#' @param df A dataframe containing all user variables and the ID variable.
#' @param usevar A character vector containing variables to be used in Mplus
#'     for analysis.
#' @param timepoints A vector containing the timepoints corresponding 
#'     to the elements in the usevar vector.
#' @param idvar A character vector containing the ID variable in the dataframe.
#' @param working_dir The working directory in which the results folder will be
#' @param min_k The minimum class structure of the model
#' @param max_k The maximum class structure of the model
#' @param overall_polynomial The overall polynomial order of the model
#' @export
fitGBTM <- function(df, usevar, timepoints, idvar, working_dir = getwd(),
                     min_k = 1, max_k = 6, overall_polynomial = 3) {
  
  # Input validation
  stopifnot(max_k <= 6, overall_polynomial <= length(timepoints))
  
  # Run GBTM for K = min - max
  list_gbtm_results <- list()
  for (k in min_k:max_k) {
    list_gbtm_results[[k]] <- runModel(
      df, usevar, timepoints, idvar, k, overall_polynomial, 'GBTM', working_dir, 4000)
  }
  
  return(list_gbtm_results)
  
}


#' @title fitLCGA
#' @description Fits LCGA models of specified class to test residual variance
#'     restrictions using the runModel function
#' @param df A dataframe containing all user variables and the ID variable.
#' @param usevar A character vector containing variables to be used in Mplus
#'     for analysis.
#' @param timepoints A vector containing the timepoints corresponding 
#'     to the elements in the usevar vector.
#' @param idvar A character vector containing the ID variable in the dataframe.
#' @param k The class structure of the model
#' @param working_dir The working directory in which the results folder will be
#' @param overall_polynomial The overall polynomial order of the model
#' @param ref_model An optional reference model to add to the list returned
#' @export
fitLCGA <- function(df, usevar, timepoints, idvar, k, working_dir = getwd(),
                    overall_polynomial = 3, ref_model = NULL) {
  
  # Initiate list and count
  list_rv_results <- list()
  cnt <- 1
  
  # If a reference model is specified, add it to the list
  if (!is.null(ref_model)) {
    list_rv_results[[cnt]] <- ref_model
    cnt <- cnt + 1
  }
  
  # Fit LCGA models
  for (m in c('LCGA1', 'LCGA2', 'LCGA3')) {
    list_rv_results[[cnt]] <- runModel(
      df, usevar, timepoints, idvar, k, overall_polynomial, m, working_dir, 4000)
    cnt <- cnt + 1
  }
  
  return(list_rv_results)
  
}


#' @title refinePolynomial
#' @description Refines the growth factors for each class in a model
#' @param model The MplusObject to refine
#' @param df A dataframe containing all user variables and the ID variable.
#' @param usevar A character vector containing variables to be used in Mplus
#'     for analysis.
#' @param timepoints A vector containing the timepoints corresponding 
#'     to the elements in the usevar vector.
#' @param idvar A character vector containing the ID variable in the dataframe.
#' @param working_dir The working directory in which the results folder will be
#' @export
refinePolynomial <- function(model, df, usevar, idvar, working_dir = getwd()) {
  
  # Get necessary model information
  k <- readr::parse_number(model[["results"]][["input"]][["variable"]][["classes"]])
  p <- readr::parse_number(stringr::str_split(model[["TITLE"]], '_')[[1]][[2]])
  m <- stringr::str_split(model[['TITLE']], '_')[[1]][[1]]
  
  # Initialize list of all current growth factors for classes
  factorsClass <- list()
  
  for (i in 1:k) {
    if (p == 3) {
      factorsClass[[i]] <- c("I","S","Q","CUB")
    } else if (p == 2) {
      factorsClass[[i]] <- c("I","S","Q")
    } else if (p == 1) {
      return(model) # Cannot further refine if already linear
    } else {
      stop('invalid polynomial order of model')
    }
  }
  
  # Create a list of all current factors to be checked for all classes
  currentFactorClass <- .create.list.currentFactor(factorsClass, k)
  
  # Create a list that contains the indexes of the mean p values of the current factors for all classes
  indexPvalueClass <- .create.list.indexPvalue(currentFactorClass, model, k)
  
  # Get the p values using the indexes
  pValClass <- .create.list.pValues(model, indexPvalueClass, k)
  
  # While any P value for a class growth factor is non-significant
  while (any(pValClass > .05)) {
    
    # Hold if any non-slope is non-significant to avoid infinite loop
    all_slope <- TRUE
    
    # Check each class
    for (i in 1:k) {
      # If the growth factor is not significant, and not the slope, remove it
      if (pValClass[[i]] > 0.05 && head(factorsClass[[i]]) != "S") {
        factorsClass[[i]] = head(factorsClass[[i]], -1) # Remove from counter
        all_slope <- FALSE
      }
    }
    
    # If all slope was not rendered false, all non-signifigant are slopes
    if (all_slope) {break}
    
    # Get the growth factors for each class as a number
    gf <- c()
    for (i in 1:k) {
      gf <- c(gf, length(factorsClass[[i]]) - 1)  # Subtract for I
    }
    
    # Run model with updated growth factors
    model <- runModel(df, usevar, timepoints, idvar, k, p, m, working_dir, 4000, gf)
    
    # Update current highest growth factor
    currentFactorClass <- .create.list.currentFactor(factorsClass, k)
    
    # Update indices of class p-values
    indexPvalueClass <- .create.list.indexPvalue(currentFactorClass, model, k)
    
    # Get the new class p values
    pValClass <- .create.list.pValues(model, indexPvalueClass, k)
    
  }
  
  return(model)
  
}
  
  
# Create a list of all current factors (= the last ones) to be checked for all classes
.create.list.currentFactor <- function(factorsClass, k) {
  currentFactorClass <- list()
  for (i in 1:k){
    currentFactorClass[[i]] <- tail(factorsClass[[i]], n=1)
  }
  return (currentFactorClass)
}


# index of the mean p value of the current factor that we check for each class
.create.list.indexPvalue <- function(currentFactorClass, model, k){
  
  # Read the list of parameters to find the position of the means and find the p values associated with the means
  listeParam <- model$results$parameters$unstandardized$param
  
  indexPvalueClass <- list()
  j <- 1 # the index of the p value of the 1st class would be the first occurrence of the current factor
  for (i in 1:k){
    indexPvalueClass[[i]] <- grep(glue::glue("^{currentFactorClass[[i]]}$"), listeParam)[j]
    # We skip the factors related to the variances 
    # j = 1: first occurrence of the factor (mean of class 1), j = 2: second occurrence of the factor (variance of class 1)
    # j = 3: third occurrence of the factor (mean of class 2)...
    j <- j + 2
  }
  return(indexPvalueClass)
}


# Get the p values using the indexes
.create.list.pValues <- function(mplusObject,indexPvalueClass, k){
  pValClass <- list()
  for (i in 1:k){
    pValClass[[i]] <- mplusObject$results$parameters$unstandardized$pval[indexPvalueClass[[i]]]
  }
  return (pValClass)
}