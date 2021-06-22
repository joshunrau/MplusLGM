## -----------------------------------------------------------------------------
## getMplusObject
## -----------------------------------------------------------------------------


#' @title getMplusObject
#' @description Provides a method to easily create an MplusObject for mixture 
#'     modeling. Current mixture models supported include: GBTM, with fixed 
#'     residual variance across time and class; LCGA1, with fixed residual 
#'     variance across time, but not class; LCGA2, with fixed residual variance 
#'     across class, but not time; and LCGA3, with unrestricted residual 
#'     variance across both time and class.
#' @param df A dataframe containing all user variables and the ID variable.
#' @param usevar A character vector containing variables to be used in Mplus
#'     for analysis.
#' @param timepoints A vector containing the timepoints corresponding 
#'     to the elements in the usevar vector.
#' @param idvar A character vector containing the ID variable in the dataframe.
#' @param classes A numeric value representing the number of classes in the model.
#' @param starts A numeric representing the number of initial stage starts for 
#'     the model. Note that the number of final stage optimizations will be set 
#'     as equal to half of this value.
#' @param overall_polynomial An numeric representing the polynomial order for 
#'     the overall model. Note that only linear, quadratic, and cubic models are 
#'     supported.
#' @param model_type A character vector representing the type of mixture model 
#'     to create. Available options are "GBTM", "LCGA1", "LCGA2", and "LCGA3".
#' @return An MplusObject
#' @example 
#' data(SampleData)
#' MyModel <- getMplusObject(
#'   df = SampleData,
#'   usevar = c('var1', 'var2', 'var3', 'var4', 'var5'),
#'   timepoints = c(1, 2, 3, 4, 5),
#'   idvar = 'id',
#'   classes = 3,
#'   starts = 500,
#'   overall_polynomial = 3,
#'   model_type = 'GBTM'
#' )
#' @export
#' @import MplusAutomation
#' @import tidyverse
#' @import glue
#' @importFrom parallel detectCores
getMplusObject <- function(df, usevar, timepoints, idvar, classes, starts, 
                           overall_polynomial, model_type) {
  
  # Validate types of inputs (i.e., check for TypeError)
  stopifnot(is.data.frame(df), is.character(usevar), is.vector(timepoints),
            is.character(idvar), is.numeric(classes), is.numeric(starts),
            is.numeric(overall_polynomial), is.character(model_type))
    
  # Validate values of inputs (i.e., check for ValueError)
  stopifnot(
    length(usevar) == length(timepoints), length(idvar) == 1,
    dplyr::between(classes, 1, 6), dplyr::between(overall_polynomial, 1, 3),
    model_type %in% c('GBTM', 'LCGA1', 'LCGA2', 'LCGA3')
    )
  
  # Create MplusObject with utility functions for each section of the input file
  model <- mplusObject(
    TITLE = .getTitle(classes, overall_polynomial, model_type, starts),
    VARIABLE = .getVariable(usevar, idvar, classes),
    ANALYSIS = .getAnalysis(starts),
    MODEL = .getModel(usevar, timepoints, overall_polynomial, model_type, classes),
    OUTPUT = .getOutout(),
    PLOT = .getPlot(usevar),
    usevariables = colnames(subset(df, select = c(idvar, usevar))),
    rdata = subset(df, select = c(idvar, usevar)),
    autov = FALSE)
  
  return(model)

}


#' Given a character vector, rebuilds each element such that the number of the 
#' characters and spaces between newlines is less than 50.
.splitLength <- function(old_vector) {
  
  # Initiate count of characters and new vector to build
  new_vector <- c()
  count <- 0
  
  # Iterate through each element in the old vector, checking length
  for (i in old_vector) {
    
    # If the length of the sub element is over 50, raise an exception
    if (nchar(i) >= 50) {
      stop('element in character vector should not be over 50 length')
    } 
    
    # If chars in the element added to the count is over 50, insert newline
    else if (count + nchar(i) >= 50) {
      new_vector <- c(new_vector, paste0('\n', i))
      count <- nchar(i) + 1 # Reset count (one is added for spaces)
    } 
    
    # Otherwise, we can add the element to the new vector without a newline
    else {
      new_vector <- c(new_vector, i)
      count <- nchar(i) + count + 1 # Add the chars in element to count
    }
    
  } 
  
  return(new_vector)
  
}


#' Coerces character vector into a single element, removing non-character 
#' elements, and joining elements of type character by nnewline character, 
.createCommand <- function(old_vector) {
  
  # Initiate new vector to contain character elements of old vector
  new_vector <- c()
  
  # Iterate through old vector adding character elements to vector we initiated
  for (i in old_vector) {
    if (is.character(i)) {
      new_vector <- c(new_vector, i)
    }
  }
  
  # Join elements with newlines, then join this with a newline character at end
  return(paste0(glue_collapse(new_vector, sep = '\n'), '\n'))
  
}


#' Creates the title section of an MplusObject.
.getTitle <- function(classes, overall_polynomial, model_type, starts) {
  model_name <- glue('{model_type}_P{overall_polynomial}_K{classes}_S{starts}')
  return(.createCommand(model_name))
}


#' Creates the variable section of an MplusObject.
.getVariable <- function(usevar, idvar, classes) {
  
  usevar <- glue('USEVAR = {glue_collapse(.splitLength(usevar), sep = " ")};')
  idvar <- glue('IDVAR = {idvar};')
  classes <- glue('CLASSES = c({classes});')
  return(.createCommand(c(usevar, idvar, classes)))
  
}


#' Creates the analysis section of an MplusObject.
.getAnalysis <- function(starts, processors = parallel::detectCores()) {
  
  model_type <- 'TYPE = MIXTURE;'
  model_starts <- glue('STARTS = {starts} {starts/4};')
  k1starts <- glue('K-1STARTS = {starts/2} {starts/8};')
  processors <- glue('PROCESSORS = {processors};')
  return(.createCommand(c(model_type, model_starts, k1starts, processors)))
  
}


#' Creates the model section of an MplusObject.
.getModel <- function(usevar, timepoints, overall_polynomial, 
                      model_type, classes) {
  
  # Calculate overall growth factors
  overall_label <- '%OVERALL%'
  
  if (overall_polynomial == 1) {
    
    overall_growth_factors <- 'i s |'
    restrict_var <- 'i-s@0;'
    class_growth_factors <- '[i s];'
    
  } else if (overall_polynomial == 2) {
    
    overall_growth_factors <- 'i s q |'
    restrict_var <- 'i-q@0;'
    class_growth_factors <- '[i s q];'
    
  } else if (overall_polynomial == 3) {
    
    overall_growth_factors <- 'i s q cub |'
    restrict_var <- 'i-cub@0;'
    class_growth_factors <- '[i s q cub];'
    
  } else {
    
    stop('polynomial order must be either 1, 2, or 3') 
    
  }
  
  # Generate list of user variables @ timepoints
  vars_timepoints <- c()
  
  for (i in 1:length(usevar)) {
    vars_timepoints <- c(vars_timepoints, glue('{usevar[[i]]}@{timepoints[[i]]}'))
  }
  
  # Split into maximum 60 chars per line
  vars_timepoints <- .splitLength(vars_timepoints)
  vars_timepoints <- glue('{glue_collapse(vars_timepoints, sep = " ")};')
  
  # If the model is a GBTM, fix  residual variance across time and classes
  if (model_type == 'GBTM') {
    
    restrict_gbtm <- glue('{usevar[1]}-{usevar[length(usevar)]} (1);')
    
  }
  
  # Specify class parameters 
  class_parameters <- c()
  for (i in 1:classes) {
    class_parameters <- c(class_parameters, glue('%c#{i}%'), class_growth_factors)
  }
  
  return(.createCommand(c(overall_label, overall_growth_factors, vars_timepoints, 
                         restrict_var, restrict_gbtm,  class_parameters)))
  
}


#' Creates the model section of an MplusObject
.getOutout <- function() {
  return(.createCommand(c('sampstat standardized', 'TECH1;', 'TECH11;')))
}


#' Creates the plot section of an MplusObject
.getPlot <- function(usevar) {
  
  # Initiate vector
  plot_type <- 'TYPE = plot3;'
  plot_usevar <- c()
  
  # Assign to new vector each user variable followed by (s)
  for (i in 1:length(usevar)) {
    plot_usevar <- c(plot_usevar, glue('{usevar[i]} (s)'))
  }
  
  # Make sure length will not be too long, then collapse into single element
  plot_usevar <- .splitLength(plot_usevar)
  plot_usevar <- glue('SERIES = {glue_collapse(plot_usevar, sep = " ")};')
  
  return(.createCommand(c(plot_type, plot_usevar)))
  
}