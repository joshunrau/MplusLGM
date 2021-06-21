library(MplusAutomation)
library(tidyverse)
library(parallel)

getMplusObject <- function(df, usevar, timepoints, idvar, classes, starts, 
                           overall_polynomial, model_type) {
  
  
  createCommand <- function(old_vector) {
    
    # Given a list of character vectors, returns a single character vectors with
    # valid elements separated by newlines, any other elements removed, with a
    # final newline added for readability
    
    new_vector <- c()
    
    for (element in old_vector) {
      
      if (typeof(element) == 'character') {
        new_vector <- c(new_vector, element)
      }
    }
    
    return(paste0(glue_collapse(new_vector, sep = '\n'), '\n'))
    
  }
  
  
  splitLength <- function(old_vector) {
    
    # Given a character vector, rebuilds each element such that the length
    # of the characters in each line to be written is less than 50. If any
    # element is over 50, raises an exception.
    
    new_vector <- c()
    count <- 0
    
    for (i in old_vector) {
      
      if (nchar(i) >= 50) { # Risk of error with Mplus
        
        stop('element in character vector should not be over 50 length')
        
      } else if (count + nchar(i) >= 50) { # If adding to count will exceed 50
        
        new_vector <- c(new_vector, paste0('\n', i))
        count <- nchar(i) + 1 # Reset count (one is added for spaces)
        
      } else {
        
        new_vector <- c(new_vector, i)
        count <- nchar(i) + count + 1 # One is added for space
        
      }
      
    } 
    
    return(new_vector)
    
  }


  
  getTitle <- function(classes, model_type) {
    
    return(createCommand(glue('{model_type}_K={classes}')))
    
  }
  
  
  getVariable <- function(usevar, idvar, classes) {
    
    usevar <- glue('USEVAR = {glue_collapse(splitLength(usevar), sep = " ")};')
    idvar <- glue('IDVAR = {idvar};')
    classes <- glue('CLASSES = c({classes});')
    return(createCommand(c(usevar, idvar, classes)))
    
  }
  
  
  getAnalysis <- function(starts, processors) {
    
    model_type <- 'TYPE = MIXTURE;'
    model_starts <- glue('STARTS = {starts} {starts/4};')
    k1starts <- glue('K-1STARTS = {starts/2} {starts/8};')
    processors <- glue('PROCESSORS = {processors = detectCores()};')
    return(createCommand(c(model_type, model_starts, k1starts, processors)))
    
  }
  
  
  getModel <- function(usevar, timepoints, model_type) {
    
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
    vars_timepoints <- splitLength(vars_timepoints)
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
    
    return(createCommand(c(overall_label, overall_growth_factors, vars_timepoints, 
                         restrict_var, restrict_gbtm,  class_parameters)))
    
  }
  
  
  getOutout <- function() {
    
    return(createCommand(c('sampstat standardized', 'TECH1;', 'TECH11;')))
    
  }
  
  
  getPlot <- function() {
    
    plot_type <- 'TYPE = plot3;'
    
    plot_usevar <- c()
    
    for (i in 1:length(usevar)) {
      
      plot_usevar <- c(plot_usevar, glue('{usevar[i]} (s)'))
      
    }
    
    plot_usevar <- splitLength(plot_usevar)
    plot_usevar <- glue('SERIES = {glue_collapse(plot_usevar, sep = " ")};')
    
    return(createCommand(c(plot_type, plot_usevar)))
    
  }
  
  
  return(
    
    mplusObject(
      
      TITLE = getTitle(classes, model_type),
      VARIABLE = getVariable(usevar, idvar, classes),
      ANALYSIS = getAnalysis(starts, processors),
      MODEL = getModel(usevar, timepoints, model_type),
      OUTPUT = getOutout(),
      PLOT = getPlot(),
      usevariables = colnames(subset(df, select = c(idvar, usevar))),
      rdata = subset(df, select = c(idvar, usevar)),
      autov = FALSE
    
    )
    
  )
  
}

MyModel <- getMplusObject(SampleData, c('var1', 'var2', 'var3', 'var4', 'var5'), 
                          c(1, 2, 3, 4, 5), 'id', 3, 500, 3, 'GBTM')


res <- mplusModeler(
  object = MyModel, 
  dataout = 'test.dat', 
  modelout = 'test.inp',
  run = 0,
  writeData = 'always', 
  hashfilename = FALSE
)
