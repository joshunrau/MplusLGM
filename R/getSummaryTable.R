#'@title getSummaryTable
#'@param df A data frame containing all descriptive variables and the grouping variable
#'@param group_var A factor defining the grouping variable
#'@param list_vars A vector containing the variables to summarize
#'@return A data frame
#'@export
#'@import tidyverse
getSummaryTable <- function(
  df, 
  group_var, 
  list_vars
  ) {
  
  # Input validation
  stopifnot(
    is.data.frame(df),
    is.factor(df[,group_var]), 
    is.vector(list_vars),
    list_vars %in% colnames(df)
  )
  
  
  # Create list for all continuous and categorical variables
  all_categorical <- c()
  all_continuous <- c()
  
  # Sort the variables and make sure all are either factors or numeric
  for (var_name in list_vars) {
    
    if (is.factor(df[,var_name])) {
      all_categorical <- c(all_categorical, var_name)
    } 
    
    else if (is.numeric(df[,var_name])){
      all_continuous <- c(all_continuous, var_name)
    }
    
    else {
      stop('all variables must be either factors or numeric')
    }

  }
  
  # Get the means for all continuous variables
  var_means <- df %>%
    dplyr::group_by(df[,group_var]) %>% 
    dplyr::summarise_at(vars(all_of(all_continuous)), mean, na.rm = TRUE)
  
  # Get the standard deviations for all continuous variables
  var_sd <- df %>%
    dplyr::group_by(df[,group_var]) %>% 
    dplyr::summarise_at(vars(all_of(all_continuous)), sd, na.rm = TRUE)
  
  # Get grouping variable levels
  group_var_labs <- as.character(var_means[[1]])
  stopifnot(group_var_labs == as.character(var_sd[[1]]))
  
  # Transpose data frames
  var_means <- round(as.data.frame(t(var_means[,-1])), digits = 2)
  var_sd <- round(as.data.frame(t(var_sd[,-1])), digits = 2)
  
  # Combine mean and sd data frames 
  cont_vars_summary <- list()
  for (i in 1:length(group_var_labs)) {
    name_var <- group_var_labs[[i]]
    cont_vars_summary[[name_var]] <- paste0(var_means[,i], ' (', var_sd[,i], ')')
  }
  
  cont_vars_summary <- data.frame(cont_vars_summary)

  # Rename variables in dataframe
  rownames(cont_vars_summary) <- all_continuous
  
  # Reapply correct column names
  colnames(cont_vars_summary) <- group_var_labs
  
  # GET FOR CATEGORICAL VARS
  
  # Get nested list of stats
  
  cat_vars_summaries <- list()
  for (i in match(all_categorical, colnames(df))) {
    
    # Get name of variable
    name_var <- colnames(df[i])
    
    # Get the counts for each level of the factor
    for (j in group_var_labs) {
      
      # Subset data frame to contain only the level being tested
      df_tmp <- df %>%
        dplyr::filter(.data[[group_var]] == toString(j))
      
      cat_vars_summaries[[name_var]][[toString(j)]] <- table(df_tmp[[name_var]])
      
    }
    
  }
  
  # Reorganize nested lists to dataframe
  
  suppressWarnings(rm(cat_vars_summary))

  for (v in labels(cat_vars_summaries)) {
    
    if (!exists('cat_vars_summary')) {
      
      cat_vars_summary <- .listTablesToDF(cat_vars_summaries[[v]], v)
      
    } else {
      
      cat_vars_summary <- rbind(cat_vars_summary,
                                .listTablesToDF(cat_vars_summaries[[v]], v))
      
    }
    
  }
  
  # Make sure all columns are the same. Then merge and return.
  
  final_summary <- rbind(cont_vars_summary, cat_vars_summary)
  
  return(final_summary)
  
}


.listTablesToDF <- function(list_tables, varName) {
  
  for (i in 1:length(list_tables)) {
    
    # get grouping var level 
    group_var_tmp <- labels(list_tables[i])
    
    # get the table
    table_tmp <- data.frame(list_tables[[i]])
    row.names(table_tmp) <- table_tmp[[1]]
    table_tmp[[group_var_tmp]] <- table_tmp[['Freq']]
    table_tmp <- subset(table_tmp, select = group_var_tmp)
    
    if (i == 1) {
    
      new_tables <- table_tmp
      
    } else {
      new_tables <- cbind(new_tables, table_tmp)
    }
    
  }
  
  # Add the entry row
  entry_row <- data.frame(t(replicate(ncol(new_tables), ' ')))
  colnames(entry_row) <- labels(list_tables)
  rownames(entry_row) <- varName
  new_tables <- rbind(entry_row, new_tables)
  
  return(new_tables)
         
}
