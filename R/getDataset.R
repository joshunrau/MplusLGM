#' @title getDataset
#' @description Selects the best model from a list of MplusObjects based on a
#'     specified method. Current selection methods are "BIC" (select model with 
#'     the lowest BIC) and "BIC_LRT" (select model with the lowest BIC that also 
#'     has a significant LRT p-value).
#' @param model An MplusObject containing save data
#' @param df A data frame to combine with Mplus classes
#' @param idvar A character vector containing the ID variable in the data frame
#' @return An MplusObject
#' @export
getDataset <- function(model, df, idvar) {
  
  C <- model[["results"]][["savedata"]][["C"]]
  ID <- model[["results"]][["savedata"]][["ID"]]
  
  class_res <- data.frame(ID, C)
  colnames(class_res) <- c(idvar, 'class')
  
  df_final <- merge(df, class_res, by = idvar)
  df_final$class <- as.factor(df_final$class)
  
  return(df_final)
}