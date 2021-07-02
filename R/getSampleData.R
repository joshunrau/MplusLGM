#' Internal function to create the testing dataset 'Diagnoses'
#' @import missForest
.getSampleData <- function() {
  
  set.seed(123)
  
  id <- 1:400
  
  dx <- c(
    replicate(100, 'Diagnosis A'),
    replicate(100, 'Diagnosis B'),
    replicate(100, 'Diagnosis C'),
    replicate(100, 'Diagnosis D')
  )
  
  sx_0 <- c(
    rpois(100, lambda = 50),
    rpois(100, lambda = 40),
    rpois(200, lambda = 14)
  )
  
  sx_1 <- c(
    rpois(100, lambda = 40),
    rpois(100, lambda = 40),
    rpois(200, lambda = 17)
  )
  
  sx_2 <- c(
    rpois(100, lambda = 30),
    rpois(100, lambda = 40),
    rpois(200, lambda = 21)
  )
  
  sx_3 <- c(
    rpois(100, lambda = 25),
    rpois(100, lambda = 40),
    rpois(200, lambda = 23)
  )
  
  sx_6 <- c(
    rpois(100, lambda = 20),
    rpois(100, lambda = 40),
    rpois(200, lambda = 32)
  )
  
  sx_9 <- c(
    rpois(100, lambda = 15),
    rpois(100, lambda = 40),
    rpois(100, lambda = 35),
    rpois(100, lambda = 41)
  )
  
  sx_12 <- c(
    rpois(100, lambda = 15),
    rpois(100, lambda = 40),
    rpois(100, lambda = 25),
    rpois(100, lambda = 50)
  )
  
  df1 <- data.frame(id, dx)
  df1$id <- as.factor(df1$id)
  df1$dx <- as.factor(df1$dx)
  
  
  df2 <- data.frame(sx_0, sx_1, sx_2, sx_3, sx_6, sx_9, sx_12)
  df2 <- missForest::prodNA(df2, .05)
  
  df2$age <- round(c(rnorm(200, 50, 8), rnorm(200, 35, 8)), digits = 2)
  df2$sex <- factor(rbinom(400, 1, .5), labels = c('Male', 'Female'))
  
  return(cbind(df1, df2))
  
}