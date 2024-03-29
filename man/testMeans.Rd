% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/testMeans.R
\name{testMeans}
\alias{testMeans}
\title{testMeans}
\usage{
testMeans(list_vars, group_var, df, correct = TRUE)
}
\arguments{
\item{list_vars}{List of variables to test}

\item{group_var}{Variable to group by}

\item{df}{A data frame}

\item{correct}{Apply Holm correction}
}
\value{
A data frame
}
\description{
Run one-way ANOVA or chi-squared tests
}
