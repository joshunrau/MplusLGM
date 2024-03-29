% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/refinePolynomial.R
\name{refinePolynomial}
\alias{refinePolynomial}
\title{refinePolynomial}
\usage{
refinePolynomial(model, df, usevar, timepoints, idvar, working_dir = getwd())
}
\arguments{
\item{model}{An MplusObject}

\item{df}{A data frame containing all user variables and the ID variable}

\item{usevar}{A character vector containing variables to be used in Mplus
for analysis}

\item{timepoints}{A vector containing the timepoints corresponding 
to the elements in the usevar vector}

\item{idvar}{A character vector containing the ID variable in the data frame}

\item{working_dir}{The directory where the results folder will be created}
}
\value{
An MplusObject
}
\description{
Given an MplusModel, refines the growth factors of each class in
    the model.
}
