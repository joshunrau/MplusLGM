% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotModel.R
\name{plotModel}
\alias{plotModel}
\title{plotModel}
\usage{
plotModel(
  model,
  x_axis_label = "time",
  y_axis_label = "variable",
  figure_caption = NULL,
  geom_line2 = NULL,
  geom_point2 = NULL
)
}
\arguments{
\item{model}{An MplusObject containing results}

\item{x_axis_label}{A character vector containing text for x-axis}

\item{y_axis_label}{A character vector containing text for y-axis}

\item{figure_caption}{A character vector containing text to be added as a caption}

\item{geom_line2}{Specify an additional geom for ggplot}

\item{geom_point2}{Specify an additional geom for ggplot}
}
\value{
A ggplot object
}
\description{
Plots class trajectories of an MplusObject
}
