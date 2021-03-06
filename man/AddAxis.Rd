% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/historyofearth.R
\name{AddAxis}
\alias{AddAxis}
\title{function to getPhyloPicIDNum axis to a tree plot}
\usage{
AddAxis(lastPP = get("last_plot.phylo", envir = .PlotPhyloEnv),
  focalPeriod = NULL, age_df = GetAgeDF())
}
\arguments{
\item{lastPP}{The output of plotting functions}

\item{focalPeriod}{The period to highlight, using names form GetAgeDF()}

\item{age_df}{Return from GetAgeDF(), which has colors}
}
\value{
modifies current plot
}
\description{
It can highlight chosen periods
}
