% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/habitat_suitability.R
\name{get_wqp}
\alias{get_wqp}
\title{Get Water Quality Data}
\usage{
get_wqp(statecode, huc12)
}
\arguments{
\item{statecode}{A statecode abreviation, e.g. 'ID'.}

\item{huc12}{A sf object with USGS HUC 12s.}
}
\description{
This function calls the Water Quality Portal from the \link\link{dataRetrieval} package to query specific water quality paramaters.
This will take a while to download and also will take a decent amount of space (> Gb) so be aware before running!
}
