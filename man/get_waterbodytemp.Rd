% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/habitat_suitability.R
\name{get_waterbodytemp}
\alias{get_waterbodytemp}
\title{Get WesternAIS Samples}
\usage{
get_waterbodytemp(waterbodies)
}
\arguments{
\item{waterbodies}{A sf object with USGS waterbodies.}
}
\description{
This function calls the \verb{https://gis.psmfc.org/server/rest/services/WesternAIS/Quagga_and_Zebra_Mussel_Monitoring_Sites/MapServer/} API and filters sites based on
water temperature.
}
