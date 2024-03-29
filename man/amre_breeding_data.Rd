% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{amre_breeding_data}
\alias{amre_breeding_data}
\title{Q-values and metadata for the American Redstart breeding samples}
\format{
A tibble with 169 rows, 10 columns:
\describe{
\item{Sample}{Individual identifier}
\item{Breeding_pop}{Breeding population based on admixture Q-values with a threshold of 0.7}
\item{Site}{State locality of sampled individual}
\item{Lat}{Latitude}
\item{Lon}{Longitude}
\item{BR}{Q-values for Basin Rockies cluster}
\item{ST}{Q-values for Southern Temperate cluster}
\item{MP}{Q-values for Maritime Provinces cluster}
\item{NT}{Q-values for Northern Temperate cluster}
\item{WB}{Q-values for Western Boreal cluster}
}
}
\usage{
amre_breeding_data
}
\description{
The tibble provides metadata for all of the individuals sampled from the breeding range used to create the genoscape map.
Each individual has a unique identifier, location data, and Q values provided from Structure.
}
\keyword{datasets}
