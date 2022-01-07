#' Q-values and metadata for the Common Yellowthroat breeding samples
#'
#' The tibble provides metadata for all of the individuals sampled from the breeding range used to create the genoscape map.
#' Each individual has a unique identifier, location data, and Q values provided from Structure.
#'
#' @format A tibble with 198 rows, 10 columns:
#' \describe{
#'   \item{Sample}{Individual identifier}
#'   \item{Assignment}{Assigned genetic cluster based on Q-values with a threshold of 0.7}
#'   \item{State}{State locality of sampled individual}
#'   \item{Lat}{Latitude}
#'   \item{Long}{Longitude}
#'   \item{CA}{Q-values for California cluster}
#'   \item{Midwest}{Q-values for Midwest cluster}
#'   \item{NewEngland}{Q-values for New England cluster}
#'   \item{West}{Q-values for West cluster}
#'   \item{Southwest}{Q-values for Southwest cluster}
#' }
"comyel_breeding_data"

#' Ecoregions of the western hemisphere
#'
#' These polygons were simplified with rmapshaper::ms_simplify()
#'
#' @format A simple feature collection with 27 features (ecoregions) and 8 fields
#' \describe{
#' }
"ecoregions_simple"

#' Metadata for the Common Yellowthroat wintering samples
#'
#' The tibble provides metadata for all of the individuals sampled from the wintering range.
#' Each individual has a unique identifier, location data, winter sampling coordinates, and breeding population assignment from Rubias.
#'
#' @format A tibble with 106 rows, 5 columns:
#' \describe{
#'   \item{Sample}{Individual identifier}
#'   \item{Location}{Wintering site}
#'   \item{Lat}{Latitude of wintering site}
#'   \item{Long}{Longitude of wintering site}
#'   \item{BreedingAssignment}{Breeding population the sample was assigned to with Rubias}
#' }
"comyel_wintering_data"
