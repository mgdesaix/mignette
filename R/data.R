#' Q-values and metadata for the Common Yellowthroat samples
#'
#' The tibble provides metadata for all of the individuals used to create the genoscape map.
#' Each individual has a unique identifier, location data, and Q values provided from Structure.
#'
#' @format A tibble with 234 rows, 14 columns:
#' \describe{
#'   \item{Sample}{Individual identifier}
#'   \item{Assignment}{Assigned genetic cluster based on Q-values with a threshold of 0.7}
#'   \item{CA}{Q-values for California cluster}
#'   \item{Midwest}{Q-values for Midwest cluster}
#'   \item{NewEngland}{Q-values for New England cluster}
#'   \item{West}{Q-values for West cluster}
#'   \item{Southwest}{Q-values for Southwest cluster}
#' }
"comyel_assignment"

#' Ecoregions of the western hemisphere
#'
#' These polygons were simplified with rmapshaper::ms_simplify()
#'
#' @format A simple feature collection with 27 features (ecoregions) and 8 fields
#' \describe{
#' }
"ecoregions_simple"
