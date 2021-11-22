#' Seasonal abundance of the Common Yellowthroat
#'
#' A raster with different seasonal layers of abundance for the Common Yellowthroat
#' base on ebirdst data.
#'
#' @format A RasterStack with abundance for 4 stages: nonbreeding, prebreeding_migration, breeding, and postbreeding_migration
#'
#' @source \url{https://ebird.org/science/status-and-trends}
"comyel_abd_season_proj"

#' Breeding range of the Common Yellowthroat
#'
#' A polygon of the breeding range based on the seasonal abundance data
#'
#' @format A multipolygon with 1 feature and 2 fields:
#' \describe{
#'   \item{season}{breeding}
#'   \item{layer}{range}
#' }
#' @source \url{https://ebird.org/science/status-and-trends/comyel}
"comyel_breed_smooth"

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
