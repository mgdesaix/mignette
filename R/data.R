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
