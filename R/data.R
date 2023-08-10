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

#' American Redstart assignment data
#'
#' The tibble provides the assignment between three populations on each the breeding and nonbreeding range.
#' The assignment data was created using genetic population assignment of individuals sampled on the nonbreeding range to genetically-distinct breeding populations.
#' @format A tibble with 3 rows, 4 columns:
#' \describe{
#'   \item{Breeding}{ID of the genetically distinct breeding population}
#'   \item{CAR}{Number of individuals from the Caribbean ecoregion assigned to the corresponding breeding population}
#'   \item{AONU}{Number of individuals from the Amazon/Orinoco-Northern Uplands ecoregion assigned to the corresponding breeding population}
#'   \item{ALM}{Number of individuals from the Atlantic Lowland Mexico ecoregion assigned to the corresponding breeding population}
#' }
"amre_assign"

#' American Redstart population relative abundance
#'
#' The tibble provides relative abundance for 6 populations.
#'
#' @format A tibble with 6 rows, 2 columns:
#' \describe{
#'   \item{Population}{ID of the population}
#'   \item{Relative_abundance}{The relative abundance of the population}
#' }
"amre_abundance"

#' American Redstart connectivity
#'
#' The matrix provides the connectivity output from the JAGS model with `amre_assign` and `amre_abundance` data
#'
#' @format A tibble with 3 rows, 3 columns:
#' \describe{
#'   \item{1}{Connectivity of nonbreeding population ALM with breeding populations WB, NT, and ST}
#'   \item{2}{Connectivity of nonbreeding population CAR with breeding populations WB, NT, and ST}
#'   \item{3}{Connectivity of nonbreeding population AONU with breeding populations WB, NT, and ST}
#' }
"amre_conn"
