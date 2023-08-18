#' Q-values and metadata for the American Redstart breeding samples
#'
#' The tibble provides metadata for all of the individuals sampled from the breeding range used to create the genoscape map.
#' Each individual has a unique identifier, location data, and Q values provided from Structure.
#'
#' @format A tibble with 169 rows, 10 columns:
#' \describe{
#'   \item{Sample}{Individual identifier}
#'   \item{Breeding_pop}{Breeding population based on admixture Q-values with a threshold of 0.7}
#'   \item{Site}{State locality of sampled individual}
#'   \item{Lat}{Latitude}
#'   \item{Lon}{Longitude}
#'   \item{BR}{Q-values for Basin Rockies cluster}
#'   \item{ST}{Q-values for Southern Temperate cluster}
#'   \item{MP}{Q-values for Maritime Provinces cluster}
#'   \item{NT}{Q-values for Northern Temperate cluster}
#'   \item{WB}{Q-values for Western Boreal cluster}
#' }
"amre_breeding_data"

#' Ecoregions of the western hemisphere
#'
#' These polygons were simplified with rmapshaper::ms_simplify()
#'
#' @format A simple feature collection with 27 features (ecoregions) and 8 fields
#' \describe{
#' }
"ecoregions_simple"

#' Metadata for the American Redstart samples from the stationary nonbreeding range
#'
#' The tibble provides metadata for all of the individuals sampled from the nonbreeding range.
#' Each individual has a unique identifier, location data, nonbreeding sampling coordinates, and breeding population assignment from the WGSassign software
#'
#' @format A tibble with 135 rows, 5 columns:
#' \describe{
#'   \item{Sample}{Individual identifier}
#'   \item{Site}{Nonbreeding sampling site}
#'   \item{Lat}{Latitude of nonbreeding site}
#'   \item{Long}{Longitude of nonbreeding site}
#'   \item{Breeding_assignment}{Breeding population the sample was assigned to}
#' }
"amre_nonbreeding_data"

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

#' American Redstart breeding range
#'
#' SpatVector object of the American redstart breeding range
#'
#' @format A SpatVector with 1 geometry, 1 attribute:
#' \describe{
#'   \item{Season}{Specifies migratory season of distribution displayed}
#' }
"amre_breeding_range"

#' American Redstart nonbreeding range
#'
#' SpatVector object of the American redstart nonbreeding range
#'
#' @format A SpatVector with 1 geometry, 1 attribute:
#' \describe{
#'   \item{Season}{Specifies migratory season of distribution displayed}
#' }
"amre_nonbreeding_range"
