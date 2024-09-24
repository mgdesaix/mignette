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

#' Metadata for the American Redstart samples from the stationary nonbreeding range
#'
#' The tibble provides metadata for all of the individuals sampled from the nonbreeding range.
#' Each individual has a unique identifier, location data, nonbreeding sampling coordinates, and breeding population assignment from the WGSassign software
#'
#' @format A tibble with 131 rows, 6 columns:
#' \describe{
#'   \item{Sample}{Individual identifier}
#'   \item{Site}{Nonbreeding sampling site}
#'   \item{Nonbreeding_region}{Conservation region of the sampling site}
#'   \item{Lat}{Latitude of nonbreeding site}
#'   \item{Long}{Longitude of nonbreeding site}
#'   \item{Breeding_assignment}{Breeding population the sample was assigned to}
#' }
"amre_nonbreeding_data"

#' Assignment probabilities for the American Redstart nonbreeding data
#'
#' The tibble provides assignment probabilities the individuals sampled from the nonbreeding range.
#' Assignment probabilities are provided from the DeSaix et al., 2023 dataset using the assignment consistency.
#'
#' @format A tibble with 131 rows, 7 columns:
#' \describe{
#'   \item{Sample}{Individual identifier}
#'   \item{WB}{Western Boreal assignment probability}
#'   \item{BR}{Basin Rockies assignment probability}
#'   \item{NT}{Northern Temperate assignment probability}
#'   \item{ST}{Southern Temperate assignment probability}
#'   \item{MP}{Maritime Provinces assignment probability}
#'   \item{Nonbreeding_region}{Conservation region of the sampling site}
#' }
"amre_nonbreeding_consistency"

#' American Redstart assignment data
#'
#' The tibble provides the number of assigned individuals from five nonbreeding ecoregions to the five breeding populations.
#' The assignment data was created using genetic population assignment of individuals sampled on the nonbreeding range to genetically-distinct breeding populations.
#' @format A tibble with 5 rows, 6 columns:
#' \describe{
#'   \item{Breeding}{ID of the genetically distinct breeding population}
#'   \item{CAR}{Number of individuals from the Caribbean ecoregion assigned to the corresponding breeding population}
#'   \item{AONU}{Number of individuals from the Amazon/Orinoco-Northern Uplands ecoregion assigned to the corresponding breeding population}
#'   \item{ALM}{Number of individuals from the Atlantic Lowland Mexico ecoregion assigned to the corresponding breeding population}
#'   \item{HCA}{Number of individuals from the Highland Central America ecoregion assigned to the corresponding breeding population}
#'   \item{LCA}{Number of individuals from the Lowland Central America ecoregion assigned to the corresponding breeding population}
#' }
"amre_assign"

#' American Redstart population relative abundance
#'
#' The tibble provides relative abundance for all nodes (breeding and nonbreeding)
#'
#' @format A tibble with 10 rows, 2 columns:
#' \describe{
#'   \item{Population}{ID of the node}
#'   \item{Relative_abundance}{The relative abundance of the node}
#' }
"amre_abundance"

#' American Redstart connectivity
#'
#' The matrix provides the connectivity output from the JAGS model with `amre_assign` and `amre_abundance` data
#'
#' @format A matrix with 5 rows, 5 columns:
#' \describe{
#'   \item{ALM}{Connectivity of nonbreeding population ALM with the breeding populations}
#'   \item{LCA}{Connectivity of nonbreeding population LCA with the breeding populations}
#'   \item{HCA}{Connectivity of nonbreeding population HCA with the breeding populations}
#'   \item{CAR}{Connectivity of nonbreeding population CAR with the breeding populations}
#'   \item{AONU}{Connectivity of nonbreeding population AONU with the breeding populations}
#' }
"amre_conn"
