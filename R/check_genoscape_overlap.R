#' Check which genoscape polygons overlap
#'
#' @param x A polygon sf object for all genoscape clusters
#'
#' @return List of genoscape clusters that overlap
#' @export
#'
check_genoscape_overlap <- function(x){
  origin.list <- sf::st_intersection(x) %>%
    dplyr::filter(n.overlaps > 1) %>%
    dplyr::pull(origins)
  new.list <- utils::relist(x$Cluster[unlist(origin.list)], skeleton = origin.list)
  return(new.list)
  print(new.list)
}
