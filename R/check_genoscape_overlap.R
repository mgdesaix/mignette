#' Check which genoscape polygons overlap
#'
#' @param x A polygon sf object for all genoscape clusters
#'
#' @return List of genoscape clusters that overlap
#' @export
#'
check_genoscape_overlap <- function(x){
  genoscape.vec <- terra::vect(x)
  genoscape.overlap <- terra::relate(genoscape.vec, relation = "overlaps", pairs = T, symmetrical = T)
  genoscape.overlap <- genoscape.overlap[genoscape.overlap[,3] > 0, c(1,2)]
  out.pairs <- matrix(x$Cluster[genoscape.overlap], nrow = dim(genoscape.overlap)[1])
  return(out.pairs)
  print(out.pairs)
}
