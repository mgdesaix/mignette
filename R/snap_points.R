#' Snap points to polygons
#'
#' @param x Points to snap
#' @param y Polygons to snap to
#' @param max_dist Distance threshold
#' @return Returns points snapped to polygons
#' @export
#'
snap_points <- function(x, y, max_dist = 1000) {

  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)

  out <- do.call(c,
                 lapply(seq(n), function(i) {
                   nrst <- sf::st_nearest_points(sf::st_geometry(x)[i], y)
                   nrst_len <- sf::st_length(nrst)
                   nrst_mn <- which.min(nrst_len)
                   if (as.vector(nrst_len[nrst_mn]) > max_dist) return(sf::st_geometry(x)[i])
                   return(sf::st_cast(nrst[nrst_mn], "POINT")[2])
                 })
  )
  return(out)
}
