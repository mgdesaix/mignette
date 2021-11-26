#' Plot genoscape overlap
#'
#' @param x A polygon sf object for all genoscape clusters
#' @param fill A character vector of colors
#'
#' @return A ggplot with the overlap regions in black
#' @export
#'
plot_genoscape_overlap <- function(x, fill){
  all.intersections <- sf::st_intersection(x) %>%
    dplyr::filter(n.overlaps > 1)
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = genoscape_polygon_sf,alpha = 0.75, aes(fill = Cluster)) +
    ggplot2::scale_fill_manual(values = fill) +
    ggplot2::geom_sf(data = all.intersections, fill = "Black")
}
