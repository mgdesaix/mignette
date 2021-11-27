#' Convert genoscape raster stack to polygons
#'
#' @param x Genoscape RasterStack
#' @param prob_threshold Probability value to include raster cell in polygon
#' @param d Distance threshold for smoothr::drop_crumbs()
#' @param f Distance threshold for smoothr::fill_holes()
#' @param s Smoothness
#'
#' @return A polygon sf object for all genoscape clusters
#' @export
#'
scape_to_shape <- function(x, prob_threshold = 0.5, d = 1000, f = 1000, s = 3){
  m <- c(-Inf, prob_threshold, NA)
  rclmat <- matrix(m, ncol = 3, byrow = TRUE)
  sf::sf_use_s2(FALSE)
  genoscape_classified <- terra::classify(x, rclmat, right = FALSE) %>%
    terra::as.list()

  rtp <- function(y){
    poly.tmp <- terra::as.polygons(y)
    poly.tmp.cluster <- names(poly.tmp)
    names(poly.tmp) <- "Cluster"
    poly.tmp[[1]] <- poly.tmp.cluster
    poly.tmp <- terra::project(poly.tmp, y="+proj=longlat +datum=WGS84")
    poly.tmp <- sf::st_as_sf(poly.tmp)

    poly.tmp.dc <- smoothr::drop_crumbs(poly.tmp,
                                        threshold = units::set_units(d, km^2))
    poly.tmp.fh <- smoothr::fill_holes(poly.tmp.dc,
                                       threshold = units::set_units(f,km^2))
    poly.tmp.smooth <- smoothr::smooth(poly.tmp.fh, method = "ksmooth", smoothness = s)
    poly.tmp.smooth <- sf::st_as_sf(poly.tmp.smooth, 'Spatial')
    return(poly.tmp.smooth)
  }
  polygon.list <- lapply(genoscape_classified, rtp)
  polygon.sf <- do.call("rbind", polygon.list)
  return(polygon.sf)
}
