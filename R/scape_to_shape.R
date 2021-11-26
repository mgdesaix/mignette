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
  cut.raster.list <- (raster::cut(x, breaks = c(-Inf, prob_threshold, Inf))) %>%
    as.list()
  rtp <- function(y){
    poly.tmp <- raster::rasterToPolygons(y, function(j){j==2},dissolve=T)
    poly.tmp.cluster <- names(poly.tmp)
    names(poly.tmp) <- "Cluster"
    poly.tmp[[1]] <- poly.tmp.cluster
    sp::proj4string(poly.tmp) <- sp::CRS("+proj=longlat +datum=WGS84")
    poly.tmp <- sf::st_as_sf(poly.tmp)

    poly.tmp.dc <- smoothr::drop_crumbs(poly.tmp,
                                        threshold = units::set_units(d, km^2))
    poly.tmp.fh <- smoothr::fill_holes(poly.tmp.dc,
                                       threshold = units::set_units(f,km^2))
    poly.tmp.smooth <- smoothr::smooth(poly.tmp.fh, method = "ksmooth", smoothness = s)
    poly.tmp.smooth <- st_as_sf(poly.tmp.smooth, 'Spatial')
    return(poly.tmp.smooth)
  }
  polygon.list <- lapply(cut.raster.list, rtp)
  polygon.sf <- do.call("rbind", polygon.list)
  return(polygon.sf)
}
