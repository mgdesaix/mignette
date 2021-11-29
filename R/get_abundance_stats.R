#' Get abundance summaries for the different nodes
#'
#' @param range_raster SpatRaster of seasonal abundance for the range of interest
#' @param node_poly Polygons of the nodes
#' @param node_names Names of the nodes
#'
#' @return
#' @export
#'
get_abundance_stats <- function(range_raster, node_poly, node_names){
  abd.1.mat <- is.na(terra::as.matrix(range_raster, wide = T))
  colNotNA <- which(colSums(abd.1.mat) != nrow(abd.1.mat))
  rowNotNA <- which(rowSums(abd.1.mat) != ncol(abd.1.mat))
  min.x <- colNotNA[1]*terra::res(range_raster)[1] + terra::ext(range_raster)[1]
  max.x <- colNotNA[length(colNotNA)] * terra::res(range_raster)[1] + terra::ext(range_raster)[1]
  min.y <- rowNotNA[1] * terra::res(range_raster)[2] + terra::ext(range_raster)[3]
  max.y <- rowNotNA[length(rowNotNA)] * terra::res(range_raster)[2] + terra::ext(range_raster)[3]
  crop.extent <- terra::ext(min.x, max.x, min.y, max.y)
  abd.cropped <- terra::crop(range_raster, crop.extent)
  summarize_abundance <- function(y, na.rm){
    return(c(mean = mean(y, na.rm=na.rm),
             sd=sd(y,na.rm = na.rm),
             min = min(y, na.rm=na.rm),
             max = max(y, na.rm=na.rm),
             total=sum(y, na.rm = na.rm)))
  }
  node_poly_proj <- node_poly %>%
    terra::vect() %>%
    terra::project("+proj=longlat + datum=WGS84")

  summary.out <- terra::extract(terra::project(abd.cropped,
                                               terra::crs(node_poly_proj)),
                                node_poly_proj,
                                fun = summarize_abundance, na.rm = TRUE)
  summary.out$ID <- node_names

  return(summary.out)
}
