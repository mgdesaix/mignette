#' Get relative abundance for genoscape
#'
#' @param populations SpatRaster object of layers with values delineating membership probabilities to a population
#' @param abunds SpatRaster object of ebirdst abundance
#' @param pop_names Character vector of raster layer names manually provided. By default the function obtains this from `names(raster)`.
#' @return A two-column matrix with population names in the first column and relative abundance values in the second column
#' @export
#'
#'
get_raster_abunds <- function(populations, abunds, pop_names = names(populations)){
  stopifnot("`populations` layers and `pop_names` file of different lengths!" = length(names(populations)) == length(pop_names))
  spp_pops <- pop_names
  # Load rasters of q values
  for(i in 1:terra::nlyr(populations)){
    populations[[i]][populations[[i]] < 0] <- 0
  }
  # Vectorize rasters of q values
  genoscape_vect <- terra::as.polygons(populations, dissolve=F)

  # Rescale q values to probabilities using the function P = exp(-(b/qval))/sum(exp(-(b/qval)))
  b <- 1
  # The value of b is set above
  terra::values(genoscape_vect) <- as.data.frame(t(apply(terra::values(genoscape_vect), 1, function(x) exp(-(b/x))/sum(exp(-(b/x))))))

  abunds <- terra::project(abunds, '+proj=longlat +datum=WGS84', method = "near")
  abunds[abunds > 1000] <- 0

  terra::crs(populations) <- terra::crs(genoscape_vect) <- terra::crs(abunds) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  # Change extent
  abunds_br <- terra::crop(abunds, terra::ext(populations))

  # Get pop size per pixel
  abunds_br_genoscape <- terra::extract(abunds_br, genoscape_vect, weights=T, fun=sum, na.rm=TRUE)
  genoscape_vect$popsize <- abunds_br_genoscape[,2]
  # Rasterize the genoscape
  genoscape2 <- terra::rasterize(genoscape_vect, populations, colnames(terra::values(genoscape_vect))[1])
  for(i in 2:ncol(genoscape_vect)){
    genoscape2 <- c(genoscape2, terra::rasterize(genoscape_vect, populations, colnames(terra::values(genoscape_vect))[i]))
  }

  # Abundance surface per genetically distinct population
  for(i in 1:(terra::nlyr(genoscape2)-1)){
    genoscape2 <- c(genoscape2, genoscape2[[i]] * genoscape2[[terra::nlyr(populations)+1]])
  }
  names(genoscape2) <- c(spp_pops, "relative_abundance", paste0(spp_pops, "_abund"))

  # Breeding abundance per genetically distinct population
  spp_pops_abunds <- cbind(spp_pops,
                           apply(terra::values(genoscape2)[,(ceiling(terra::nlyr(genoscape2)/2)+1):terra::nlyr(genoscape2)], 2, function(x) sum(x, na.rm=T))
  )

  colnames(spp_pops_abunds) <- c("Population", "Relative_abundance")
  rownames(spp_pops_abunds) <- NULL
  return(spp_pops_abunds)
}
