#' Get relative abundance for genoscape
#'
#' @param populations SpatRaster object of layers with values delineating membership probabilities to a population
#' @param abunds SpatRaster object of abundance
#' @param pop_names Character vector of raster layer names. By default the function obtains this from `names(populations)`.
#' @return A two-column data frame with population names in the first column and relative abundance values in the second column
#' @export
#'
#'
get_raster_abunds <- function(populations, abunds, pop_names = terra::names(populations)){
  stopifnot("`populations` layers and `pop_names` file of different lengths!" = length(names(populations)) == length(pop_names))
  stopifnot("Coordinate reference systems are not the same for `populations` and `abunds` objects (check crs)" = terra::crs(populations) == terra::crs(abunds))

  spp_pops <- pop_names

  populations_01 <- populations %>%
    terra::clamp(lower = 0) %>% # classify less than 0 to 0
    terra::app(., fun=function(x) exp(-(1/x))/sum(exp(-(1/x)))) # rescale probabilities 0-1

  # standardize abundance pixels to genoscape pixels (by summing)
  abunds_br <- terra::resample(abunds, populations_01, method="sum")

  # calculate weighted abundance
  abunds_weighted_raster <- abunds_br * populations_01

  # summarize abundance
  final_abunds <- abunds_weighted_raster %>%
    terra::values() %>%
    apply(., 2, function(x) sum(x, na.rm = T)) %>%
    as.numeric()

  final_abunds_df <- data.frame("Population" = spp_pops,
                                "Relative_abundance" = final_abunds)
  return(final_abunds_df)
}
