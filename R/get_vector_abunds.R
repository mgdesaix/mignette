#' Get relative abundance for vector files
#'
#' @param populations SpatVector object of population boundaries
#' @param abunds SpatRaster object of ebirdst abundance
#' @param pop_names Character vector of population names manually provided. By default the function obtains this from `populations[[1]][[1]]`.
#' @return A two-column matrix with population names in the first column and relative abundance values in the second column
#' @export
#'

get_vector_abunds <- function(populations, abunds, pop_names = populations[[1]][[1]]){

  stopifnot("`populations` layers and `pop_names` file of different lengths!" = length(populations[[1]][[1]]) == length(pop_names))

  abunds <- terra::project(abunds, '+proj=longlat +datum=WGS84', method = "near")
  abunds[abunds > 1000] <- 0

  terra::crs(populations) <- terra::crs(abunds) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  abunds_nb <- terra::crop(abunds, terra::ext(populations)) # c(-170, -30, 0, 60)
  abunds_nb_ecoregions <- terra::extract(abunds_nb, populations, weights=T, na.rm=TRUE)
  abunds_nb_ecoregions_w <- abunds_nb_ecoregions %>%
    tidyr::drop_na() %>%
    dplyr::mutate(Relative_abundance = .[[2]]*.[[3]]) %>%
    dplyr::group_by_at(1) %>%
    dplyr::summarize(Relative_abundance = sum(Relative_abundance)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Relative_abundance)

  spp_winterRegions_abunds <- cbind(pop_names,
                                    abunds_nb_ecoregions_w) %>%
    as.matrix()


  colnames(spp_winterRegions_abunds) <- c("Population", "Relative_abundance")
  return(spp_winterRegions_abunds)
}
