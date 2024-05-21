#' Get relative abundance for vector files
#'
#' @param populations SpatVector object of population boundaries
#' @param abunds SpatRaster object of ebirdst abundance
#' @param pop_names Character vector of population names manually provided. By default the function obtains this from `populations[[1]][[1]]`.
#' @return A two-column matrix with population names in the first column and relative abundance values in the second column
#' @export
#'

get_vector_abunds <- function(populations, abunds, pop_names){

  stopifnot("`populations` layers and `pop_names` file of different lengths!" = dim(populations)[1] == length(pop_names))
  stopifnot("Coordinate reference systems are not the same for `populations` and `abunds` objects (check crs)" = terra::crs(populations) == terra::crs(abunds))


  abunds_crop <- terra::crop(abunds, terra::ext(populations)) # Crop abundance by polygons
  abunds_values <- terra::extract(abunds_crop, populations, weights=T, na.rm=TRUE) # extract values
  final_abunds <- abunds_values %>%
    tidyr::drop_na() %>%
    dplyr::mutate(Relative_abundance = .[[2]]*.[[3]]) %>%
    dplyr::group_by_at(1) %>%
    dplyr::summarize(Relative_abundance = sum(Relative_abundance)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Relative_abundance)

  final_abunds_df <- data.frame("Population" = pop_names,
                                "Relative_abundance" = final_abunds)

  return(final_abunds_df)
}
