get_season_shape <- function(range_smooth, species,
                             season = "breeding"){
  breed_season <- dplyr::filter(range_smooth,
                                season == season,
                                layer == "range")
  breed.wgs <- sf::st_transform(breed_season, crs = 4326)
  out.name.breed.wgs <- paste0(species, ".", season, ".sf.WGS84.ebird")

  sf::st_write(breed.wgs, dsn = '.',
           layer = out.name.breed.wgs,
           driver = "ESRI Shapefile")
}
