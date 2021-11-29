#' Convert seasonable abundance raster to shapefile
#'
#' @param abd_season Name of seasonal abundance RasterStack (projected)
#' @param ne_land Natural Earth land data (same projection as abd_season)
#' @param smooth_area km^2 region to smooth
#' @param split_migration TRUE/FALSE of whether to split spring and fall migration
#' @param show_yearround TRUE/FALSE of whether to create shapefile for year round abundance
#'
#' @return Sf object for all seasons
#' @export
#'
range_smooth <- function(abd_season,
                             ne_land,
                             smooth_area = 1000,
                             split_migration = FALSE,
                             show_yearround = FALSE){

  abd_season_agg <- terra::aggregate(abd_season, fact = 3)
  # raster to polygon, one season at a time
  range <- list()

  for (s in names(abd_season_agg)) {
    # range
    range[[s]] <- terra::subst(abd_season_agg[[s]], from = 0, to = NA) %>%
      terra::as.polygons() %>%
      sf::st_as_sf() %>%
      # combine polygon pieces into a single multipolygon
      sf::st_set_precision(1e6) %>%
      sf::st_union() %>%
      sf::st_sf() %>%
      # tag layers with season
      dplyr::mutate(season = s)
  }

  # combine the sf objects for all seasons
  range <- do.call(rbind, range)
  row.names(range) <- NULL

  cell_area <- units::set_units(smooth_area, km^2)
  range_smooth <- range %>%
    # drop fragment polygons smaller than user specified size
    smoothr::drop_crumbs(threshold = cell_area) %>%
    # drop holes in polygons smaller than user specified size
    smoothr::fill_holes(threshold = cell_area) %>%
    # smooth the polygon edges
    smoothr::smooth(method = "ksmooth", smoothness = 2)

  # mask to land area
  ne_land_buffer <- sf::st_buffer(ne_land, dist = max(terra::res(abd_season)) / 2)

  range_smooth_masked <- sf::st_intersection(sf::st_transform(range_smooth, crs = sf::st_crs(ne_land_buffer)),
                                      ne_land_buffer)
  return(range_smooth_masked)
}
