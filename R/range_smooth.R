#' Convert seasonable abundance raster to shapefile
#'
#' @param abd_season_proj Name of seasonal abundance RasterStack (projected)
#' @param path Output directory
#' @param species Six-letter code for the bird species from ebirdst
#' @param ne_land Natural Earth land data (same projection as abd_season_proj)
#' @param pred_region Predicted region
#' @param split_migration TRUE/FALSE of whether to split spring and fall migration
#' @param show_yearround TRUE/FALSE of whether to create shapefile for year round abundance
#'
#' @return Sf object for all seasons
#' @export
#'
range_smooth <- function(abd_season_proj,
                             path = "./",
                             species,
                             ne_land,
                             pred_region,
                             split_migration = FALSE,
                             show_yearround = FALSE){

  # # determine spatial extent for plotting
  # # ext <- ebirdst::calc_full_extent(abd_season_proj)
  # # set the plotting order of the seasons
  # season_order <- c("postbreeding_migration", "prebreeding_migration",
  #                   "nonbreeding", "breeding")
  #
  # # remove zeros from abundnace layers
  # abd_no_zero <- terra::subst(abd_season_proj, from = 0, to = NA)
  # # seasonal layer
  # plot_seasons <- raster::intersect(season_order, names(abd_no_zero))
  # # legends
  # legend_seasons <- plot_seasons
  # if (split_migration) {
  #   legend_seasons[legend_seasons %in% c("prebreeding_migration",
  #                                        "postbreeding_migration")] <- "migration"
  #   legend_seasons <- unique(legend_seasons)
  # }
  # if (show_yearround) {
  #   legend_seasons <- c(legend_seasons, "year_round")
  # }

  abd_season_agg <- terra::aggregate(abd_season_proj, fact = 3)
  # raster to polygon, one season at a time
  range <- list()
  pred_area <- list()

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
      dplyr::mutate(season = s, layer = "range")
    # prediction area
    pred_area[[s]] <- terra::as.polygons(abd_season_agg[[s]]) %>%
      sf::st_as_sf() %>%
      # combine polygon pieces into a single multipolygon
      sf::st_set_precision(1e6) %>%
      sf::st_union() %>%
      sf::st_sf() %>%
      # tag layers with season
      dplyr::mutate(season = s, layer = "prediction_area")
  }

  # combine the sf objects for all seasons
  range <- rbind(do.call(rbind, range), do.call(rbind, pred_area))
  row.names(range) <- NULL

  cell_area <- (1.5 * prod(terra::res(abd_season_agg)))
  range_smooth <- range %>%
    # drop fragment polygons smaller than 1.5 times the aggregated cell size
    smoothr::drop_crumbs(threshold = cell_area) %>%
    # drop holes in polygons smaller than 1.5 times the aggregated cell size
    smoothr::fill_holes(threshold = cell_area) %>%
    # smooth the polygon edges
    smoothr::smooth(method = "ksmooth", smoothness = 2)
  # clip zeros to land border, range to buffered land to handle coastal species
  range_split <- split(range_smooth, range_smooth$layer)
  # mask to land area
  ne_land_buffer <- sf::st_buffer(ne_land, dist = max(terra::res(pred_region)) / 2)
  pred_region <- terra::mask(pred_region, terra::vect(ne_land_buffer))

  range_smooth <- rbind(
    sf::st_intersection(sf::st_transform(range_split$range, crs = sf::st_crs(ne_land_buffer)), ne_land_buffer),
    sf::st_intersection(sf::st_transform(range_split$prediction_area, crs = sf::st_crs(ne_land)), ne_land))
  out.name.range_smooth <- paste0(path, species, ".all_season_smooth")
  sf::st_write(range_smooth, dsn = '.',
           layer = out.name.range_smooth,
           driver = "ESRI Shapefile")
  return(range_smooth)
}
