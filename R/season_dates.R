season_dates <- function(species, path = "./"){
  sp_path <- ebirdst::ebirdst_download(species = species)
  abd <- ebirdst::load_raster(sp_path, product = "abundance")
  season_dates <- dplyr::filter(ebirdst::ebirdst_runs, species_code == species) %>%
    dplyr::select(dplyr::setdiff(dplyr::matches("(start)|(end)"),
                                 dplyr::matches("year_round"))) %>%
    tidyr::gather("label", "date") %>%
    tidyr::separate(label, c("season", "start_end"), "_(?=s|e)") %>%
    tidyr::spread(start_end, date) %>%
    dplyr::select(season, start, end) %>%
    dplyr::mutate(pass = !(is.na(start) | is.na(end)))

  weeks <- ebirdst::parse_raster_dates(abd)
  weeks_season <- rep(NA_character_, length(weeks))
  for (i in seq_len(nrow(season_dates))) {
    s <- season_dates[i, ]
    if (!s$pass) {
      next()
    }
    if (s$start <= s$end) {
      in_season <- weeks >= s$start & weeks <= s$end
    } else {
      in_season <- weeks >= s$start | weeks <= s$end
    }
    weeks_season[in_season] <- s$season
  }
  week_pass <- !is.na(weeks_season)
  abd <- abd[[which(week_pass)]]
  weeks <- weeks[week_pass]
  weeks_season <- weeks_season[week_pass]
  mean_season <- function(s) {
    raster::mean(abd[[which(weeks_season == s)]], na.rm = T)
  }
  seasons <- unique(weeks_season)

  abd_season <- lapply(seasons, mean) %>%
    raster::stack() %>%
    stats::setNames(seasons)
}
