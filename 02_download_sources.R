# =============================================================================
# 02_download_sources.R
# Downloads raw, open-access data into 01_raw/. Runs once when sourced.
# Key changes vs previous version:
#   * NASA POWER is sampled at ADM2 centroids (~75 points), not ADM1 (12).
#   * Adds WorldPop gridded population raster (a true spatial/raster layer).
#   * UN Data Portal block rewritten (the old `|> .[1]` was invalid syntax).
#   * Consistent lowercase boundary filenames + single path scheme.
# =============================================================================

# ---- 1. Administrative boundaries: geoBoundaries ADM1 + ADM2 ----------------
download_geoboundaries <- function(iso3 = country_iso3, levels = c("ADM1", "ADM2")) {
  for (adm in levels) {
    out_file <- adm_path(adm)
    if (file.exists(out_file)) { message("Boundary present: ", basename(out_file)); next }
    api_url <- glue::glue("https://www.geoboundaries.org/api/current/gbOpen/{iso3}/{adm}/")
    meta <- httr2::request(api_url) |> httr2::req_perform() |> httr2::resp_body_json()
    dl_url <- meta$gjDownloadURL %||% meta$staticDownloadLink
    ok <- safe_download(dl_url, out_file)
    log_event(glue::glue("geoBoundaries {adm}"), "geoBoundaries", dl_url,
              ifelse(ok, "downloaded", "failed"),
              "Admin boundary GeoJSON; converted to GPKG/SHP in step 03.")
  }
}
download_geoboundaries()
getwd()
# ---- 2. WorldPop gridded population (1 km, UN-adjusted) ---------------------
# A genuine raster layer -> zonal population per province in step 03.
if (isTRUE(download_worldpop)) {
  wp_url <- glue::glue(
    "https://data.worldpop.org/GIS/Population/Global_2000_2020/",
    "{worldpop_year}/{country_iso3}/{tolower(country_iso3)}_ppp_{worldpop_year}_1km_Aggregated_UNadj.tif"
  )
  wp_file <- file.path(path_worldpop, glue::glue("{tolower(country_iso3)}_ppp_{worldpop_year}_1km_UNadj.tif"))
  ok <- safe_download(wp_url, wp_file)
  log_event("WorldPop population 1km", "WorldPop", wp_url,
            ifelse(ok, "downloaded", "failed"),
            "Top-down UN-adjusted population grid; aggregated to ADM2 in step 03.")
}

# ---- 3. NASA POWER daily climate at ADM2 centroids -------------------------
adm2 <- sf::st_read(adm_path("ADM2"), quiet = TRUE) |>
  sf::st_make_valid() |> sf::st_transform(4326) |> janitor::clean_names()

# robust name + id columns (geoBoundaries -> shape_name / shape_id)
name_col <- intersect(c("shape_name", "name", "adm2_name", "province"), names(adm2))[1]
if (is.na(name_col)) name_col <- setdiff(names(adm2), attr(adm2, "sf_column"))[1]
id_col   <- intersect(c("shape_id", "shapeid"), names(adm2))[1]

adm2 <- adm2 |>
  dplyr::mutate(
    adm2_name = as.character(.data[[name_col]]),
    adm2_id   = if (!is.na(id_col)) as.character(.data[[id_col]]) else as.character(dplyr::row_number())
  )

adm2_pts  <- sf::st_point_on_surface(adm2)
coords    <- sf::st_coordinates(adm2_pts)
centroids <- adm2_pts |>
  dplyr::mutate(lon = coords[, 1], lat = coords[, 2]) |>
  sf::st_drop_geometry() |>
  dplyr::select(adm2_id, adm2_name, lon, lat)

safe_write_csv(centroids, file.path(path_nasa, "morocco_adm2_centroids_for_nasa_power.csv"))

parse_nasa_power <- function(json_file, adm2_id, adm2_name, lon, lat) {
  x <- jsonlite::fromJSON(json_file)
  params <- x$properties$parameter
  purrr::imap_dfr(params, function(values, var) {
    tibble::tibble(date = as.Date(names(values), format = "%Y%m%d"),
                   variable = var, value = as.numeric(values))
  }) |>
    tidyr::pivot_wider(names_from = variable, values_from = value) |>
    dplyr::mutate(adm2_id = adm2_id, adm2_name = adm2_name,
                  lon = lon, lat = lat, .before = 1)
}

nasa_all <- list()
for (i in seq_len(nrow(centroids))) {
  cid  <- centroids$adm2_id[i]; cname <- centroids$adm2_name[i]
  lon  <- centroids$lon[i];     lat   <- centroids$lat[i]
  safe <- janitor::make_clean_names(paste0(cid, "_", cname))
  csv_file  <- file.path(path_nasa, glue::glue("nasa_power_daily_{safe}_{start_year}_{end_year}.csv"))
  json_file <- file.path(path_nasa, glue::glue("nasa_power_daily_{safe}_{start_year}_{end_year}.json"))

  if (file.exists(csv_file)) {                          # cache: don't re-download
    nasa_all[[cid]] <- readr::read_csv(csv_file, show_col_types = FALSE); next
  }
  api <- glue::glue(
    "https://power.larc.nasa.gov/api/temporal/daily/point?",
    "parameters={paste(nasa_params, collapse = ',')}&community=AG",
    "&longitude={lon}&latitude={lat}",
    "&start={start_year}0101&end={end_year}1231&format=JSON"
  )
  ok <- safe_download(api, json_file, quiet = TRUE)
  if (ok) {
    dat <- tryCatch(parse_nasa_power(json_file, cid, cname, lon, lat),
                    error = function(e) NULL)
    if (!is.null(dat)) { safe_write_csv(dat, csv_file); nasa_all[[cid]] <- dat }
  }
  Sys.sleep(0.5)  # be polite to the API
}
if (length(nasa_all)) {
  safe_write_csv(dplyr::bind_rows(nasa_all),
                 file.path(path_nasa, glue::glue("nasa_power_daily_morocco_adm2_{start_year}_{end_year}.csv")))
}
log_event("NASA POWER daily climate (ADM2)", "NASA POWER API",
          "https://power.larc.nasa.gov/api/temporal/daily/point", "downloaded",
          glue::glue("Daily climate at {nrow(centroids)} ADM2 centroids, {start_year}-{end_year}."))

# ---- 4. World Bank WDI / ITU / UN national context indicators --------------
wdi_raw <- tryCatch(
  WDI::WDI(country = country_iso2, indicator = wdi_indicators,
           start = 2010, end = as.integer(format(Sys.Date(), "%Y")), extra = TRUE),
  error = function(e) { message("WDI failed: ", conditionMessage(e)); NULL })
if (!is.null(wdi_raw)) {
  safe_write_csv(wdi_raw, file.path(path_wdi, "worldbank_wdi_itu_un_morocco.csv"))
  log_event("World Bank WDI / ITU / UN", "World Bank API", "https://api.worldbank.org/v2/",
            "downloaded", "National context indicators incl. ITU technology + UN demographics.")
}

# ---- 5. UN Population Division Data Portal (catalog, robust) ----------------
un_base <- "https://population.un.org/dataportalapi/api/v1"
ok_ind <- safe_download(paste0(un_base, "/indicators?format=csv&sort=name"),
                        file.path(path_un, "un_indicators_catalog.csv"), quiet = TRUE)
ok_loc <- safe_download(paste0(un_base, "/locations?format=csv&sort=name"),
                        file.path(path_un, "un_locations_catalog.csv"), quiet = TRUE)
log_event("UN Population Data Portal catalog", "UN Population Division",
          paste0(un_base, "/indicators"), ifelse(ok_ind && ok_loc, "downloaded", "partial"),
          "Catalogs saved for documentation; national series only (not ADM2-resolved).")


# ---- 6. FAOSTAT land-use (national context; OPTIONAL, no interactive login) -
if (isTRUE(get0("download_fao", ifnotfound = FALSE))) {
  tryCatch({
    fao_land <- FAOSTAT::get_faostat_bulk("RL")
    fao_mar  <- fao_land |> dplyr::filter(stringr::str_to_lower(Area) == "morocco")
    safe_write_csv(fao_mar, file.path(path_fao, "faostat_land_use_morocco_raw.csv"))
    log_event("FAOSTAT Land Use", "FAOSTAT", "https://www.fao.org/faostat/", "downloaded",
              "Morocco land-use subset (national).")
  }, error = function(e)
    log_event("FAOSTAT Land Use", "FAOSTAT", "https://www.fao.org/faostat/", "failed",
              conditionMessage(e)))
} else {
  log_event("FAOSTAT Land Use", "FAOSTAT", "https://www.fao.org/faostat/", "skipped",
            "Skipped to avoid interactive login; national context only.")
}

# # ---- 6. FAOSTAT land-use (national context; wrapped, non-fatal) -------------
# tryCatch({
#   fao_land <- FAOSTAT::get_faostat_bulk("RL")
#   fao_mar  <- fao_land |> dplyr::filter(stringr::str_to_lower(Area) == "morocco")
#   safe_write_csv(fao_mar, file.path(path_fao, "faostat_land_use_morocco_raw.csv"))
#   log_event("FAOSTAT Land Use", "FAOSTAT", "https://www.fao.org/faostat/", "downloaded",
#             "Morocco land-use subset (national).")
# }, error = function(e)
#   log_event("FAOSTAT Land Use", "FAOSTAT", "https://www.fao.org/faostat/", "failed",
#             conditionMessage(e)))

# ---- 7. OpenStreetMap raw extract (Geofabrik GeoPackage) -------------------
osm_url <- "https://download.geofabrik.de/africa/morocco-latest-free.gpkg.zip"
if (isTRUE(download_heavy_osm)) {
  osm_zip <- file.path(path_osm, "morocco-latest-free.gpkg.zip")
  ok <- safe_download(osm_url, osm_zip)
  if (ok && length(list.files(path_osm, pattern = "\\.gpkg$")) == 0) unzip(osm_zip, exdir = path_osm)
  log_event("OpenStreetMap Geofabrik", "Geofabrik / OSM", osm_url,
            ifelse(ok, "downloaded", "failed"),
            "Road + POI layers used as ADM2 technology-access proxies.")
} else {
  log_event("OpenStreetMap Geofabrik", "Geofabrik / OSM", osm_url, "skipped",
            "download_heavy_osm = FALSE.")
}

message("Raw downloads complete.")
