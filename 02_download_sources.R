# =============================================================================
# 02_download_sources_revised.R
# Downloads raw open-access data into 01_raw/ and records every operation.
# =============================================================================

# ---- 1. Administrative boundaries: geoBoundaries ADM1 + ADM2 ---------------
download_geoboundaries <- function(iso3 = country_iso3, levels = c("ADM1", "ADM2")) {
  for (adm in levels) {
    out_file <- adm_path(adm)
    if (file.exists(out_file)) {
      message("Boundary present: ", basename(out_file))
      next
    }

    api_url <- glue::glue(
      "https://www.geoboundaries.org/api/current/gbOpen/{iso3}/{adm}/"
    )

    result <- tryCatch(
      {
        metadata <- httr2::request(api_url) |>
          httr2::req_perform() |>
          httr2::resp_body_json()

        download_url <- metadata$gjDownloadURL %||% metadata$staticDownloadLink
        if (is.null(download_url)) stop("No boundary download URL returned.")

        ok <- safe_download(download_url, out_file)
        log_event(
          glue::glue("geoBoundaries {adm}"),
          "geoBoundaries",
          download_url,
          ifelse(ok, "downloaded", "failed"),
          "Administrative boundary GeoJSON; standardised in step 03."
        )
        ok
      },
      error = function(e) {
        log_event(
          glue::glue("geoBoundaries {adm}"),
          "geoBoundaries",
          api_url,
          "failed",
          conditionMessage(e)
        )
        FALSE
      }
    )

    if (!isTRUE(result)) stop("Boundary download failed for ", adm, call. = FALSE)
  }
}
download_geoboundaries()

# ---- 2. WorldPop gridded population ----------------------------------------
if (isTRUE(download_worldpop)) {
  worldpop_url <- glue::glue(
    "https://data.worldpop.org/GIS/Population/Global_2000_2020/",
    "{worldpop_year}/{country_iso3}/",
    "{tolower(country_iso3)}_ppp_{worldpop_year}_1km_Aggregated_UNadj.tif"
  )

  worldpop_file <- file.path(
    path_worldpop,
    glue::glue(
      "{tolower(country_iso3)}_ppp_{worldpop_year}_1km_UNadj.tif"
    )
  )

  ok <- safe_download(worldpop_url, worldpop_file)
  log_event(
    "WorldPop population 1 km",
    "WorldPop",
    worldpop_url,
    ifelse(ok, "downloaded", "failed"),
    "UN-adjusted population grid; aggregated to ADM2 in step 03."
  )

  if (!ok) warning("WorldPop could not be downloaded; population outputs will be incomplete.")
}

# ---- 3. NASA POWER daily climate at ADM2 representative points -------------
adm2_download <- sf::st_read(adm_path("ADM2"), quiet = TRUE) |>
  sf::st_make_valid() |>
  sf::st_transform(4326) |>
  janitor::clean_names()

name_col <- intersect(
  c("shape_name", "name", "adm2_name", "province"),
  names(adm2_download)
)[1]
if (is.na(name_col)) {
  name_col <- setdiff(names(adm2_download), attr(adm2_download, "sf_column"))[1]
}
id_col <- intersect(c("shape_id", "shapeid"), names(adm2_download))[1]

adm2_download <- adm2_download |>
  dplyr::mutate(
    adm2_name = as.character(.data[[name_col]]),
    adm2_id = if (!is.na(id_col)) {
      as.character(.data[[id_col]])
    } else {
      as.character(dplyr::row_number())
    }
  )

adm2_points <- sf::st_point_on_surface(adm2_download)
coords <- sf::st_coordinates(adm2_points)
centroids <- adm2_points |>
  dplyr::mutate(lon = coords[, 1], lat = coords[, 2]) |>
  sf::st_drop_geometry() |>
  dplyr::select(adm2_id, adm2_name, lon, lat)

safe_write_csv(
  centroids,
  file.path(path_nasa, "morocco_adm2_representative_points.csv")
)

parse_nasa_power <- function(json_file, adm2_id, adm2_name, lon, lat) {
  x <- jsonlite::fromJSON(json_file)
  parameters <- x$properties$parameter

  purrr::imap_dfr(
    parameters,
    function(values, variable) {
      tibble::tibble(
        date = as.Date(names(values), format = "%Y%m%d"),
        variable = variable,
        value = as.numeric(values)
      )
    }
  ) |>
    tidyr::pivot_wider(names_from = variable, values_from = value) |>
    dplyr::mutate(
      adm2_id = adm2_id,
      adm2_name = adm2_name,
      lon = lon,
      lat = lat,
      .before = 1
    )
}

nasa_all <- list()
failed_nasa <- character()

for (i in seq_len(nrow(centroids))) {
  current_id <- centroids$adm2_id[i]
  current_name <- centroids$adm2_name[i]
  lon <- centroids$lon[i]
  lat <- centroids$lat[i]

  safe_name <- janitor::make_clean_names(paste0(current_id, "_", current_name))
  csv_file <- file.path(
    path_nasa,
    glue::glue("nasa_power_daily_{safe_name}_{start_year}_{end_year}.csv")
  )
  json_file <- file.path(
    path_nasa,
    glue::glue("nasa_power_daily_{safe_name}_{start_year}_{end_year}.json")
  )

  if (file.exists(csv_file)) {
    nasa_all[[current_id]] <- readr::read_csv(csv_file, show_col_types = FALSE)
    next
  }

  api_url <- glue::glue(
    "https://power.larc.nasa.gov/api/temporal/daily/point?",
    "parameters={paste(nasa_params, collapse = ',')}&community=AG",
    "&longitude={lon}&latitude={lat}",
    "&start={start_year}0101&end={end_year}1231&format=JSON"
  )

  ok <- safe_download(api_url, json_file, quiet = TRUE)
  if (ok) {
    data_i <- tryCatch(
      parse_nasa_power(json_file, current_id, current_name, lon, lat),
      error = function(e) {
        message("NASA parsing failed for ", current_name, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(data_i)) {
      safe_write_csv(data_i, csv_file)
      nasa_all[[current_id]] <- data_i
    } else {
      failed_nasa <- c(failed_nasa, current_id)
    }
  } else {
    failed_nasa <- c(failed_nasa, current_id)
  }

  Sys.sleep(0.5)
}

if (length(nasa_all) > 0) {
  combined_nasa <- dplyr::bind_rows(nasa_all)
  safe_write_csv(
    combined_nasa,
    file.path(
      path_nasa,
      glue::glue(
        "nasa_power_daily_morocco_adm2_{start_year}_{end_year}.csv"
      )
    )
  )
}

safe_write_csv(
  tibble::tibble(adm2_id = unique(failed_nasa)),
  file.path(path_logs, "nasa_power_failed_adm2.csv")
)

log_event(
  "NASA POWER daily climate (ADM2)",
  "NASA POWER API",
  "https://power.larc.nasa.gov/api/temporal/daily/point",
  ifelse(length(failed_nasa) == 0, "downloaded", "partial"),
  glue::glue(
    "Successful ADM2 points: {length(nasa_all)}; failed: {length(unique(failed_nasa))}."
  )
)

# ---- 4. World Bank / ITU / UN national context -----------------------------
wdi_raw <- tryCatch(
  WDI::WDI(
    country = country_iso2,
    indicator = wdi_indicators,
    start = 2010,
    end = as.integer(format(Sys.Date(), "%Y")),
    extra = TRUE
  ),
  error = function(e) {
    message("WDI download failed: ", conditionMessage(e))
    NULL
  }
)

if (!is.null(wdi_raw)) {
  safe_write_csv(
    wdi_raw,
    file.path(path_wdi, "worldbank_wdi_itu_un_morocco.csv")
  )
  log_event(
    "World Bank WDI / ITU / UN",
    "World Bank API",
    "https://api.worldbank.org/v2/",
    "downloaded",
    "National context only; not used as ADM2 model features."
  )
} else {
  log_event(
    "World Bank WDI / ITU / UN",
    "World Bank API",
    "https://api.worldbank.org/v2/",
    "failed",
    "National context download failed."
  )
}

# ---- 5. UN Population Division catalog -------------------------------------
un_base <- "https://population.un.org/dataportalapi/api/v1"
ok_indicators <- safe_download(
  paste0(un_base, "/indicators?format=csv&sort=name"),
  file.path(path_un, "un_indicators_catalog.csv"),
  quiet = TRUE
)
ok_locations <- safe_download(
  paste0(un_base, "/locations?format=csv&sort=name"),
  file.path(path_un, "un_locations_catalog.csv"),
  quiet = TRUE
)

log_event(
  "UN Population Data Portal catalog",
  "UN Population Division",
  paste0(un_base, "/indicators"),
  ifelse(ok_indicators && ok_locations, "downloaded", "partial"),
  "Catalogs retained for documentation; national context only."
)

# ---- 6. FAOSTAT land use: optional -----------------------------------------
if (isTRUE(download_fao)) {
  tryCatch(
    {
      fao_land <- FAOSTAT::get_faostat_bulk("RL")
      fao_morocco <- fao_land |>
        dplyr::filter(stringr::str_to_lower(Area) == "morocco")
      safe_write_csv(
        fao_morocco,
        file.path(path_fao, "faostat_land_use_morocco_raw.csv")
      )
      log_event(
        "FAOSTAT land use",
        "FAOSTAT",
        "https://www.fao.org/faostat/",
        "downloaded",
        "National context only."
      )
    },
    error = function(e) {
      log_event(
        "FAOSTAT land use",
        "FAOSTAT",
        "https://www.fao.org/faostat/",
        "failed",
        conditionMessage(e)
      )
    }
  )
} else {
  log_event(
    "FAOSTAT land use",
    "FAOSTAT",
    "https://www.fao.org/faostat/",
    "skipped",
    "download_fao = FALSE."
  )
}

# ---- 7. OpenStreetMap Geofabrik GeoPackage ---------------------------------
osm_url <- "https://download.geofabrik.de/africa/morocco-latest-free.gpkg.zip"
if (isTRUE(download_heavy_osm)) {
  osm_zip <- file.path(path_osm, "morocco-latest-free.gpkg.zip")
  ok <- safe_download(osm_url, osm_zip)

  if (ok && length(list.files(path_osm, pattern = "\\.gpkg$", recursive = TRUE)) == 0) {
    unzip(osm_zip, exdir = path_osm)
  }

  gpkg_files <- list.files(
    path_osm,
    pattern = "\\.gpkg$",
    recursive = TRUE,
    full.names = TRUE
  )

  final_status <- if (ok && length(gpkg_files) > 0) "downloaded" else "failed"
  log_event(
    "OpenStreetMap Geofabrik",
    "Geofabrik / OSM",
    osm_url,
    final_status,
    "Road and POI layers used as ADM2 technology/service-access proxies."
  )

  if (final_status == "failed") {
    stop(
      "OSM GeoPackage is required for the technology-gap domain but was not available.",
      call. = FALSE
    )
  }
} else {
  log_event(
    "OpenStreetMap Geofabrik",
    "Geofabrik / OSM",
    osm_url,
    "skipped",
    "Technology-gap modelling will stop unless a GeoPackage is supplied manually."
  )
}

message("Raw downloads complete.")