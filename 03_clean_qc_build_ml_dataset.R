# =============================================================================
# 03_clean_qc_build_ml_dataset.R
# Standardises formats, aligns CRS, builds the ADM2 (~75 provinces) feature
# table, runs a raster zonal step (WorldPop), checks missingness, imputes.
# =============================================================================

# ---- 1. Boundaries: load, clean, key, parent-region, area ------------------
adm1 <- sf::st_read(adm_path("ADM1"), quiet = TRUE) |>
  sf::st_make_valid() |> sf::st_transform(4326) |> janitor::clean_names()
adm2 <- sf::st_read(adm_path("ADM2"), quiet = TRUE) |>
  sf::st_make_valid() |> sf::st_transform(4326) |> janitor::clean_names()

pick_name <- function(x, cands) {
  nm <- intersect(cands, names(x))[1]
  if (is.na(nm)) nm <- setdiff(names(x), attr(x, "sf_column"))[1]
  nm
}
n1 <- pick_name(adm1, c("shape_name", "name", "region", "adm1_name"))
n2 <- pick_name(adm2, c("shape_name", "name", "province", "adm2_name"))
i2 <- intersect(c("shape_id", "shapeid"), names(adm2))[1]

adm1 <- adm1 |> dplyr::mutate(adm1_name = as.character(.data[[n1]]))
adm2 <- adm2 |>
  dplyr::mutate(
    adm2_name = as.character(.data[[n2]]),
    adm2_id   = if (!is.na(i2)) as.character(.data[[i2]]) else as.character(dplyr::row_number())
  )

# Tag each ADM2 to its parent ADM1 by a representative interior point.
adm2_pts <- adm2 |> sf::st_point_on_surface()
parent   <- sf::st_join(adm2_pts, adm1["adm1_name"], join = sf::st_within) |>
  sf::st_drop_geometry() |> dplyr::select(adm2_id, adm1_name)
adm2 <- adm2 |> dplyr::left_join(parent, by = "adm2_id")

# Area in km2 via an equal-area projection (EPSG:6933).
# Compute on the sf object (not a named geometry column) so it works whether
# the geometry is called "geometry" or "geom".
adm2_proj <- sf::st_transform(adm2, 6933)
adm2_proj$area_km2 <- as.numeric(sf::st_area(adm2_proj)) / 1e6
adm2_area <- sf::st_drop_geometry(adm2_proj)[, c("adm2_id", "area_km2")]
adm2 <- adm2 |> dplyr::left_join(adm2_area, by = "adm2_id")

# Standardised GIS exports (GPKG + SHP).
adm2_out <- adm2 |> dplyr::select(adm2_id, adm2_name, adm1_name, area_km2)
sf::st_write(adm1, file.path(path_int_spatial, "morocco_adm1.gpkg"), delete_dsn = TRUE, quiet = TRUE)
sf::st_write(adm2_out, file.path(path_int_spatial, "morocco_adm2.gpkg"), delete_dsn = TRUE, quiet = TRUE)
sf::st_write(adm2_out, file.path(path_int_spatial, "morocco_adm2.shp"), delete_layer = TRUE, quiet = TRUE)
safe_write_csv(sf::st_drop_geometry(adm2_out), file.path(path_int_spatial, "morocco_adm2_keys.csv"))

# ---- 2. NASA climate -> annual -> recent 5-yr summaries (per ADM2) ---------
nasa_file <- file.path(path_nasa, glue::glue("nasa_power_daily_morocco_adm2_{start_year}_{end_year}.csv"))
if (file.exists(nasa_file)) {
  nasa <- readr::read_csv(nasa_file, show_col_types = FALSE) |>
    janitor::clean_names() |> dplyr::mutate(year = lubridate::year(date))

  climate_annual <- nasa |>
    dplyr::group_by(adm2_id, adm2_name, year) |>
    dplyr::summarise(
      temp_mean_c            = mean(t2m, na.rm = TRUE),
      temp_max_mean_c        = mean(t2m_max, na.rm = TRUE),
      precipitation_total_mm = sum(prectotcorr, na.rm = TRUE),
      dry_days_lt_1mm        = sum(prectotcorr < 1, na.rm = TRUE),
      heat_days_gt_35c       = sum(t2m_max > 35, na.rm = TRUE),
      wind_mean_ms           = mean(ws2m, na.rm = TRUE),
      humidity_mean_pct      = mean(rh2m, na.rm = TRUE),
      solar_mean_kwh_m2_day  = mean(allsky_sfc_sw_dwn, na.rm = TRUE),
      .groups = "drop")

  climate_recent <- climate_annual |>
    dplyr::filter(year >= max(year, na.rm = TRUE) - 4) |>
    dplyr::group_by(adm2_id) |>
    dplyr::summarise(
      temp_mean_c_5yr            = mean(temp_mean_c, na.rm = TRUE),
      temp_max_mean_c_5yr        = mean(temp_max_mean_c, na.rm = TRUE),
      precipitation_total_mm_5yr = mean(precipitation_total_mm, na.rm = TRUE),
      dry_days_lt_1mm_5yr        = mean(dry_days_lt_1mm, na.rm = TRUE),
      heat_days_gt_35c_5yr       = mean(heat_days_gt_35c, na.rm = TRUE),
      wind_mean_ms_5yr           = mean(wind_mean_ms, na.rm = TRUE),
      solar_mean_kwh_m2_day_5yr  = mean(solar_mean_kwh_m2_day, na.rm = TRUE),
      .groups = "drop")

  safe_write_csv(climate_annual, file.path(path_int_climate, "morocco_adm2_climate_annual.csv"))
  safe_write_csv(climate_recent, file.path(path_int_climate, "morocco_adm2_climate_recent_5yr.csv"))
} else {
  climate_recent <- adm2 |> sf::st_drop_geometry() |> dplyr::select(adm2_id)
  message("NASA file missing; climate features will be NA.")
}

# ---- 3. WorldPop raster -> zonal population & density (per ADM2) ------------
pop_features <- adm2 |> sf::st_drop_geometry() |> dplyr::select(adm2_id, area_km2) |>
  dplyr::mutate(population = NA_real_, population_density_per_km2 = NA_real_)
wp_file <- list.files(path_worldpop, pattern = "\\.tif$", full.names = TRUE)[1]
if (!is.na(wp_file) && file.exists(wp_file)) {
  try({
    pop_rast <- terra::rast(wp_file)
    adm2_wgs <- sf::st_transform(adm2, sf::st_crs(pop_rast))
    pop_sum  <- exactextractr::exact_extract(pop_rast, adm2_wgs, "sum", progress = FALSE)
    pop_features <- adm2 |> sf::st_drop_geometry() |>
      dplyr::select(adm2_id, area_km2) |>
      dplyr::mutate(population = as.numeric(pop_sum),
                    population_density_per_km2 = population / area_km2)
  }, silent = TRUE)
}
safe_write_csv(pop_features, file.path(path_int_socio, "morocco_adm2_population.csv"))

# ---- 4. OSM technology-access proxies (per ADM2) ---------------------------
empty_osm <- function() adm2 |> sf::st_drop_geometry() |> dplyr::select(adm2_id) |>
  dplyr::mutate(osm_road_km_per_100km2 = NA_real_,
                osm_poi_count_per_100km2 = NA_real_,
                osm_health_education_poi_per_100km2 = NA_real_)
osm_features <- empty_osm()

try({
  gpkg <- list.files(path_osm, pattern = "\\.gpkg$", recursive = TRUE, full.names = TRUE)[1]
  if (!is.na(gpkg) && file.exists(gpkg)) {
    layers <- sf::st_layers(gpkg)$name
    roads_layer <- layers[stringr::str_detect(layers, stringr::regex("road|line", ignore_case = TRUE))][1]
    pois_layer  <- layers[stringr::str_detect(layers, stringr::regex("poi|point", ignore_case = TRUE))][1]

    adm2_eq <- adm2 |> sf::st_transform(6933) |> dplyr::select(adm2_id)
    area_tbl <- adm2 |> sf::st_drop_geometry() |> dplyr::select(adm2_id, area_km2)

    road_density <- NULL
    if (!is.na(roads_layer)) {
      roads <- sf::st_read(gpkg, layer = roads_layer, quiet = TRUE)
      if ("fclass" %in% names(roads))
        roads <- roads |> dplyr::filter(stringr::str_to_lower(fclass) %in% osm_major_road_classes)
      roads <- sf::st_transform(sf::st_make_valid(roads), 6933)
      ri <- sf::st_intersection(roads, adm2_eq)
      ri$len_km <- as.numeric(sf::st_length(ri)) / 1000
      road_density <- ri |>
        sf::st_drop_geometry() |>
        dplyr::group_by(adm2_id) |>
        dplyr::summarise(road_km = sum(len_km, na.rm = TRUE), .groups = "drop") |>
        dplyr::left_join(area_tbl, by = "adm2_id") |>
        dplyr::mutate(osm_road_km_per_100km2 = road_km / area_km2 * 100) |>
        dplyr::select(adm2_id, osm_road_km_per_100km2)
    }

    poi_density <- NULL
    if (!is.na(pois_layer)) {
      pois <- sf::st_read(gpkg, layer = pois_layer, quiet = TRUE) |> sf::st_transform(6933)
      poi_density <- sf::st_join(pois, adm2_eq) |> sf::st_drop_geometry() |>
        dplyr::filter(!is.na(adm2_id)) |>
        dplyr::mutate(
          cls = stringr::str_to_lower(dplyr::coalesce(
            if ("fclass"  %in% names(pois)) as.character(fclass)  else NA_character_,
            if ("amenity" %in% names(pois)) as.character(amenity) else NA_character_)),
          health_edu = stringr::str_detect(cls %||% "",
            "hospital|clinic|doctors|pharmacy|school|university|college")) |>
        dplyr::group_by(adm2_id) |>
        dplyr::summarise(poi_count = dplyr::n(),
                         health_edu_poi = sum(health_edu, na.rm = TRUE), .groups = "drop") |>
        dplyr::left_join(area_tbl, by = "adm2_id") |>
        dplyr::mutate(osm_poi_count_per_100km2 = poi_count / area_km2 * 100,
                      osm_health_education_poi_per_100km2 = health_edu_poi / area_km2 * 100) |>
        dplyr::select(adm2_id, osm_poi_count_per_100km2, osm_health_education_poi_per_100km2)
    }

    osm_features <- adm2 |> sf::st_drop_geometry() |> dplyr::select(adm2_id) |>
      dplyr::left_join(road_density, by = "adm2_id") |>
      dplyr::left_join(poi_density,  by = "adm2_id")
  }
}, silent = TRUE)
safe_write_csv(osm_features, file.path(path_int_socio, "morocco_adm2_osm_access_features.csv"))

# ---- 5. National context (WDI/ITU/UN) cleaned + FEASIBILITY note ------------
wdi_file <- file.path(path_wdi, "worldbank_wdi_itu_un_morocco.csv")
if (file.exists(wdi_file)) {
  wdi <- readr::read_csv(wdi_file, show_col_types = FALSE) |> janitor::clean_names()
  ind_cols <- intersect(janitor::make_clean_names(names(wdi_indicators)), names(wdi))
  wdi_latest <- wdi |>
    tidyr::pivot_longer(dplyr::all_of(ind_cols), names_to = "indicator", values_to = "value") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::group_by(indicator) |> dplyr::slice_max(year, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |> dplyr::select(indicator, year, value)
  safe_write_csv(wdi_latest, file.path(path_int_socio, "morocco_national_context_latest.csv"))
}

# Auto-written feasibility note (Task 2 documentation): which variables are
# spatially usable at ADM2 vs national-only context, and the proxy choices.
feasibility <- tibble::tribble(
  ~variable,                              ~source,            ~native_resolution, ~adm2_usable, ~role,                         ~limitation_or_proxy,
  "temp/heat/precip/dryness (5yr)",       "NASA POWER",       "point @ centroid", "yes",        "climate exposure",            "1 centroid per province; large desert provinces under-sampled",
  "population, population density",       "WorldPop 1km",     "1 km raster",      "yes",        "exposure magnitude / context","UN-adjusted top-down estimate",
  "road density, POI density, health/edu","OpenStreetMap",    "vector",           "yes",        "technology/services access",  "OSM completeness varies; proxy for service access",
  "internet/mobile/broadband (ITU)",      "World Bank/ITU",   "national",         "no",         "national context only",       "national constant; cannot discriminate provinces -> OSM density used as spatial proxy",
  "electricity access (urban/rural)",     "World Bank",       "national",         "no",         "national context only",       "national constant; future proxy: downscale by HCP urban/rural pop share",
  "land use, GDP/capita, poverty",        "FAOSTAT/WB",       "national",         "no",         "national context only",       "national constant; documented, not modelled at ADM2"
)
safe_write_csv(feasibility, file.path(path_logs, "feasibility_matrix.csv"))

# ---- 6. Assemble the ADM2 ML-ready feature table ---------------------------
keys <- adm2 |> sf::st_drop_geometry() |> dplyr::select(adm2_id, adm2_name, adm1_name, area_km2)
ml_adm2 <- keys |>
  dplyr::left_join(climate_recent, by = "adm2_id") |>
  dplyr::left_join(dplyr::select(pop_features, -area_km2), by = "adm2_id") |>
  dplyr::left_join(osm_features, by = "adm2_id")
safe_write_csv(ml_adm2, file.path(path_ml, "morocco_ctvi_adm2_features_raw.csv"))

# ---- 7. Missingness report -------------------------------------------------
miss_report <- tibble::tibble(
  variable    = names(ml_adm2),
  missing_n   = purrr::map_int(ml_adm2, ~sum(is.na(.x))),
  missing_pct = round(purrr::map_dbl(ml_adm2, ~mean(is.na(.x)) * 100), 1))
safe_write_csv(miss_report, file.path(path_logs, "missingness_adm2_features.csv"))

# ---- 8. Median imputation for modelling (keep raw copy untouched) ----------
num_cols <- ml_adm2 |> dplyr::select(where(is.numeric)) |> names()
ml_imputed <- ml_adm2
for (col in num_cols) {
  v <- ml_imputed[[col]]
  if (any(is.na(v)) && !all(is.na(v))) v[is.na(v)] <- stats::median(v, na.rm = TRUE)
  ml_imputed[[col]] <- v
}
# drop columns that are entirely NA or constant (useless for ML)
keep <- num_cols[purrr::map_lgl(num_cols, ~ {
  v <- ml_imputed[[.x]]; !all(is.na(v)) && length(unique(stats::na.omit(v))) > 1 })]
ml_imputed <- ml_imputed |> dplyr::select(adm2_id, adm2_name, adm1_name, dplyr::all_of(keep))
safe_write_csv(ml_imputed, file.path(path_ml, "morocco_ctvi_adm2_features_imputed.csv"))

message("ADM2 feature build complete: ", nrow(ml_adm2), " provinces, ",
        length(keep), " modelling features.")
