# =============================================================================
# 03_clean_qc_build_ml_dataset_revised.R
# Cleans and aligns the raw data, builds the ADM2 feature table, performs explicit
# quality control and creates an analytical environment-type classification.
#
# Critical safeguards:
#   * OSM failures are never hidden.
#   * A failed technology extraction stops the pipeline.
#   * Zero mapped facilities/roads are distinguished from missing extraction.
#   * Population density is retained for context/classification, not as a CTVI
#     technology-gap input.
# =============================================================================

# ---- 1. Administrative boundaries ------------------------------------------
pick_name_column <- function(x, candidates) {
  selected <- intersect(candidates, names(x))[1]
  if (is.na(selected)) {
    selected <- setdiff(names(x), attr(x, "sf_column"))[1]
  }
  selected
}

adm1 <- sf::st_read(adm_path("ADM1"), quiet = TRUE) |>
  sf::st_make_valid() |>
  sf::st_transform(4326) |>
  janitor::clean_names()

adm2 <- sf::st_read(adm_path("ADM2"), quiet = TRUE) |>
  sf::st_make_valid() |>
  sf::st_transform(4326) |>
  janitor::clean_names()

adm1_name_col <- pick_name_column(
  adm1,
  c("shape_name", "name", "region", "adm1_name")
)
adm2_name_col <- pick_name_column(
  adm2,
  c("shape_name", "name", "province", "prefecture", "adm2_name")
)
adm2_id_col <- intersect(c("shape_id", "shapeid"), names(adm2))[1]

adm1 <- adm1 |>
  dplyr::mutate(adm1_name = as.character(.data[[adm1_name_col]]))

adm2 <- adm2 |>
  dplyr::mutate(
    adm2_name = as.character(.data[[adm2_name_col]]),
    adm2_id = if (!is.na(adm2_id_col)) {
      as.character(.data[[adm2_id_col]])
    } else {
      as.character(dplyr::row_number())
    }
  )

# Attach each ADM2 area to its ADM1 parent using an interior point.
adm2_points <- sf::st_point_on_surface(adm2)
parent_lookup <- sf::st_join(
  adm2_points,
  adm1["adm1_name"],
  join = sf::st_within,
  left = TRUE
) |>
  sf::st_drop_geometry() |>
  dplyr::select(adm2_id, adm1_name) |>
  dplyr::distinct(adm2_id, .keep_all = TRUE)

adm2 <- adm2 |>
  dplyr::select(-dplyr::any_of("adm1_name")) |>
  dplyr::left_join(parent_lookup, by = "adm2_id")

# Area in km2 using an equal-area projection.
adm2_equal_area <- sf::st_transform(adm2, 6933)
adm2_area <- tibble::tibble(
  adm2_id = adm2_equal_area$adm2_id,
  area_km2 = as.numeric(sf::st_area(adm2_equal_area)) / 1e6
)

adm2 <- adm2 |>
  dplyr::left_join(adm2_area, by = "adm2_id")

adm2_out <- adm2 |>
  dplyr::select(adm2_id, adm2_name, adm1_name, area_km2)

sf::st_write(
  adm1,
  file.path(path_int_spatial, "morocco_adm1.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)
sf::st_write(
  adm2_out,
  file.path(path_int_spatial, "morocco_adm2.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)
sf::st_write(
  adm2_out,
  file.path(path_int_spatial, "morocco_adm2.shp"),
  delete_layer = TRUE,
  quiet = TRUE
)
safe_write_csv(
  sf::st_drop_geometry(adm2_out),
  file.path(path_int_spatial, "morocco_adm2_keys.csv")
)

# ---- 2. NASA climate: annual and recent-period summaries -------------------
nasa_file <- file.path(
  path_nasa,
  glue::glue("nasa_power_daily_morocco_adm2_{start_year}_{end_year}.csv")
)

safe_sum <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

if (file.exists(nasa_file)) {
  nasa <- readr::read_csv(nasa_file, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ dplyr::if_else(.x <= -900, NA_real_, as.numeric(.x))
      ),
      date = as.Date(date),
      year = lubridate::year(date)
    )

  required_nasa_columns <- c(
    "t2m", "t2m_max", "prectotcorr", "ws2m", "rh2m",
    "allsky_sfc_sw_dwn"
  )
  missing_nasa_columns <- setdiff(required_nasa_columns, names(nasa))
  if (length(missing_nasa_columns) > 0) {
    stop(
      "NASA file is missing required fields: ",
      paste(missing_nasa_columns, collapse = ", "),
      call. = FALSE
    )
  }

  climate_annual <- nasa |>
    dplyr::group_by(adm2_id, adm2_name, year) |>
    dplyr::summarise(
      temp_mean_c = mean(t2m, na.rm = TRUE),
      temp_max_mean_c = mean(t2m_max, na.rm = TRUE),
      precipitation_total_mm = safe_sum(prectotcorr),
      dry_days_lt_1mm = safe_sum(prectotcorr < 1),
      heat_days_gt_35c = safe_sum(t2m_max > 35),
      wind_mean_ms = mean(ws2m, na.rm = TRUE),
      humidity_mean_pct = mean(rh2m, na.rm = TRUE),
      solar_mean_kwh_m2_day = mean(allsky_sfc_sw_dwn, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ dplyr::na_if(.x, NaN))
    )

  most_recent_year <- max(climate_annual$year, na.rm = TRUE)
  first_recent_year <- most_recent_year - recent_climate_years + 1

  climate_recent <- climate_annual |>
    dplyr::filter(year >= first_recent_year) |>
    dplyr::group_by(adm2_id) |>
    dplyr::summarise(
      temp_mean_c_5yr = mean(temp_mean_c, na.rm = TRUE),
      temp_max_mean_c_5yr = mean(temp_max_mean_c, na.rm = TRUE),
      precipitation_total_mm_5yr = mean(precipitation_total_mm, na.rm = TRUE),
      dry_days_lt_1mm_5yr = mean(dry_days_lt_1mm, na.rm = TRUE),
      heat_days_gt_35c_5yr = mean(heat_days_gt_35c, na.rm = TRUE),
      wind_mean_ms_5yr = mean(wind_mean_ms, na.rm = TRUE),
      humidity_mean_pct_5yr = mean(humidity_mean_pct, na.rm = TRUE),
      solar_mean_kwh_m2_day_5yr = mean(solar_mean_kwh_m2_day, na.rm = TRUE),
      climate_period_start = first_recent_year,
      climate_period_end = most_recent_year,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ dplyr::na_if(.x, NaN))
    )

  safe_write_csv(
    climate_annual,
    file.path(path_int_climate, "morocco_adm2_climate_annual.csv")
  )
  safe_write_csv(
    climate_recent,
    file.path(path_int_climate, "morocco_adm2_climate_recent_5yr.csv")
  )
} else {
  stop(
    "Combined NASA climate file not found. Run 02_download_sources_revised.R first.",
    call. = FALSE
  )
}

# ---- 3. WorldPop zonal population and density ------------------------------
population_features <- adm2 |>
  sf::st_drop_geometry() |>
  dplyr::select(adm2_id, area_km2) |>
  dplyr::mutate(
    population = NA_real_,
    population_density_per_km2 = NA_real_
  )

worldpop_file <- list.files(
  path_worldpop,
  pattern = "\\.tif$",
  full.names = TRUE
)[1]

if (!is.na(worldpop_file) && file.exists(worldpop_file)) {
  population_features <- tryCatch(
    {
      population_raster <- terra::rast(worldpop_file)
      adm2_for_population <- sf::st_transform(adm2, sf::st_crs(population_raster))
      population_sum <- exactextractr::exact_extract(
        population_raster,
        adm2_for_population,
        "sum",
        progress = FALSE
      )

      adm2 |>
        sf::st_drop_geometry() |>
        dplyr::select(adm2_id, area_km2) |>
        dplyr::mutate(
          population = as.numeric(population_sum),
          population_density_per_km2 = population / area_km2
        )
    },
    error = function(e) {
      warning("WorldPop extraction failed: ", conditionMessage(e))
      population_features
    }
  )
} else {
  warning("WorldPop raster was not found; population priority outputs will be incomplete.")
}

safe_write_csv(
  population_features,
  file.path(path_int_socio, "morocco_adm2_population.csv")
)

# ---- 4. OSM technology/service-access features -----------------------------
osm_gpkg <- list.files(
  path_osm,
  pattern = "\\.gpkg$",
  recursive = TRUE,
  full.names = TRUE
)[1]

if (is.na(osm_gpkg) || !file.exists(osm_gpkg)) {
  stop(
    "No OSM GeoPackage was found. Technology-gap modelling cannot continue.",
    call. = FALSE
  )
}

osm_features <- tryCatch(
  {
    osm_layers <- sf::st_layers(osm_gpkg)$name

    road_layer <- osm_layers[
      stringr::str_detect(
        osm_layers,
        stringr::regex("road|line", ignore_case = TRUE)
      )
    ][1]
    poi_layer <- osm_layers[
      stringr::str_detect(
        osm_layers,
        stringr::regex("poi|point", ignore_case = TRUE)
      )
    ][1]

    if (is.na(road_layer)) {
      stop("No road/line layer could be identified in the OSM GeoPackage.")
    }
    if (is.na(poi_layer)) {
      stop("No POI/point layer could be identified in the OSM GeoPackage.")
    }

    message("OSM road layer: ", road_layer)
    message("OSM POI layer: ", poi_layer)

    adm2_equal <- adm2 |>
      sf::st_transform(6933) |>
      dplyr::select(adm2_id)
    area_lookup <- adm2 |>
      sf::st_drop_geometry() |>
      dplyr::select(adm2_id, area_km2)

    # Road density.
    roads <- sf::st_read(osm_gpkg, layer = road_layer, quiet = TRUE) |>
      sf::st_make_valid()

    if ("fclass" %in% names(roads)) {
      roads <- roads |>
        dplyr::filter(
          stringr::str_to_lower(as.character(fclass)) %in% osm_major_road_classes
        )
    }

    if (nrow(roads) == 0) stop("OSM road layer contains no usable road features.")

    roads <- sf::st_transform(roads, 6933)
    road_intersections <- suppressWarnings(
      sf::st_intersection(roads, adm2_equal)
    )
    road_intersections$length_km <- as.numeric(
      sf::st_length(road_intersections)
    ) / 1000

    road_density <- road_intersections |>
      sf::st_drop_geometry() |>
      dplyr::group_by(adm2_id) |>
      dplyr::summarise(
        road_km = sum(length_km, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::right_join(area_lookup, by = "adm2_id") |>
      dplyr::mutate(
        road_km = tidyr::replace_na(road_km, 0),
        osm_road_km_per_100km2 = road_km / area_km2 * 100
      ) |>
      dplyr::select(adm2_id, osm_road_km_per_100km2)

    # POI and health/education service density.
    pois <- sf::st_read(osm_gpkg, layer = poi_layer, quiet = TRUE) |>
      sf::st_make_valid() |>
      sf::st_transform(6933)

    if (nrow(pois) == 0) stop("OSM POI layer contains no usable point features.")

    pois_joined <- sf::st_join(
      pois,
      adm2_equal,
      join = sf::st_within,
      left = FALSE
    )

    poi_density <- pois_joined |>
      sf::st_drop_geometry() |>
      dplyr::mutate(
        poi_class = dplyr::coalesce(
          if ("fclass" %in% names(pois_joined)) {
            as.character(fclass)
          } else {
            NA_character_
          },
          if ("amenity" %in% names(pois_joined)) {
            as.character(amenity)
          } else {
            NA_character_
          },
          ""
        ),
        poi_class = stringr::str_to_lower(poi_class),
        health_education_poi = stringr::str_detect(
          poi_class,
          "hospital|clinic|doctor|pharmacy|school|university|college"
        )
      ) |>
      dplyr::group_by(adm2_id) |>
      dplyr::summarise(
        poi_count = dplyr::n(),
        health_education_poi_count = sum(health_education_poi, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::right_join(area_lookup, by = "adm2_id") |>
      dplyr::mutate(
        poi_count = tidyr::replace_na(poi_count, 0L),
        health_education_poi_count = tidyr::replace_na(
          health_education_poi_count,
          0L
        ),
        osm_poi_count_per_100km2 = poi_count / area_km2 * 100,
        osm_health_education_poi_per_100km2 =
          health_education_poi_count / area_km2 * 100
      ) |>
      dplyr::select(
        adm2_id,
        osm_poi_count_per_100km2,
        osm_health_education_poi_per_100km2
      )

    adm2 |>
      sf::st_drop_geometry() |>
      dplyr::select(adm2_id) |>
      dplyr::left_join(road_density, by = "adm2_id") |>
      dplyr::left_join(poi_density, by = "adm2_id")
  },
  error = function(e) {
    log_event(
      "OSM technology features",
      "Geofabrik / OSM",
      osm_gpkg,
      "failed",
      conditionMessage(e)
    )
    stop(
      "OSM feature extraction failed: ",
      conditionMessage(e),
      call. = FALSE
    )
  }
)

safe_write_csv(
  osm_features,
  file.path(path_int_socio, "morocco_adm2_osm_access_features.csv")
)

osm_qc <- feature_qc_table(
  osm_features,
  preferred_technology_features,
  "technology"
)
safe_write_csv(osm_qc, file.path(path_logs, "osm_technology_feature_qc.csv"))
assert_minimum_features(
  osm_qc,
  "technology",
  min_valid_technology_features
)

log_event(
  "OSM technology features",
  "Geofabrik / OSM",
  osm_gpkg,
  "processed",
  paste0(
    sum(osm_qc$usable),
    " usable technology/service-access indicators produced."
  )
)

# ---- 5. National context ----------------------------------------------------
wdi_file <- file.path(path_wdi, "worldbank_wdi_itu_un_morocco.csv")
if (file.exists(wdi_file)) {
  wdi <- readr::read_csv(wdi_file, show_col_types = FALSE) |>
    janitor::clean_names()

  indicator_columns <- intersect(
    janitor::make_clean_names(names(wdi_indicators)),
    names(wdi)
  )

  if (length(indicator_columns) > 0) {
    wdi_latest <- wdi |>
      tidyr::pivot_longer(
        dplyr::all_of(indicator_columns),
        names_to = "indicator",
        values_to = "value"
      ) |>
      dplyr::filter(!is.na(value)) |>
      dplyr::group_by(indicator) |>
      dplyr::slice_max(year, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(indicator, year, value)

    safe_write_csv(
      wdi_latest,
      file.path(path_int_socio, "morocco_national_context_latest.csv")
    )
  }
}

# ---- 6. Feasibility and role matrix ----------------------------------------
feasibility <- tibble::tribble(
  ~variable, ~source, ~native_resolution, ~adm2_usable, ~role, ~limitation_or_proxy,
  "heat, dry days and precipitation", "NASA POWER", "ADM2 representative point", "yes", "climate exposure", "One representative point per province/prefecture; large areas may be under-sampled",
  "population and population density", "WorldPop", "1 km raster", "yes", "context, environment classification and priority", "Not included in the technology-gap sub-index",
  "road density", "OpenStreetMap", "vector", "yes", "technology/service access", "Completeness varies geographically",
  "POI and health/education POI density", "OpenStreetMap", "vector", "yes", "technology/service access", "Proxy for service availability rather than direct digital connectivity",
  "internet, mobile and broadband", "World Bank/ITU", "national", "no", "national context", "National constants cannot distinguish ADM2 areas",
  "electricity access", "World Bank", "national", "no", "national context", "Subnational HCP/ANRT data should replace proxies when obtained"
)
safe_write_csv(feasibility, file.path(path_logs, "feasibility_matrix.csv"))

# ---- 7. Assemble raw ADM2 feature table ------------------------------------
keys <- adm2 |>
  sf::st_drop_geometry() |>
  dplyr::select(adm2_id, adm2_name, adm1_name, area_km2)

ml_raw <- keys |>
  dplyr::left_join(climate_recent, by = "adm2_id") |>
  dplyr::left_join(
    dplyr::select(population_features, -area_km2),
    by = "adm2_id"
  ) |>
  dplyr::left_join(osm_features, by = "adm2_id")

# ---- 8. Analytical environment type ----------------------------------------
# This creates a reproducible first-pass classification for comparison. It must
# be reviewed against official settlement/urban-boundary data before publication.
density_for_classification <- ml_raw$population_density_per_km2
if (all(!is.finite(density_for_classification))) {
  stop(
    "Population density is unavailable; environment types cannot be classified.",
    call. = FALSE
  )
}
density_for_classification[!is.finite(density_for_classification)] <- stats::median(
  density_for_classification,
  na.rm = TRUE
)

smart_density_cut <- as.numeric(stats::quantile(
  density_for_classification,
  smart_city_density_quantile,
  na.rm = TRUE,
  type = 8
))
rural_density_cut <- as.numeric(stats::quantile(
  density_for_classification,
  rural_density_quantile,
  na.rm = TRUE,
  type = 8
))

ml_raw <- ml_raw |>
  dplyr::mutate(
    population_density_for_classification = density_for_classification,
    smart_city_name_anchor = pattern_match_any(
      adm2_name,
      smart_city_name_patterns
    ),
    urban_edge_name_anchor = pattern_match_any(
      adm2_name,
      urban_edge_name_patterns
    ),
    environment_type_auto = dplyr::case_when(
      smart_city_name_anchor ~ "Smart city / urban core",
      urban_edge_name_anchor ~ "Urban edge / peri-urban",
      population_density_for_classification >= smart_density_cut ~
        "Smart city / urban core",
      population_density_for_classification <= rural_density_cut ~
        "Rural / lower-density",
      TRUE ~ "Urban edge / peri-urban"
    ),
    environment_classification_basis_auto = dplyr::case_when(
      smart_city_name_anchor ~ "Named urban-core anchor",
      urban_edge_name_anchor ~ "Named peri-urban anchor",
      population_density_for_classification >= smart_density_cut ~
        glue::glue("Population density >= {smart_city_density_quantile * 100}th percentile"),
      population_density_for_classification <= rural_density_cut ~
        glue::glue("Population density <= {rural_density_quantile * 100}th percentile"),
      TRUE ~ "Intermediate population-density band"
    ),
    adm2_name_key = normalise_text(adm2_name)
  )

overrides <- readr::read_csv(
  environment_override_file,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) |>
  janitor::clean_names() |>
  dplyr::mutate(
    adm2_id = dplyr::na_if(stringr::str_squish(adm2_id), ""),
    adm2_name = dplyr::na_if(stringr::str_squish(adm2_name), ""),
    environment_type = dplyr::na_if(
      stringr::str_squish(environment_type),
      ""
    ),
    adm2_name_key = normalise_text(adm2_name)
  ) |>
  dplyr::filter(!is.na(environment_type))

invalid_override_labels <- setdiff(
  unique(overrides$environment_type),
  allowed_environment_types
)
if (length(invalid_override_labels) > 0) {
  stop(
    "Invalid environment type(s) in override file: ",
    paste(invalid_override_labels, collapse = ", "),
    ". Allowed values are: ",
    paste(allowed_environment_types, collapse = "; "),
    call. = FALSE
  )
}

overrides_by_id <- overrides |>
  dplyr::filter(!is.na(adm2_id)) |>
  dplyr::select(
    adm2_id,
    environment_type_by_id = environment_type,
    override_notes_by_id = notes
  ) |>
  dplyr::distinct(adm2_id, .keep_all = TRUE)

overrides_by_name <- overrides |>
  dplyr::filter(!is.na(adm2_name_key)) |>
  dplyr::select(
    adm2_name_key,
    environment_type_by_name = environment_type,
    override_notes_by_name = notes
  ) |>
  dplyr::distinct(adm2_name_key, .keep_all = TRUE)

ml_raw <- ml_raw |>
  dplyr::left_join(overrides_by_id, by = "adm2_id") |>
  dplyr::left_join(overrides_by_name, by = "adm2_name_key") |>
  dplyr::mutate(
    environment_type = dplyr::coalesce(
      environment_type_by_id,
      environment_type_by_name,
      environment_type_auto
    ),
    environment_classification_basis = dplyr::case_when(
      !is.na(environment_type_by_id) ~ "Manual override by ADM2 ID",
      !is.na(environment_type_by_name) ~ "Manual override by ADM2 name",
      TRUE ~ environment_classification_basis_auto
    ),
    environment_classification_notes = dplyr::coalesce(
      override_notes_by_id,
      override_notes_by_name,
      "Auto-generated classification; review before publication"
    ),
    environment_type = factor(
      environment_type,
      levels = allowed_environment_types
    )
  ) |>
  dplyr::select(
    -adm2_name_key,
    -environment_type_by_id,
    -environment_type_by_name,
    -override_notes_by_id,
    -override_notes_by_name
  )

environment_counts <- ml_raw |>
  dplyr::count(environment_type, name = "n")
safe_write_csv(
  environment_counts,
  file.path(path_logs, "environment_type_counts.csv")
)

if (any(environment_counts$n < minimum_environment_group_n)) {
  warning(
    "At least one environment group contains fewer than ",
    minimum_environment_group_n,
    " ADM2 areas. Review environment_type_overrides.csv."
  )
}

safe_write_csv(
  ml_raw |>
    dplyr::select(
      adm2_id,
      adm2_name,
      adm1_name,
      population,
      population_density_per_km2,
      environment_type,
      environment_classification_basis,
      environment_classification_notes
    ),
  file.path(path_tables, "environment_type_classification_review.csv")
)

# ---- 9. Missingness, imputation and feature QC -----------------------------
safe_write_csv(
  ml_raw,
  file.path(path_ml, "morocco_ctvi_adm2_features_raw.csv")
)

missingness_report <- tibble::tibble(
  variable = names(ml_raw),
  missing_n = purrr::map_int(ml_raw, ~ sum(is.na(.x))),
  missing_pct = round(purrr::map_dbl(ml_raw, ~ mean(is.na(.x)) * 100), 1),
  unique_n = purrr::map_int(
    ml_raw,
    ~ dplyr::n_distinct(.x[!is.na(.x)])
  )
)
safe_write_csv(
  missingness_report,
  file.path(path_logs, "missingness_adm2_features.csv")
)

ml_imputed <- ml_raw
imputable_variables <- intersect(
  c(
    names(indicator_direction),
    "population",
    "population_density_per_km2"
  ),
  names(ml_imputed)
)

imputation_rows <- list()
for (variable in imputable_variables) {
  values <- ml_imputed[[variable]]
  missing_before <- sum(!is.finite(values))

  if (missing_before > 0 && !all(!is.finite(values))) {
    imputation_value <- stats::median(values[is.finite(values)], na.rm = TRUE)
    values[!is.finite(values)] <- imputation_value
    ml_imputed[[variable]] <- values
  } else {
    imputation_value <- NA_real_
  }

  imputation_rows[[variable]] <- tibble::tibble(
    variable = variable,
    missing_before = missing_before,
    imputation_method = ifelse(
      missing_before > 0,
      "Median imputation",
      "No imputation"
    ),
    imputation_value = imputation_value
  )
}
imputation_summary <- dplyr::bind_rows(imputation_rows)

safe_write_csv(
  imputation_summary,
  file.path(path_logs, "imputation_summary.csv")
)

climate_qc <- feature_qc_table(
  ml_imputed,
  unique(c(preferred_climate_features, climate_fallback_features)),
  "climate"
)
technology_qc <- feature_qc_table(
  ml_imputed,
  preferred_technology_features,
  "technology"
)
feature_qc <- dplyr::bind_rows(climate_qc, technology_qc)

safe_write_csv(
  feature_qc,
  file.path(path_logs, "model_feature_qc.csv")
)
assert_minimum_features(
  climate_qc,
  "climate",
  min_valid_climate_features
)
assert_minimum_features(
  technology_qc,
  "technology",
  min_valid_technology_features
)

safe_write_csv(
  ml_imputed,
  file.path(path_ml, "morocco_ctvi_adm2_features_imputed.csv")
)

message(
  "ADM2 feature build complete: ",
  nrow(ml_imputed),
  " areas; environment groups = ",
  paste(environment_counts$environment_type, environment_counts$n, sep = ": ", collapse = ", ")
)
