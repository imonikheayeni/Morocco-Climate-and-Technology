# =============================================================================
# 01_setup.R
# Climate-Technology Vulnerability Mapping in Morocco
# Central configuration, folder structure, packages and shared helpers.
#
# Design decisions (reported in the manuscript):
#   * The equal-weight CTVI is the primary index; PCA, robust scaling and
#     log-transformed access densities are sensitivity analyses.
#   * Population density is contextual. It is NOT a technology-gap indicator.
#   * Four median-split climate-technology typologies are the primary typology.
#   * Urban-core, peri-urban and rural/lower-density groups are analytical
#     classes, with a geographic (contiguity-based) definition as a check.
# =============================================================================

# ---- Project root -----------------------------------------------------------
if (!exists("root_dir") || !dir.exists(root_dir)) {
  root_dir <- getwd()
}
root_dir <- normalizePath(root_dir, winslash = "/", mustWork = FALSE)
message("Project root folder: ", root_dir)

# ---- Country and study-period settings -------------------------------------
country_iso3 <- "MAR"
country_iso2 <- "MA"
country_name <- "Morocco"

start_year <- 2014
end_year <- 2024
worldpop_year <- 2020
recent_climate_years <- 5

# WorldPop distributes the area south of 27 deg 40' N as a separate country
# raster (ISO code ESH). Both rasters are required so that all 75 ADM2 units
# in the boundary file receive a population estimate. Using MAR alone gave
# zero or near-zero totals for the five southernmost units.
worldpop_iso3 <- c("MAR", "ESH")

# ---- Analytical settings ---------------------------------------------------
random_seed <- 123
min_valid_climate_features <- 3L
min_valid_technology_features <- 2L
spatial_alpha <- 0.05
spatial_alpha_exploratory <- 0.10
map_quantile_classes <- 5L

# Primary typology uses median splits of climate exposure and technology gap.
# Units at or above the median are classed as "high".
typology_threshold_method <- "median"

# Environment classification (analytical aid, not an official classification).
smart_city_density_quantile <- 0.85
rural_density_quantile <- 0.40
minimum_environment_group_n <- 3L

allowed_environment_types <- c(
  "Smart city / urban core",
  "Urban edge / peri-urban",
  "Rural / lower-density"
)

# Short labels used in the manuscript and figures.
environment_short_labels <- c(
  "Smart city / urban core" = "Urban core",
  "Urban edge / peri-urban" = "Peri-urban",
  "Rural / lower-density" = "Rural/lower-density"
)

# Name anchors are matched as WHOLE WORDS against normalised ADM2 names.
# Whole-word matching prevents "sale" (Sale) from matching "Saleh"
# (Fquih Ben Saleh), which a substring match would do.
smart_city_name_patterns <- c(
  "casablanca", "rabat", "sale", "marrakech",
  "fes", "meknes", "tanger", "tangier", "tetouan"
)

urban_edge_name_patterns <- c(
  "mediouna", "nouaceur", "al haouz", "fahs anjra",
  "moulay yacoub", "skhirate temara", "kenitra"
)

# Sensitivity settings.
knn_sensitivity_k <- 4L
lisa_permutations <- 999L
population_flag_threshold <- 1000  # totals below this are flagged

# ---- Canonical folder scheme -----------------------------------------------
path_raw <- file.path(root_dir, "01_raw")
path_admin <- file.path(path_raw, "boundaries")
path_nasa <- file.path(path_raw, "nasa_power")
path_worldpop <- file.path(path_raw, "worldpop")
path_wdi <- file.path(path_raw, "world_bank")
path_un <- file.path(path_raw, "un")
path_fao <- file.path(path_raw, "fao")
path_osm <- file.path(path_raw, "osm")
path_hcp <- file.path(path_raw, "hcp")

path_intermediate <- file.path(root_dir, "02_intermediate")
path_int_spatial <- file.path(path_intermediate, "spatial")
path_int_climate <- file.path(path_intermediate, "climate")
path_int_socio <- file.path(path_intermediate, "socio_tech")

path_processed <- file.path(root_dir, "03_processed")
path_ml <- file.path(path_processed, "ml_ready")

path_maps <- file.path(root_dir, "04_maps")
path_logs <- file.path(root_dir, "05_logs")
path_docs <- file.path(root_dir, "00_documentation")
path_tables <- file.path(root_dir, "06_tables")
path_manuscript <- file.path(root_dir, "07_manuscript")

all_dirs <- c(
  path_raw, path_admin, path_nasa, path_worldpop, path_wdi, path_un,
  path_fao, path_osm, path_hcp, path_intermediate, path_int_spatial,
  path_int_climate, path_int_socio, path_processed, path_ml,
  path_maps, path_logs, path_docs, path_tables, path_manuscript
)
for (d in all_dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---- Packages ---------------------------------------------------------------
required_pkgs <- c(
  "tidyverse", "sf", "terra", "exactextractr",
  "jsonlite", "httr2", "curl", "lubridate", "glue",
  "janitor", "readxl", "scales", "stringi",
  "WDI",
  "cluster", "randomForest", "spdep",
  "viridisLite", "openxlsx", "patchwork"
)
# FAOSTAT is optional (national context only) and loaded in 02 if requested.

install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, dependencies = TRUE)
  }
}
install_if_missing(required_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ---- NASA POWER parameters --------------------------------------------------
nasa_params <- c(
  "T2M", "T2M_MAX", "T2M_MIN", "PRECTOTCORR", "WS2M", "RH2M",
  "ALLSKY_SFC_SW_DWN"
)

# ---- World Bank / ITU / UN national context indicators ---------------------
# National context only. National constants never discriminate ADM2 areas.
wdi_indicators <- c(
  internet_users_pct = "IT.NET.USER.ZS",
  mobile_subscriptions_per100 = "IT.CEL.SETS.P2",
  fixed_broadband_per100 = "IT.NET.BBND.P2",
  electricity_access_pct = "EG.ELC.ACCS.ZS",
  rural_electricity_access_pct = "EG.ELC.ACCS.RU.ZS",
  urban_electricity_access_pct = "EG.ELC.ACCS.UR.ZS",
  population_total = "SP.POP.TOTL",
  urban_population_pct = "SP.URB.TOTL.IN.ZS",
  agricultural_land_pct = "AG.LND.AGRI.ZS",
  cereal_yield_kg_per_ha = "AG.YLD.CREL.KG",
  gdp_per_capita_constant_usd = "NY.GDP.PCAP.KD",
  national_poverty_headcount_pct = "SI.POV.NAHC"
)

# ---- Download toggles -------------------------------------------------------
download_heavy_osm <- TRUE
download_worldpop <- TRUE
download_fao <- FALSE

# ---- OpenStreetMap settings -------------------------------------------------
# Road classes counted in the road-network density (Geofabrik "fclass").
# This is the mapped road network from motorways down to residential streets,
# not major roads only.
osm_road_classes <- c(
  "motorway", "trunk", "primary", "secondary",
  "tertiary", "unclassified", "residential"
)

# Geofabrik GeoPackage layers. The POI indicator uses the POINT layer
# (gis_osm_pois_free*), not the polygon layer (gis_osm_pois_a_free*).
osm_road_layer_pattern <- "^gis_osm_roads(_free)?(_1)?$"
osm_poi_point_layer_pattern <- "^gis_osm_pois(_free)?(_1)?$"

osm_health_education_regex <-
  "hospital|clinic|doctor|pharmacy|school|university|college"

# ---- Indicator definitions -------------------------------------------------
# Solar radiation is excluded because it may indicate renewable-energy
# potential as well as climatic exposure.
preferred_climate_features <- c(
  "heat_days_gt_35c_5yr",
  "dry_days_lt_1mm_5yr",
  "precipitation_total_mm_5yr"
)
climate_fallback_features <- c("temp_max_mean_c_5yr")

preferred_technology_features <- c(
  "osm_road_km_per_100km2",
  "osm_poi_count_per_100km2",
  "osm_health_education_poi_per_100km2"
)

# +1: larger raw value means greater vulnerability.
# -1: larger raw value means lower vulnerability and is direction-reversed.
indicator_direction <- c(
  heat_days_gt_35c_5yr = 1,
  temp_max_mean_c_5yr = 1,
  dry_days_lt_1mm_5yr = 1,
  precipitation_total_mm_5yr = -1,
  osm_road_km_per_100km2 = -1,
  osm_poi_count_per_100km2 = -1,
  osm_health_education_poi_per_100km2 = -1
)

indicator_labels <- c(
  heat_days_gt_35c_5yr = "Very hot days (> 35 °C)",
  temp_max_mean_c_5yr = "Mean daily maximum temperature",
  dry_days_lt_1mm_5yr = "Dry days (< 1 mm)",
  precipitation_total_mm_5yr = "Annual precipitation",
  osm_road_km_per_100km2 = "Road density",
  osm_poi_count_per_100km2 = "All mapped service (POI) density",
  osm_health_education_poi_per_100km2 = "Health and education facility density"
)

# ---- Helpers ----------------------------------------------------------------
`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
}

adm_path <- function(level) {
  file.path(
    path_admin,
    paste0("geoboundaries_", country_iso3, "_", level, ".geojson")
  )
}

worldpop_file_name <- function(iso3, year = worldpop_year) {
  glue::glue("{tolower(iso3)}_ppp_{year}_1km_Aggregated_UNadj.tif")
}

safe_write_csv <- function(x, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, file, na = "")
}

safe_download <- function(url, destfile, quiet = FALSE, overwrite = FALSE) {
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(destfile) && !overwrite) {
    message("Already present, skipping: ", basename(destfile))
    return(TRUE)
  }
  message("Downloading: ", url)
  tryCatch(
    {
      curl::curl_download(url, destfile, mode = "wb", quiet = quiet)
      TRUE
    },
    error = function(e) {
      message("Download failed: ", conditionMessage(e))
      if (file.exists(destfile)) unlink(destfile)
      FALSE
    }
  )
}

log_event <- function(dataset, source, url, status, note = NA_character_) {
  row <- tibble::tibble(
    timestamp = as.character(Sys.time()),
    dataset = dataset,
    source = source,
    url = url,
    status = status,
    note = note
  )
  file <- file.path(path_logs, "download_log.csv")
  readr::write_csv(row, file, append = file.exists(file))
}

# Records file size, modification time and MD5 checksum of raw inputs so that
# a later run can confirm it used identical source files.
record_checksum <- function(files, dataset) {
  files <- files[file.exists(files)]
  if (length(files) == 0) return(invisible(NULL))
  rows <- tibble::tibble(
    recorded_at = as.character(Sys.time()),
    dataset = dataset,
    file = basename(files),
    size_bytes = file.size(files),
    modified = as.character(file.mtime(files)),
    md5 = unname(tools::md5sum(files))
  )
  out <- file.path(path_logs, "raw_file_checksums.csv")
  previous <- if (file.exists(out)) {
    readr::read_csv(out, show_col_types = FALSE,
                    col_types = readr::cols(.default = "c"))
  } else {
    NULL
  }
  combined <- dplyr::bind_rows(
    previous,
    dplyr::mutate(rows, dplyr::across(dplyr::everything(), as.character))
  ) |>
    dplyr::distinct(dataset, file, md5, .keep_all = TRUE)
  safe_write_csv(combined, out)
  invisible(rows)
}

scale_01 <- function(x) {
  finite <- x[is.finite(x)]
  if (length(finite) == 0 || dplyr::n_distinct(finite) <= 1) {
    return(rep(NA_real_, length(x)))
  }
  (x - min(finite)) / (max(finite) - min(finite))
}

# Robust 5th-95th percentile scaling for sensitivity analysis.
robust_scale_01 <- function(x, lower = 0.05, upper = 0.95) {
  finite <- x[is.finite(x)]
  if (length(finite) == 0 || dplyr::n_distinct(finite) <= 1) {
    return(rep(NA_real_, length(x)))
  }
  limits <- stats::quantile(finite, probs = c(lower, upper), na.rm = TRUE, type = 8)
  if (!is.finite(diff(limits)) || diff(limits) <= 0) return(scale_01(x))
  clipped <- pmin(pmax(x, limits[[1]]), limits[[2]])
  (clipped - limits[[1]]) / diff(limits)
}

# Sample skewness and excess kurtosis with the small-sample corrections used
# by common spreadsheet functions (SKEW, KURT), as in the JRC-COIN guidance.
sample_skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3 || stats::sd(x) == 0) return(NA_real_)
  m <- mean(x)
  s <- stats::sd(x)
  n / ((n - 1) * (n - 2)) * sum(((x - m) / s)^3)
}

sample_excess_kurtosis <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 4 || stats::sd(x) == 0) return(NA_real_)
  m <- mean(x)
  s <- stats::sd(x)
  term1 <- n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(((x - m) / s)^4)
  term2 <- 3 * (n - 1)^2 / ((n - 2) * (n - 3))
  term1 - term2
}

# Rank-biserial correlation from the Mann-Whitney U statistic, reported as an
# absolute value: 0 = complete overlap, 1 = complete separation.
rank_biserial <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (length(x) == 0 || length(y) == 0) return(NA_real_)
  w <- suppressWarnings(stats::wilcox.test(x, y, exact = FALSE)$statistic)
  abs(1 - 2 * unname(w) / (length(x) * length(y)))
}

normalise_text <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("[^a-z0-9]+", " ") |>
    stringr::str_squish()
}

# Whole-word match of any pattern against normalised names.
pattern_match_any <- function(x, patterns) {
  x_norm <- normalise_text(x)
  pats <- normalise_text(patterns)
  regex <- paste0(
    "\\b(",
    paste(stringr::str_replace_all(pats, " ", "\\\\s+"), collapse = "|"),
    ")\\b"
  )
  stringr::str_detect(x_norm, regex)
}

feature_qc_table <- function(data, variables, domain) {
  tibble::tibble(variable = variables) |>
    dplyr::mutate(
      domain = domain,
      available = variable %in% names(data),
      non_missing_n = purrr::map_int(
        variable,
        ~ if (.x %in% names(data)) sum(is.finite(data[[.x]])) else 0L
      ),
      missing_pct = purrr::map_dbl(
        variable,
        ~ if (.x %in% names(data)) mean(!is.finite(data[[.x]])) * 100 else 100
      ),
      unique_n = purrr::map_int(
        variable,
        ~ if (.x %in% names(data)) dplyr::n_distinct(data[[.x]][is.finite(data[[.x]])]) else 0L
      ),
      sd = purrr::map_dbl(
        variable,
        ~ if (.x %in% names(data)) stats::sd(data[[.x]], na.rm = TRUE) else NA_real_
      ),
      usable = available & non_missing_n > 0 & unique_n > 1
    )
}

assert_minimum_features <- function(qc, domain, minimum) {
  usable_n <- sum(qc$usable & qc$domain == domain)
  if (usable_n < minimum) {
    failed <- qc |>
      dplyr::filter(domain == !!domain, !usable) |>
      dplyr::pull(variable)
    stop(
      domain, " domain contains only ", usable_n, " usable indicators; ",
      minimum, " are required. Check: ", paste(failed, collapse = ", "),
      call. = FALSE
    )
  }
}

# ---- Environment override file ---------------------------------------------
# Optional manual overrides keyed by ADM2 ID (preferred) or ADM2 name.
# Leave empty to use the documented automatic rule.
environment_override_file <- file.path(path_docs, "environment_type_overrides.csv")
if (!file.exists(environment_override_file)) {
  safe_write_csv(
    tibble::tibble(
      adm2_id = character(),
      adm2_name = character(),
      environment_type = character(),
      notes = character()
    ),
    environment_override_file
  )
}

message("Setup complete.")
