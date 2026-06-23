# =============================================================================
# 01_setup.R
# Creates ONE folder scheme, installs/loads packages, sets parameters & helpers.
# (Replaces the previous version which defined paths twice and conflicted.)
# =============================================================================

# `root_dir` is normally set by 00_run_all.R. If this script is run on its own,
# fall back to the working directory.
if (!exists("root_dir") || !dir.exists(root_dir)) root_dir <- getwd()
message("Project root folder is: ", root_dir)

# ---- Country settings -------------------------------------------------------
country_iso3 <- "MAR"
country_iso2 <- "MA"
country_name <- "Morocco"

# ---- Study window -----------------------------------------------------------
start_year <- 2014
end_year   <- 2024
worldpop_year <- 2020   # latest WorldPop top-down year

# ---- SINGLE canonical folder scheme ----------------------------------------
# Everything reads and writes through THESE variables. Do not redefine them.
path_raw          <- file.path(root_dir, "01_raw")
path_admin        <- file.path(path_raw, "boundaries")   # matches existing files
path_nasa         <- file.path(path_raw, "nasa_power")
path_worldpop     <- file.path(path_raw, "worldpop")
path_wdi          <- file.path(path_raw, "world_bank")
path_un           <- file.path(path_raw, "un")
path_fao          <- file.path(path_raw, "fao")
path_osm          <- file.path(path_raw, "osm")
path_hcp          <- file.path(path_raw, "hcp")

path_intermediate <- file.path(root_dir, "02_intermediate")
path_int_spatial  <- file.path(path_intermediate, "spatial")
path_int_climate  <- file.path(path_intermediate, "climate")
path_int_socio    <- file.path(path_intermediate, "socio_tech")

path_processed    <- file.path(root_dir, "03_processed")
path_ml           <- file.path(path_processed, "ml_ready")

path_maps         <- file.path(root_dir, "04_maps")
path_logs         <- file.path(root_dir, "05_logs")
path_docs         <- file.path(root_dir, "00_documentation")

all_dirs <- c(path_raw, path_admin, path_nasa, path_worldpop, path_wdi, path_un,
              path_fao, path_osm, path_hcp, path_intermediate, path_int_spatial,
              path_int_climate, path_int_socio, path_processed, path_ml,
              path_maps, path_logs, path_docs)
for (d in all_dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
message("Folder setup complete.")

# ---- Packages ---------------------------------------------------------------
required_pkgs <- c(
  "tidyverse", "sf", "terra", "exactextractr",      # spatial + raster
  "jsonlite", "httr2", "curl", "lubridate", "glue", # io / utils
  "janitor", "readxl", "scales",
  "WDI", "FAOSTAT",                                 # context sources
  "cluster", "factoextra", "randomForest",          # machine learning
  "spdep"                                           # hotspot statistics
)

install_if_missing <- function(pkgs) {
  miss <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(miss)) install.packages(miss, dependencies = TRUE)
}
install_if_missing(required_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ---- NASA POWER parameters (exposure) --------------------------------------
nasa_params <- c(
  "T2M",               # mean 2 m temperature, deg C
  "T2M_MAX",           # daily max temperature, deg C
  "T2M_MIN",           # daily min temperature, deg C
  "PRECTOTCORR",       # precipitation, mm/day
  "WS2M",              # wind speed at 2 m, m/s
  "RH2M",              # relative humidity, %
  "ALLSKY_SFC_SW_DWN"  # solar radiation, kWh/m2/day
)

# ---- World Bank / ITU / UN context indicators (NATIONAL level) -------------
# NOTE: these are national values. They are documented and kept as CONTEXT,
# they are NOT used as ADM2 model features (a national constant cannot
# discriminate between provinces). See the feasibility note written in 03.
wdi_indicators <- c(
  internet_users_pct            = "IT.NET.USER.ZS",
  mobile_subscriptions_per100   = "IT.CEL.SETS.P2",
  fixed_broadband_per100        = "IT.NET.BBND.P2",
  electricity_access_pct        = "EG.ELC.ACCS.ZS",
  rural_electricity_access_pct  = "EG.ELC.ACCS.RU.ZS",
  urban_electricity_access_pct  = "EG.ELC.ACCS.UR.ZS",
  population_total              = "SP.POP.TOTL",
  urban_population_pct          = "SP.URB.TOTL.IN.ZS",
  agricultural_land_pct         = "AG.LND.AGRI.ZS",
  cereal_yield_kg_per_ha        = "AG.YLD.CREL.KG",
  gdp_per_capita_constant_usd   = "NY.GDP.PCAP.KD",
  national_poverty_headcount_pct= "SI.POV.NAHC"
)

# ---- Download toggles -------------------------------------------------------
download_heavy_osm <- TRUE   # ~480 MB Geofabrik GeoPackage
download_worldpop  <- TRUE   # ~ tens of MB WorldPop 1 km raster

# ---- OSM road classes treated as "access" infrastructure -------------------
osm_major_road_classes <- c("motorway", "trunk", "primary", "secondary",
                            "tertiary", "unclassified", "residential")

# ---- Helper: boundary file paths -------------------------------------------
adm_path <- function(level) {
  # level is "ADM1" or "ADM2"; filenames are lowercase to match existing files
  file.path(path_admin, paste0("geoboundaries_", country_iso3, "_", level, ".geojson"))
}

# ---- Small helpers ----------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

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
  tryCatch({
    curl::curl_download(url, destfile, mode = "wb", quiet = quiet)
    TRUE
  }, error = function(e) {
    message("  Download FAILED: ", conditionMessage(e)); FALSE
  })
}

# Append-only master download log (one tidy file, not many).
log_event <- function(dataset, source, url, status, note = NA_character_) {
  row <- tibble::tibble(
    timestamp = as.character(Sys.time()),
    dataset = dataset, source = source, url = url,
    status = status, note = note
  )
  f <- file.path(path_logs, "download_log.csv")
  readr::write_csv(row, f, append = file.exists(f))
}

# Rescale to 0-1 (NA-safe; constant vectors -> NA so they drop out of PCA)
scale_01 <- function(x) {
  if (all(is.na(x)) || length(unique(stats::na.omit(x))) <= 1)
    return(rep(NA_real_, length(x)))
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

message("Setup complete. Root directory: ", root_dir)

#list.files(file.path(root_dir, "01_raw", "worldpop"), pattern = "\\.tif$")
