# =============================================================================
# Morocco Climate-Technology Vulnerability Index (CTVI)
# Master runner
# -----------------------------------------------------------------------------
# Runs the full pipeline in order. Each script is sourced exactly once.
#
# HOW TO RUN
#   1. Set `root_dir` below to the folder that contains the R/, 01_raw/, ...
#      sub-folders (the project root). Use FORWARD slashes, even on Windows.
#   2. Open this file in RStudio and click "Source", or run:
#        source("R/00_run_all.R")
#
# Author: Imonikhe Ayeni  |  Pipeline rewritten for ADM2 + ML + hotspots
# =============================================================================
getwd()
# ---- 0. Project root --------------------------------------------------------
# EDIT THIS ONE LINE to point at your project root, then never touch paths again.
root_dir <- "C:/Users/User/Downloads/Morocco_CTVI_data_sourcing_pack"

# Fallback: if the line above does not exist on this machine, use the current
# working directory instead (handy when a collaborator clones the repo).
if (!dir.exists(root_dir)) {
  message("root_dir not found, falling back to getwd(): ", getwd())
  root_dir <- getwd()
}
setwd(root_dir)
message("Project root: ", root_dir)

# ---- 1. Run the pipeline ----------------------------------------------------
source(file.path(root_dir, "01_setup.R"))                  # folders, pkgs, params
source(file.path(root_dir, "02_download_sources.R"))       # raw downloads
source(file.path(root_dir, "03_clean_qc_build_ml_dataset.R")) # ADM2 feature build
source(file.path(root_dir, "04_model_and_index.R"))        # PCA / clustering / RF
source(file.path(root_dir, "05_hotspots_and_maps.R"))      # Gi*, LISA, maps


message("\n=============================================================")
message("Pipeline complete. Outputs:")
message("- 01_raw/                original downloaded source files")
message("- 02_intermediate/       harmonised climate / spatial / socio-tech data")
message("- 03_processed/ml_ready/ modelling tables, index, clusters")
message("- 04_maps/               choropleth, typology and HOTSPOT maps")
message("- 05_logs/               download log, missingness & feasibility notes")
message("=============================================================")

### sanity check
# readr::read_csv(file.path(root_dir, "03_processed", "ml_ready",
#                           "morocco_ctvi_adm2_features_imputed.csv")) |>
#   dplyr::select(adm2_name, population, population_density_per_km2) |> head()
# ### whre hotspot landed
# readr::read_csv(file.path(root_dir, "03_processed", "ml_ready",
#                           "morocco_ctvi_adm2_hotspots.csv")) |>
#   dplyr::arrange(dplyr::desc(ctvi_score)) |>
#   dplyr::select(adm2_name, adm1_name, ctvi_score, hotspot, lisa) |> head(10)
