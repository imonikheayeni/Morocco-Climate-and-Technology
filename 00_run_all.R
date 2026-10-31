# =============================================================================
# 00_run_all.R
# Runs the Climate-Technology Vulnerability Index (CTVI) pipeline for Morocco.
#
# Usage: open the project folder (the folder containing this file) in RStudio
# or set it as the working directory, then run:
#   source("00_run_all.R")
#
# Outputs used in the manuscript are written to 07_manuscript/. Check
# 07_manuscript/consistency_report.csv after every run.
# =============================================================================

root_dir <- getwd()

scripts <- c(
  "01_setup.R",
  "02_download_sources.R",
  "03_clean_qc_build_ml_dataset.R",
  "04_model_index_typology.R",
  "05_hotspots_maps_reporting.R",
  "06_publication_figures.R",
  "07_manuscript_values.R"
)

missing_scripts <- scripts[!file.exists(file.path(root_dir, scripts))]
if (length(missing_scripts) > 0) {
  stop(
    "The following scripts are missing from the project root: ",
    paste(missing_scripts, collapse = ", "),
    call. = FALSE
  )
}

pipeline_started <- Sys.time()
message("CTVI pipeline started: ", pipeline_started)

for (script in scripts) {
  message("\n", paste(rep("=", 78), collapse = ""))
  message("Running ", script)
  message(paste(rep("=", 78), collapse = ""))
  source(file.path(root_dir, script), local = globalenv(), echo = FALSE)
}

writeLines(
  capture.output(sessionInfo()),
  file.path(path_logs, "session_info.txt")
)

pipeline_finished <- Sys.time()
message("\nCTVI pipeline completed: ", pipeline_finished)
message(
  "Elapsed time: ",
  round(as.numeric(difftime(pipeline_finished, pipeline_started, units = "mins")), 1),
  " minutes"
)
