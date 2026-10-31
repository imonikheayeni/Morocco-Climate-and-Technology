# Morocco Climate-Technology Vulnerability Index (CTVI) — pipeline

Rewritten R pipeline for sourcing data, building an ML-based vulnerability
index at **province level (ADM2, ~75 units)**, and producing **statistical
hotspot maps** of Morocco.

## How to run
1. Open `R/00_run_all.R`, set `root_dir` to your project folder (forward slashes).
2. Source it. Scripts run in order 01 → 05. Large downloads (NASA ~75 calls,
   OSM ~480 MB, WorldPop raster) are cached, so re-runs skip what already exists.

## What changed from the previous version
- **One folder scheme.** The old `01_setup.R` defined paths twice and they
  conflicted (`boundaries` vs `admin_boundaries`); reads then missed the files.
  Now every script reads/writes through a single set of path variables, and
  boundary filenames are lowercase to match existing downloads.
- **No more double-sourcing / hard-coded `setwd`.** `00_run_all.R` sets the root
  once and sources each script exactly once.
- **Unit of analysis is ADM2 (~75 provinces), not ADM1 (12).** Every province is
  tagged to its parent ADM1 for context. This is the "keep more regions" goal.
- **A real raster step.** WorldPop 1 km population is aggregated to each province
  with `exactextractr` — giving spatially-varying population and density, and
  satisfying the "standardise raster" sourcing task.
- **National indicators handled honestly.** WDI/ITU/UN/FAOSTAT are national
  constants, so they can't discriminate between provinces. They're saved as
  documented *context* and listed in `05_logs/feasibility_matrix.csv`, not forced
  into the model. OSM road/POI density is the spatial technology-access proxy.
- **Fixed the broken UN block** (the old `|> .[1]` was invalid R).

## Machine-learning methodology (replaces the fixed 60/40 weights)
- **PCA** derives the composite CTVI weights from the data (`ctvi_score`); a
  transparent equal-weight index (`ctvi_equal_weight`) is kept for auditing.
- **k-means + hierarchical (Ward) clustering** define vulnerability typologies;
  `k` is chosen by the silhouette score, not assumed.
- **Random Forest** attributes which variables drive the index (importance +
  OOB pseudo-R²).

## Hotspot analysis
- **Getis-Ord Gi\*** → significant hot/cold spots at 90/95/99% (`map_05`).
- **Local Moran's I (LISA)** → High-High vulnerable clusters, Low-Low resilient
  clusters, and spatial outliers (`map_06`).

## Key outputs
| File | What it is |
|---|---|
| `03_processed/ml_ready/morocco_ctvi_adm2_features_raw.csv` | ADM2 feature table (raw) |
| `..._features_imputed.csv` | median-imputed, model-ready table |
| `..._index_and_clusters.csv` | CTVI, PCA & equal-weight scores, typologies, ranks |
| `..._hotspots.csv` / `..._index_hotspots.gpkg` | Gi\* and LISA results (+ GIS layer) |
| `pca_loadings.csv`, `pca_variance_explained.csv` | PCA diagnostics |
| `rf_variable_importance.csv` | Random Forest drivers |
| `04_maps/map_01 … map_07 (.png/.pdf)` | CTVI, typology, exposure, access gap, **Gi\* hotspots**, **LISA**, population priority |
| `04_maps/ctvi_rf_variable_importance.png`, `ctvi_hclust_dendrogram.png` | result charts |
| `05_logs/feasibility_matrix.csv` | variable / source / resolution / ADM2-usability / proxy notes |
| `05_logs/missingness_adm2_features.csv`, `download_log.csv` | QC + provenance |

## Known limitations (carry into the write-up)
- NASA POWER is sampled at one centroid per province; very large desert
  provinces are under-sampled. A gridded/raster climate aggregation would refine
  this if time allows.
- OSM completeness varies regionally; treat road/POI density as a *proxy* for
  service access, not a census of it.
- The Random Forest explains the constructed index (driver attribution); there is
  no external ground-truth vulnerability label, so this is interpretive, not
  out-of-sample prediction.
