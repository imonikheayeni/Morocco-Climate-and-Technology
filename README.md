# Climate-Technology Vulnerability Mapping in Morocco

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24044041.svg)](https://doi.org/10.5281/zenodo.24044041)

Code and derived data for:

> Ayeni, I., Namwila, M., Iraqi, Y., Damaj, I. *Mapping Climate-Technology Vulnerability across Morocco: An Open-Data Provincial Index, Spatial Typologies, and Urban-Rural Inequalities.* Manuscript submitted to *City and Environment Interactions*.

The pipeline builds a Climate-Technology Vulnerability Index (CTVI) for Morocco's 75 provinces and prefectures (ADM2) from open data. It keeps a climate-exposure sub-index (CE) and a technology/service-access gap sub-index (TG) separately visible, derives four planning typologies, compares urban-core, peri-urban and rural/lower-density units, and tests spatial concentration with FDR-adjusted Getis-Ord Gi* and Local Moran's I.

## Repository layout

| Path | Contents |
|---|---|
| `00_run_all.R` | Runs the whole pipeline (scripts 01-07) |
| `01_setup.R` | Settings, folder scheme, packages, helper functions |
| `02_download_sources.R` | Downloads raw inputs; logs dates and MD5 checksums |
| `03_clean_qc_build_ml_dataset.R` | Climate summaries, population, OSM densities, environment groups |
| `04_model_index_typology.R` | CTVI, typologies, group tests, sensitivity analyses |
| `05_hotspots_maps_reporting.R` | Gi*, Local Moran's I, global Moran's I, spatial checks |
| `06_publication_figures.R` | Multi-panel Figures 2-7 and the graphical-abstract map panels (vector PDF) |
| `07_manuscript_values.R` | `values.tex`, Supplementary Tables S1-S3, consistency report |
| `00_documentation/` | ADM2 display names, optional environment overrides, sources for Figure 1 and the graphical abstract |
| `01_raw/boundaries/` | geoBoundaries ADM1/ADM2 files used in the paper (pinned) |
| `03_processed/`, `05_logs/`, `06_tables/`, `07_manuscript/` | Outputs of the run archived with each release |
| `07_manuscript/as_submitted/` | The single-panel figure files and graphical abstract used in the submitted manuscript, with a table linking each to its pipeline output |

Earlier exploratory material (regional ADM1 notebooks and spreadsheets) was removed from the current version; it remains available in the repository history.

## How to run

Requirements: R 4.2 or later (UTF-8 locale) and an internet connection for the first run. Missing CRAN packages are installed by `01_setup.R`.

```r
# With the repository folder as the working directory:
source("00_run_all.R")
```

The first run downloads about 0.5 GB (mainly the Geofabrik OpenStreetMap GeoPackage) and calls the NASA POWER API once per ADM2 unit. Downloads are cached, so later runs reuse them.

After a run, open `07_manuscript/consistency_report.csv`. Every numerical statement in the manuscript is listed with the value from the run and a `PASS`/`CHECK` status.

## Data sources

| Source | Use | Licence |
|---|---|---|
| geoBoundaries gbOpen, MAR ADM1/ADM2 (Runfola et al., 2020) | Study units | Open licence of the source dataset (see geoBoundaries metadata) |
| NASA POWER daily point API (AG community) | Heat days, dry days, precipitation, 2020-2024 | Public, NASA Langley Research Center |
| WorldPop 2020, 1 km, UN-adjusted (doi:10.5258/SOTON/WP00671), country rasters MAR and ESH | Population, density, priority score | CC BY 4.0 |
| OpenStreetMap via Geofabrik (`morocco-latest-free.gpkg.zip`) | Road, POI and health/education densities | ODbL 1.0, (c) OpenStreetMap contributors |
| World Bank WDI | National context only | CC BY 4.0 |

WorldPop distributes the area south of 27 deg 40' N as a separate raster (ISO code ESH). Both the MAR and ESH rasters are downloaded and summed so that all 75 ADM2 units receive a population estimate. Boundaries are used for analytical consistency only and imply no position on any territorial question.

Geofabrik's "latest" extract changes daily. `05_logs/raw_file_checksums.csv` and `05_logs/osm_layers_used.csv` identify the extract and layers used in each run; the derived densities are archived in `03_processed/ml_ready/`.

## Methods summary

- Climate: mean annual days with Tmax > 35 C, dry days (< 1 mm) and annual precipitation, 2020-2024, at one interior point per unit.
- Access: road km (motorway to residential classes), all OSM POI point features, and health/education POIs, each per 100 km2 (EPSG:6933).
- Index: direction-aligned min-max scaling; equal weights within and between the two sub-indices.
- Typologies: median splits of CE and TG (units at the median count as high).
- Environment groups: whole-word name anchors for major cities and their fringe units, then population-density percentiles (85th and 40th) across all units. A contiguity-based peri-urban definition is tested as a check.
- Spatial statistics: queen contiguity; Benjamini-Hochberg adjustment; k-nearest-neighbour (k = 4) and permutation checks.
- Sensitivity: robust (5th-95th percentile) scaling, log-transformed access densities, PCA ranking, k-means/Ward clustering, Random Forest reconstruction diagnostic.

## Citation and licence

Cite the archived release:

> Ayeni, I., Namwila, M., Iraqi, Y., Damaj, I. (2026). *Code and derived data for: Mapping Climate-Technology Vulnerability across Morocco* (Version 1.0.0). Zenodo. https://doi.org/10.5281/zenodo.24044041

Version 1.0.0 is the version used in the manuscript. The concept DOI [10.5281/zenodo.24044040](https://doi.org/10.5281/zenodo.24044040) always resolves to the latest version.

Code: MIT licence (`LICENSE`). Derived tables: CC BY 4.0, except values derived from OpenStreetMap, which are available under ODbL 1.0.

## Funding

British Council, Going Global Partnerships - Research Environments programme (grant RE2025-942).
