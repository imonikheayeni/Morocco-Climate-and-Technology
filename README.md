# Morocco-Climate-and-Technology



# 📦 Data

This section describes the datasets used in the Morocco climate-exposure and technology-access analysis, how to download them, and what to be aware of before running the pipeline.

All datasets are pulled by `morocco_dataset_downloader.py` (CLI) or `morocco_dataset_downloader.ipynb` (Colab/Jupyter). The two share the same logic — pick whichever fits your workflow.

---

## Quick start

### Option A — Colab (recommended for first runs)

1. Open `morocco_dataset_downloader.ipynb` in Google Colab.
2. Run cell 1 to install the geospatial stack (~60–90 s).
3. *(Optional)* Uncomment the Drive-mount cell to persist downloads across sessions.
4. Edit the `TARGETS` variable in the **Configuration** cell:
   - `"all"` — everything
   - `"climate"` — C1, C2, C3, C4
   - `"tech"` — T1, T2, T4
   - `"admin"` — administrative boundaries only
   - or an explicit list, e.g. `["C1", "T4", "ADMIN"]`
5. Run the remaining cells top-to-bottom.

### Option B — Local CLI

```bash
pip install -r requirements.txt
python morocco_dataset_downloader.py --all
# or selectively:
python morocco_dataset_downloader.py --group climate --year-start 2015 --year-end 2023
python morocco_dataset_downloader.py --ids C1 T4 ADMIN
python morocco_dataset_downloader.py --summary-only
```

---

## Datasets

### 🌡️ Group 1 — Climate exposure

| ID | Variable | Source | Resolution | Temporal | Auth? |
|----|----------|--------|------------|----------|-------|
| **C1** | Precipitation (→ SPI-12 drought) | CHIRPS v2.0 (UCSB CHC) | 0.05° (~5 km) | Monthly, 1981–present | No |
| **C2** | Land Surface Temperature (heat stress) | MODIS MOD11A2 v061 (NASA LP DAAC) | 1 km | 8-day composites | **Yes** |
| **C3** | NDVI (vegetation) | MODIS MOD13A1 v061 (NASA LP DAAC) | 500 m | 16-day composites | **Yes** |
| **C4** | Aridity Index (P/PET baseline) | Global Aridity Index v3 (CGIAR-CSI / Figshare) | ~1 km | Climatology 1970–2000 | No |

**SPI-12 (C1):** computed downstream with the [`climate-indices`](https://pypi.org/project/climate-indices/) package — fit a gamma distribution to 12-month rolling precipitation sums, then transform to standard normal.

**Aridity Index thresholds (C4):**

| AI value | Class |
|----------|-------|
| < 0.03 | Hyper-arid |
| 0.03–0.20 | Arid |
| 0.20–0.50 | Semi-arid |
| 0.50–0.65 | Dry sub-humid |
| > 0.65 | Humid |

### 📡 Group 2 — Technology access

| ID | Variable | Source | Coverage | Auth? |
|----|----------|--------|----------|-------|
| **T1** | Electricity access (% of population) | World Bank `EG.ELC.ACCS.ZS` | National, annual 2000–2023 | No |
| **T2** | Mobile cellular subscriptions (per 100) | World Bank `IT.CEL.SETS.P2` | National, annual 2010–2023 | No |
| **T3** | Individuals using the Internet (%) | World Bank `IT.NET.USER.ZS` | National, annual 2010–2023 | No |
| **T3b** | Fixed broadband (per 100) | World Bank `IT.NET.BBND.P2` | National, annual 2010–2023 | No |
| **T4** | Road network | OpenStreetMap via [Geofabrik](https://download.geofabrik.de/africa/morocco.html) | Snapshot, vector | No |

> ⚠️ **Sub-national tech data is manual.** The World Bank only provides *national* figures. For provincial/regional analysis you'll need:
> - **HCP** (Haut-Commissariat au Plan) — provincial electrification from [RGPH 2014 census tables](https://www.hcp.ma/)
> - **ANRT** (Agence Nationale de Réglementation des Télécommunications) — regional telecom from [annual reports](https://www.anrt.ma/publications/rapport-annuel)
> - **ITU DataHub** — detailed telecom indicators at [datahub.itu.int](https://datahub.itu.int/)

### 🗺️ Group 3 — Administrative boundaries

| Source | Levels | Notes |
|--------|--------|-------|
| **GADM v4.1** | 0–3 (country → commune) | Most detailed; used as the primary join layer |
| **HDX / OCHA** (`cod-ab-mar`) | 0–3 | UN-validated common operational dataset |
| **Natural Earth 10m** | 0 (country only) | For cartography / wide-extent maps |

---

## Output structure

```
morocco_data/
├── raw/                    # Downloaded files, untouched
│   ├── C1_chirps/          # *.tif monthly precipitation
│   ├── C2_modis_lst/       # helper scripts (see auth below)
│   ├── C3_modis_ndvi/      # helper scripts
│   ├── C4_aridity/         # Global_AI_v3.tif, Global_ET0_v3.tif
│   ├── T1_electricity/     # World Bank CSV
│   ├── T2_T3_telecom/      # World Bank CSV
│   └── T4_roads/           # OSM shapefile extract
├── processed/              # Cleaned / clipped / filtered outputs
│   ├── C4_aridity_morocco.tif
│   ├── T1_electricity_access.csv
│   ├── T2_T3_telecom_indicators.csv
│   └── T4_morocco_major_roads.gpkg
├── boundaries/             # Admin shapefiles
│   ├── gadm41_MAR_shp/
│   ├── hdx_*/
│   └── natural_earth/
└── download_log.txt        # Per-dataset status (DONE / PARTIAL / FAILED)
```

---

## Authentication & manual steps

### MODIS (C2, C3) — needs an account

The downloader **does not pull MODIS directly** — it writes ready-to-run helper scripts to `raw/C2_modis_lst/` and `raw/C3_modis_ndvi/`. Pick one path:

**Path 1 — Google Earth Engine (recommended)**
- Free for non-commercial / research use.
- One-time setup: `pip install earthengine-api && earthengine authenticate`
- Run the generated `download_modis_*_gee.py` — exports annual mean composites directly to your Google Drive.
- Handles tiling, mosaicking, and unit conversion server-side.

**Path 2 — NASA Earthdata bulk download**
- Create a free account at [urs.earthdata.nasa.gov](https://urs.earthdata.nasa.gov).
- Configure `~/.netrc`:
  ```bash
  echo "machine urs.earthdata.nasa.gov login YOUR_USER password YOUR_PASS" > ~/.netrc
  chmod 600 ~/.netrc
  ```
- Run the generated `download_modis_lst.sh`. Note: the shell script is Linux/macOS only.

Morocco is covered by MODIS tiles **h17v05, h17v06, h18v05, h18v06**.

---

## Disk usage & runtime

Rough estimates for the default 2015–2023 window:

| Dataset | Size | Time (broadband) |
|---------|------|------------------|
| C1 CHIRPS (monthly TIFFs, 108 files) | 1–2 GB | 15–40 min |
| C2 MODIS LST (via GEE) | varies (annual means: ~10 MB/yr) | hours (async) |
| C3 MODIS NDVI (via GEE) | varies | hours (async) |
| C4 Aridity (2 global TIFFs) | ~500 MB | 2–5 min |
| T1 / T2 / T3 (CSV from APIs) | < 1 MB | seconds |
| T4 OSM Morocco shapefile | ~150 MB | 1–2 min |
| Boundaries (GADM + HDX + NE) | ~50 MB | < 1 min |
| **Full pipeline** | **~2–3 GB on disk** | **~30–60 min** (excluding async GEE) |

> 💡 **Colab free-tier tip:** if you're not using Drive, the runtime disk is ephemeral and limited. Either narrow the date range or mount Drive (cell 3 in the notebook) to keep downloads.

---

## Licenses & citation

When using these data, **please cite the original sources** — not this repository. Each dataset has its own terms:

| Dataset | License | Required citation |
|---------|---------|-------------------|
| CHIRPS | Public domain | Funk et al. (2015). *Scientific Data*, 2: 150066. doi:10.1038/sdata.2015.66 |
| MODIS MOD11A2 | NASA public-use | Wan, Z., Hook, S., Hulley, G. (2021). MOD11A2 MODIS/Terra LST 8-Day L3 Global 1km SIN Grid V061. NASA LP DAAC. |
| MODIS MOD13A1 | NASA public-use | Didan, K. (2021). MOD13A1 MODIS/Terra Vegetation Indices 16-Day L3 Global 500m SIN Grid V061. NASA LP DAAC. |
| Global Aridity Index v3 | CC BY 4.0 | Zomer, R.J., Xu, J., Trabucco, A. (2022). *Scientific Data*, 9: 409. doi:10.1038/s41597-022-01493-1 |
| World Bank indicators | CC BY 4.0 | World Bank Open Data. data.worldbank.org |
| OSM (via Geofabrik) | ODbL 1.0 | © OpenStreetMap contributors |
| GADM | Free for academic / non-commercial use; **not** redistributable | gadm.org |
| HDX / OCHA CODs | Varies — typically CC BY-IGO | data.humdata.org |
| Natural Earth | Public domain | naturalearthdata.com |

⚠️ **GADM is the strictest.** It cannot be redistributed — each user must download their own copy. The pipeline does this automatically; do **not** commit GADM files to the repo.

---

## Known limitations & caveats

- **Spatial resolution mismatch.** CHIRPS (5 km) ≠ MODIS LST (1 km) ≠ NDVI (500 m). All raster products must be resampled to a common grid before pixel-wise analysis. The pipeline does **not** do this automatically — handle it explicitly in your analysis notebook.
- **Aridity Index is a static climatology** (1970–2000). It captures long-run aridity, not contemporary anomalies. For trend analysis, combine with CHIRPS-derived SPI.
- **OSM road completeness is uneven** across Morocco — coverage is excellent for urban areas and major routes, sparser for rural pistes and tracks. Filtering to `motorway/trunk/primary/secondary/tertiary` (as the pipeline does) gives the most consistent picture.
- **World Bank data is national.** For sub-national tech-access analysis, the HCP/ANRT manual workflow is unavoidable.
- **MODIS pixel masking.** MOD11A2 LST has quality flags (`LST_QC`) — production analysis should mask cloud-contaminated pixels before computing means. The GEE script in this repo produces simple annual means and does **not** apply QC masking by default.
- **CHIRPS reliability.** Generally excellent over Africa, but station density varies. Northern Morocco is well-covered; the southern Saharan zone relies more heavily on satellite estimates.
- **Date coverage of source APIs may lag.** The World Bank indicators typically have a 1–2 year reporting lag. Re-run periodically.

---

## Reproducibility

The pipeline pins data versions where the source provides them (e.g. MODIS `v061`, GADM `v4.1`, Aridity Index `v3`). Where versions aren't pinned (CHIRPS, World Bank, OSM/Geofabrik), the URLs always pull the latest available data — record the **download date** for any analysis you publish. The `download_log.txt` file captures timestamped per-dataset status for this purpose.
