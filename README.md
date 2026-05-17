# Morocco-Climate-and-Technology



## Dataset Downloader
 
Datasets used in the Morocco climate exposure and technology access analysis. All downloads are handled by  `morocco_dataset_downloader.ipynb` (Colab).
 
### Running it
 
In Colab, open the notebook, run the install cell, set `TARGETS` in the config cell, and run the rest.
 


 
### Datasets
 
**Climate**
 
| ID | Variable | Source | Resolution |
|----|----------|--------|------------|
| C1 | Precipitation (for SPI-12) | CHIRPS v2.0 | 5 km, monthly |
| C2 | Land surface temperature | MODIS MOD11A2 v061 | 1 km, 8-day |
| C3 | NDVI (vegetation) | MODIS MOD13A1 v061 | 500 m, 16-day |
| C4 | Aridity Index | Global AI v3 (CGIAR-CSI) | 1 km, 1970–2000 climatology |
 
**Technology access**
 
| ID | Variable | Source |
|----|----------|--------|
| T1 | Electricity access | World Bank (`EG.ELC.ACCS.ZS`) |
| T2/T3 | Mobile, internet, broadband | World Bank (`IT.*`) |
| T4 | Road network | OSM via Geofabrik |
 
**Boundaries:** GADM v4.1 (levels 0–3), HDX/OCHA CODs, Natural Earth 10m.
 
### Output layout
 
```
morocco_data/
├── raw/            untouched downloads
├── processed/      cleaned, clipped, filtered outputs
├── boundaries/     admin shapefiles
└── download_log.txt
```
 
### What needs auth or manual work
 
**MODIS (C2, C3)** isn't pulled directly. The pipeline writes two helper scripts to the C2/C3 folders: one for NASA Earthdata (needs an account and `~/.netrc`), one for Google Earth Engine (`pip install earthengine-api && earthengine authenticate`). GEE is easier; it exports annual composites straight to your Drive. Morocco sits in MODIS tiles h17v05/v06 and h18v05/v06.
 
**Sub-national tech data is manual.** World Bank only goes down to national level. For provincial or regional figures:
 
- Electrification: HCP RGPH 2014, https://www.hcp.ma/
- Telecom: ANRT annual reports, https://www.anrt.ma/publications/rapport-annuel
- Detailed telecom: ITU DataHub, https://datahub.itu.int/
### Disk and time
 
A full pipeline run is 2–3 GB and 30–60 min on broadband, dominated by CHIRPS ( about 108 monthly TIFFs) and the Aridity Index globals. On Colab's free tier, either narrow `YEAR_START`/`YEAR_END` or mount Drive so downloads survive a runtime restart.
 
### Things to know before using the data
 
Resolutions don't match (CHIRPS 5 km, LST 1 km, NDVI 500 m). Resample to a common grid before any pixel-wise comparison; the downloader doesn't do this.
 
The Aridity Index is a 1970–2000 baseline, not current conditions. Pair it with CHIRPS-derived SPI for anomalies.
 
OSM road coverage is excellent for urban and primary routes, patchy for rural pistes. The pipeline keeps motorway through tertiary by default.
 
MODIS LST has quality flags (`LST_QC`); the default GEE script computes plain annual means without QC masking. Fix that before publication.
 
World Bank indicators usually lag 1–2 years.
 
### Citations and licenses
 

 
- **CHIRPS**: Funk et al. (2015), *Scientific Data* 2:150066. Public domain.
- **MODIS MOD11A2 / MOD13A1**: Wan et al. (2021), Didan (2021). NASA LP DAAC. Public use.
- **Global Aridity Index v3**: Zomer et al. (2022), *Scientific Data* 9:409. CC BY 4.0.
- **World Bank**: data.worldbank.org. CC BY 4.0.
- **OSM**: © OpenStreetMap contributors. ODbL 1.0.
- **GADM**: gadm.org. Free for academic use, not redistributable. Don't commit GADM files to the repo; each user re-downloads.
- **HDX / OCHA CODs**: data.humdata.org. Mostly CC BY-IGO.
- **Natural Earth**: naturalearthdata.com. Public domain.
For reproducibility, record the date you downloaded. CHIRPS, World Bank, and OSM URLs always pull the latest available. `download_log.txt` timestamps each dataset automatically.



## Machine Learning Analysis — Regional Vulnerability Typologies
 
The one-dimensional CTVI ranking collapses heterogeneous vulnerability profiles onto a single axis. Two regions can earn the same score for entirely different reasons. The ML component addresses this by classifying Morocco's 12 administrative regions into **four distinct vulnerability typologies** based on the multidimensional shape of their indicator profiles, not their composite score.
 
### Method
 
| Step | Detail |
|------|--------|
| **Pre-processing** | 10 regional indicators, min-max normalised to [0,1] (from `Regional_Normalized` sheet) |
| **Dimensionality reduction** | PCA — 2 components retained, covering **86.7% of cross-regional variance** |
| **Primary clustering** | K-Means (k=4, 200 initialisations, `random_state=42`) |
| **Validation** | Ward hierarchical clustering + silhouette analysis across k=2…6 |
| **Interpretation** | Random Forest permutation importance on the national CTVI time series (2000–2024) |
 
k=4 is preferred over the silhouette-optimal k=5, which isolates a single region (Drâa-Tafilalet) — a valid outlier flag but not a policy-actionable typology.
 
### Results
 
| Typology | Regions | Mean CTVI | Profile |
|----------|---------|-----------|---------|
| **T1: Resilient North** | Casablanca-Settat, Rabat-Salé-Kénitra | 0.37 | Low climate exposure, strongest tech penetration, high GDP |
| **T2: Mixed-Profile Interior** | Tanger-Tétouan, Fès-Meknès, Oriental, Béni Mellal-Khénifra | 0.49 | Moderate across all pillars; most internally diverse cluster |
| **T3: Climate-Stressed South** | Laâyoune-Sakia El Hamra, Dakhla-Oued Ed-Dahab | 0.65 | Extreme heat and water stress, but buffered by high GDP/capita |
| **T4: Extreme Aridity Frontier** | Marrakech-Safi, Souss-Massa, Guelmim-Oued Noun, Drâa-Tafilalet | 0.68 | High vulnerability across all four pillars — primary target for adaptation funding |
 
**Validation:** silhouette score = 0.41 (>0.25 threshold), Ward ARI = 0.40 — both above the conventional thresholds for non-random structure. The null hypothesis (no natural groupings) is rejected.
 
**Key insight:** Marrakech-Safi (rank 4) and Dakhla-Oued Ed-Dahab (rank 5) score within 0.011 of each other on the composite CTVI but land in different typologies with opposite policy implications — illustrating the practical value of the typology over the linear ranking alone.
 
#### Outputs
 
Running `Morocco_CTVI_ML_Pipeline.ipynb` produces:
 
```
fig1_k_selection.png        — Elbow & silhouette plots
fig2_pca_clusters.png       — Four typologies in PCA space
fig3_dendrogram.png         — Ward hierarchical dendrogram
fig4_cluster_profile.png    — Indicator heatmap by typology
fig5_pc1_loadings.png       — Data-driven indicator weights (PC1)
fig6_rf_importance.png      — Random Forest permutation importance
cluster_assignments.csv     — Region → typology mapping
cluster_profiles.csv        — Mean normalised scores per typology
pca_loadings.csv            — PCA component loadings
rf_feature_importance.csv   — RF indicator importance ranking
```
 
All results are fully reproducible with `random_state=42`.
 
