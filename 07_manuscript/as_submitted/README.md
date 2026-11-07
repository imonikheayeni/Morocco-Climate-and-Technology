# Figures as submitted

These are the figure files used in the submitted manuscript (Elsevier double-column template, one panel per figure). They come from the pipeline outputs in `07_manuscript/` as follows.

| Manuscript figure | Content | Source |
|---|---|---|
| Fig. 1 | Analytical workflow | `00_documentation/figure_1_workflow.tex` |
| Fig. 2 | CTVI | `07_manuscript/Figure_2.pdf`, panel (a) |
| Fig. 3 | Climate-exposure sub-index (CE) | `07_manuscript/Figure_2.pdf`, panel (b) |
| Fig. 4 | Service-access gap sub-index (TG) | `07_manuscript/Figure_2.pdf`, panel (c) |
| Fig. 5 | Planning typologies | `07_manuscript/Figure_3.pdf`, panel (a) |
| Fig. 6 | Bivariate tertile map | `07_manuscript/Figure_3.pdf`, panel (b) |
| Fig. 7 | Mean scores by environment | `07_manuscript/Figure_4.pdf`, panel (a) |
| Fig. 8 | Typology composition by environment | `07_manuscript/Figure_4.pdf`, panel (b), labels corrected (see below) |
| Fig. 9 | Getis-Ord Gi* hot and cold spots | `07_manuscript/Figure_5.pdf`, panel (a) |
| Fig. 10 | Local Moran's I | `07_manuscript/Figure_5.pdf`, panel (b) |
| Fig. 11 | Population-weighted priority | `07_manuscript/Figure_6.pdf` |
| Fig. 12 | Random Forest diagnostic | `07_manuscript/Figure_7.pdf` |
| Graphical abstract | Planning typologies and hot spots | `00_documentation/graphical_abstract.tex`, built from the map panels of `07_manuscript/Graphical_abstract.pdf` with plain-language text |

Figs. 2 to 10 were cropped from the multi-panel pipeline figures without other changes, except for Fig. 8.

## Changes made to the scripts after the archived run

The outputs in `05_logs/`, `06_tables/` and `07_manuscript/` are from the run of 5 November 2026. Two presentation fixes were made to the scripts afterwards. Neither changes any value.

1. `06_publication_figures.R`, Figure 4(b): the percentage labels of two segments in the peri-urban bar were drawn in swapped positions (24% and 18%). The labels now stack in the same order as the bars. `07_manuscript/Figure_4.pdf` predates the fix; Fig. 8 here shows the corrected labels.
2. `07_manuscript_values.R`, Supplementary Table S3d: the spatial checks now carry readable labels instead of internal identifiers (for example "Gi* hot spots, queen contiguity" instead of `gi_hot_spots_queen`). `07_manuscript/Table_S3d_spatial_checks.csv` and `Supplementary_material.tex` predate the fix.

Re-running `06_publication_figures.R` and `07_manuscript_values.R` regenerates both outputs with the fixes.
