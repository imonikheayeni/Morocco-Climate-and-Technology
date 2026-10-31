# Publication QA branch — review before release
This branch replaces an older set of modelling scripts with the reviewed 00–05 pipeline.
Run `source("00_run_all.R")` from the project root in R; 06_publication_QA.R will check numerical integrity.
The current WorldPop extraction includes five implausible zero/near-zero totals: Aousserd, Boujdour, Laayoune, Oued Ed-Dahab, Es-Semara. The script now flags and excludes invalid population inputs from priority scores. **Do not publish population priority maps pending independent validation.**
Supply and review `00_documentation/environment_type_overrides.csv` to reproduce the audited 11/34/30 grouping. The publication S1 table must be used as the identifier-locked mapping. The existing naming rules can accidentally classify Fquih Ben Saleh as Salé.
Critical next checks: re-run on original NASA POWER, OSM and WorldPop raw data; verify administrative boundaries, OSM layer names, climate point sampling, density-denominator confounding, the 75-unit crosswalk, and compare reproduced statistics with the journal supplement.
Zenodo will preserve a repository snapshot only after a **GitHub release** is created on an enabled repository. No DOI is asserted by this branch.
