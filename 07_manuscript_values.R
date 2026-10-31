# =============================================================================
# 07_manuscript_values.R
# Links the manuscript to the pipeline.
#
# Writes to 07_manuscript/:
#   * values.tex                 LaTeX macros for every value the manuscript
#                                takes from the pipeline (\input in main.tex)
#   * Supplementary_material.tex Supplementary Tables S1-S3 (compile to PDF)
#   * Table_S1/S2/S3 *.csv       machine-readable versions
#   * consistency_report.csv     every numeric statement in the manuscript
#                                checked against this run (PASS / CHECK)
# =============================================================================

read_out <- function(...) readr::read_csv(file.path(...), show_col_types = FALSE)

display_names <- read_out(path_docs, "adm2_display_names.csv") |>
  dplyr::select(adm2_id, display_name, unit_type)

idx <- read_out(path_ml, "morocco_ctvi_adm2_index_typologies.csv") |>
  dplyr::left_join(display_names, by = "adm2_id")
spatial <- read_out(path_tables, "spatial_statistics_by_unit.csv") |>
  dplyr::left_join(display_names, by = "adm2_id")
population <- read_out(path_tables, "population_extraction_check.csv") |>
  dplyr::left_join(display_names, by = "adm2_id")
kruskal <- read_out(path_tables, "environment_kruskal_wallis_tests.csv")
pairwise <- read_out(path_tables, "environment_pairwise_wilcoxon_bh.csv")
agreement <- read_out(path_tables, "sensitivity_rank_and_typology_agreement.csv")
distribution <- read_out(path_tables, "indicator_distribution_diagnostics.csv")
geo_counts <- read_out(path_tables, "geographic_environment_counts.csv")
geo_crosstab <- read_out(path_tables, "geographic_vs_primary_environment.csv")
geo_pairwise <- read_out(path_tables, "geographic_environment_pairwise_bh.csv")
geo_kruskal <- read_out(path_tables, "geographic_environment_kruskal_wallis.csv")
spatial_sens <- read_out(path_tables, "spatial_statistics_sensitivity.csv")
moran <- read_out(path_tables, "global_moran_spatial_weights_sensitivity.csv")
diagnostics <- read_out(path_tables, "model_diagnostics.csv")
clusters <- read_out(path_tables, "data_driven_cluster_profiles.csv")
rf_importance <- read_out(path_ml, "rf_index_sensitivity_importance.csv")

if (anyNA(idx$display_name)) {
  stop("Some ADM2 IDs are missing from 00_documentation/adm2_display_names.csv.")
}

core <- "Smart city / urban core"
peri <- "Urban edge / peri-urban"
rural <- "Rural / lower-density"

# ---- Formatting helpers ------------------------------------------------------
# Write UTF-8 bytes regardless of the system locale (accented place names).
write_utf8 <- function(lines, file) {
  con <- file(file, open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(as.character(lines)), con, useBytes = TRUE)
}

f3 <- function(x) formatC(x, format = "f", digits = 3)
f2 <- function(x) formatC(x, format = "f", digits = 2)
f1 <- function(x) formatC(x, format = "f", digits = 1)
fint <- function(x) formatC(round(x), format = "d", big.mark = ",")
fp <- function(p) {
  if (!is.finite(p)) return("$p$ not available")
  if (p < 0.001) "$p<0.001$" else paste0("$p=", f3(p), "$")
}
fp_adj <- function(p) sub("^\\$p", "adjusted $p", fp(p))
number_word <- function(n) {
  words <- c("zero", "one", "two", "three", "four", "five", "six", "seven",
             "eight", "nine", "ten", "eleven", "twelve")
  if (n >= 0 && n <= 12) words[n + 1] else as.character(n)
}
latex_list <- function(x) {
  x <- as.character(x)
  if (length(x) <= 1) return(x)
  if (length(x) == 2) return(paste(x, collapse = " and "))
  paste0(paste(head(x, -1), collapse = ", "), " and ", tail(x, 1))
}
latex_escape <- function(x) gsub("([&%#_])", "\\\\\\1", x)
region_tex <- function(x) gsub("-", "--", x, fixed = TRUE)
format_date <- function(x) {
  d <- as.Date(substr(as.character(x), 1, 10))
  ifelse(is.na(d), "date not recorded", format(d, "%e %B %Y") |> trimws())
}

# ---- Data access dates from the download log ---------------------------------
download_log_file <- file.path(path_logs, "download_log.csv")
download_log <- if (file.exists(download_log_file)) {
  readr::read_csv(download_log_file, show_col_types = FALSE,
                  col_types = readr::cols(.default = "c"))
} else {
  tibble::tibble(timestamp = character(), dataset = character(), status = character())
}
first_success <- function(pattern, fallback_files = character()) {
  hits <- download_log |>
    dplyr::filter(stringr::str_detect(dataset, pattern), status == "downloaded")
  if (nrow(hits) > 0) return(format_date(min(hits$timestamp)))
  fallback_files <- fallback_files[file.exists(fallback_files)]
  if (length(fallback_files) > 0) return(format_date(min(file.mtime(fallback_files))))
  "date not recorded"
}
osm_date <- first_success(
  "OpenStreetMap",
  list.files(path_osm, pattern = "\\.(zip|gpkg)$", recursive = TRUE, full.names = TRUE)
)
nasa_date <- first_success(
  "NASA POWER",
  list.files(path_nasa, pattern = "\\.json$", full.names = TRUE)
)
worldpop_date <- first_success(
  "WorldPop population",
  list.files(path_worldpop, pattern = "\\.tif$", full.names = TRUE)
)

# ---- Population ----------------------------------------------------------------
population_source_cols <- grep("^population_[a-z]{3}$", names(population), value = TRUE)
esh_col <- "population_esh"
esh_units <- if (esh_col %in% names(population)) {
  population |> dplyr::filter(.data[[esh_col]] > 0)
} else {
  population[0, ]
}
pop_flagged <- population |> dplyr::filter(population_flag != "OK")

# ---- Priority ------------------------------------------------------------------
priority_top <- idx |>
  dplyr::arrange(dplyr::desc(priority_pop_weighted)) |>
  dplyr::slice_head(n = 10)
priority_regions <- priority_top |>
  dplyr::count(adm1_name, sort = TRUE) |>
  dplyr::filter(n >= 2)
priority_top_hot <- sum(
  priority_top$adm2_id %in% spatial$adm2_id[grepl("^Hot", spatial$gi_class_queen)]
)
priority_bottom_quintile <- idx |>
  dplyr::filter(priority_pop_weighted <= stats::quantile(priority_pop_weighted, 0.2, type = 8))
esh_in_bottom_quintile <- sum(esh_units$adm2_id %in% priority_bottom_quintile$adm2_id)

# ---- Distribution diagnostics ----------------------------------------------------
access_dist <- distribution |> dplyr::filter(domain == "technology")
climate_dist <- distribution |> dplyr::filter(domain == "climate")

# ---- Pairwise lookup -----------------------------------------------------------
get_pair <- function(tbl, score, a, b) {
  out <- tbl |>
    dplyr::filter(
      measure == score,
      (group_1 == a & group_2 == b) | (group_1 == b & group_2 == a)
    ) |>
    dplyr::slice_head(n = 1)
  if (nrow(out) == 0) {
    out <- tibble::tibble(measure = score, group_1 = a, group_2 = b,
                          p_adjusted = NA_real_, rank_biserial_r = NA_real_)
  }
  out
}
first_or_na <- function(x) if (length(x) == 0) NA_real_ else as.numeric(x[[1]])

# ---- Geographic environment sensitivity -----------------------------------------
geo_core <- "Urban core"
geo_adj <- "Adjacent to an urban core"
geo_non <- "Not adjacent to an urban core"
geo_n <- function(level) {
  v <- geo_counts$n[geo_counts$environment_geographic == level]
  if (length(v) == 0) 0L else as.integer(v)
}
geo_adj_from_peri <- geo_crosstab |>
  dplyr::filter(primary_environment == peri, environment_geographic == geo_adj) |>
  dplyr::pull(n)
geo_adj_from_peri <- if (length(geo_adj_from_peri) == 0) 0L else geo_adj_from_peri

geo_ce <- get_pair(geo_pairwise, "climate_exposure_score", geo_core, geo_adj)
geo_tg <- get_pair(geo_pairwise, "technology_gap_score", geo_core, geo_adj)
geo_ctvi <- get_pair(geo_pairwise, "ctvi_score", geo_core, geo_adj)
geo_adj_mean <- function(score) mean(idx[[score]][idx$environment_geographic == geo_adj])
geo_core_mean <- function(score) mean(idx[[score]][idx$environment_geographic == geo_core])

ce_same <- isTRUE(geo_ce$p_adjusted > spatial_alpha)
tg_larger <- isTRUE(geo_tg$p_adjusted <= spatial_alpha) &&
  isTRUE(geo_adj_mean("technology_gap_score") > geo_core_mean("technology_gap_score"))
geo_pattern_reproduced <- isTRUE(ce_same && tg_larger)

geo_sentence <- paste0(
  "Under the contiguity-based definition, ", geo_n(geo_adj),
  " units adjoined an urban core (", geo_adj_from_peri,
  " of them classed as peri-urban in the primary grouping). Their climate exposure ",
  if (ce_same) "did not differ significantly from" else "differed significantly from",
  " that of urban cores (mean ", f3(geo_adj_mean("climate_exposure_score")), " versus ",
  f3(geo_core_mean("climate_exposure_score")), "; ", fp_adj(geo_ce$p_adjusted),
  "), whereas their access gap was ",
  if (isTRUE(geo_tg$p_adjusted <= spatial_alpha)) {
    if (geo_adj_mean("technology_gap_score") > geo_core_mean("technology_gap_score")) "significantly larger" else "significantly smaller"
  } else {
    "not significantly different"
  },
  " (", f3(geo_adj_mean("technology_gap_score")), " versus ",
  f3(geo_core_mean("technology_gap_score")), "; ", fp_adj(geo_tg$p_adjusted),
  ", $r=", f3(geo_tg$rank_biserial_r), "$). ",
  if (geo_pattern_reproduced) {
    "The contrast between city fringes and urban cores therefore does not depend on the density-based definition of the peri-urban group."
  } else {
    "The contrast found with the density-based groups is therefore sensitive to how the urban fringe is defined."
  }
)

# ---- Spatial sensitivity ---------------------------------------------------------
sens_value <- function(id) first_or_na(spatial_sens$value[spatial_sens$check == id])
moran_q <- moran |> dplyr::filter(weights == "Queen contiguity")
moran_k <- moran |> dplyr::filter(weights != "Queen contiguity")

knn_sentence <- paste0(
  "With $k$-nearest-neighbour weights ($k=", sens_value("knn_k"), "$), Gi* identified ",
  sens_value("gi_hot_spots_knn"), " hot spots, ", sens_value("gi_hot_spots_both"),
  " of which were also hot spots under queen contiguity, and ",
  sens_value("gi_cold_spots_knn"), " cold spots (",
  sens_value("gi_cold_spots_both"), " shared)."
)
lisa_perm_n <- sens_value("lisa_significant_permutation_bh05")
lisa_perm_sentence <- if (lisa_perm_n == 0) {
  paste0(
    "A conditional permutation test (", fint(sens_value("lisa_permutations")),
    " permutations) gave the same result: no unit was significant after adjustment."
  )
} else {
  paste0(
    "With a conditional permutation test (", fint(sens_value("lisa_permutations")),
    " permutations), ", lisa_perm_n, " units were significant after adjustment."
  )
}

# ---- Sensitivity agreement -----------------------------------------------------
agree <- function(id, col) first_or_na(agreement[[col]][agreement$variant_id == id])

# ---- Write values.tex ----------------------------------------------------------
macro <- function(name, value) {
  paste0("\\newcommand{\\", name, "}{", value, "}")
}

values <- c(
  "% Generated by 07_manuscript_values.R. Do not edit by hand.",
  paste0("% Run completed: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
  macro("valOsmDate", osm_date),
  macro("valNasaDate", nasa_date),
  macro("valWorldPopDate", worldpop_date),
  macro("valPopTotalMillion", f1(sum(population$population) / 1e6)),
  macro("valPopFlaggedN", nrow(pop_flagged)),
  macro("valPopMin", fint(min(population$population))),
  macro("valPopMinName", min(population$display_name[population$population == min(population$population)])),
  macro("valEshUnitsN", nrow(esh_units)),
  macro("valEshUnitsWord", number_word(nrow(esh_units))),
  macro("valEshPopMin", fint(if (nrow(esh_units) > 0) min(esh_units$population) else NA)),
  macro("valEshPopMax", fint(if (nrow(esh_units) > 0) max(esh_units$population) else NA)),
  macro("valPriorityTopTen", latex_list(priority_top$display_name)),
  macro("valPriorityRegions", latex_list(region_tex(priority_regions$adm1_name))),
  macro("valPriorityTopHotN", priority_top_hot),
  macro("valEshBottomQuintileN", esh_in_bottom_quintile),
  macro("valPcaRho", f3(agree("pca", "spearman_rho"))),
  macro("valRobustRho", f3(agree("robust", "spearman_rho"))),
  macro("valLogRho", f3(agree("log_access", "spearman_rho"))),
  macro("valPcaTopTen", agree("pca", "top10_retained")),
  macro("valRobustTopTen", agree("robust", "top10_retained")),
  macro("valLogTopTen", agree("log_access", "top10_retained")),
  macro("valRobustTypAgree", agree("robust", "typology_agreement_n")),
  macro("valLogTypAgree", agree("log_access", "typology_agreement_n")),
  macro("valLogMaxRankChange", agree("log_access", "max_abs_rank_change")),
  macro("valAccessSkewMin", f1(min(access_dist$skewness))),
  macro("valAccessSkewMax", f1(max(access_dist$skewness))),
  macro("valAccessKurtMin", f1(min(access_dist$excess_kurtosis))),
  macro("valAccessKurtMax", f1(max(access_dist$excess_kurtosis))),
  macro("valAccessLogSkewMin", f1(min(access_dist$skewness_log1p))),
  macro("valAccessLogSkewMax", f1(max(access_dist$skewness_log1p))),
  macro("valAccessJrcN", sum(access_dist$jrc_treatment_suggested, na.rm = TRUE)),
  macro("valClimateSkewMaxAbs", f1(max(abs(climate_dist$skewness)))),
  macro("valGeoCoreN", geo_n(geo_core)),
  macro("valGeoAdjN", geo_n(geo_adj)),
  macro("valGeoNonAdjN", geo_n(geo_non)),
  macro("valGeoSentence", geo_sentence),
  macro("valMoranQueenI", f3(moran_q$moran_i)),
  macro("valMoranQueenP", fp(moran_q$p_value)),
  macro("valMoranKnnI", f3(moran_k$moran_i)),
  macro("valMoranKnnP", fp(moran_k$p_value)),
  macro("valKnnK", sens_value("knn_k")),
  macro("valKnnWord", number_word(sens_value("knn_k"))),
  macro("valKnnSentence", knn_sentence),
  macro("valLisaPermSentence", lisa_perm_sentence),
  macro("valLisaPermutations", fint(sens_value("lisa_permutations"))),
  macro("valRfRsq", f3(diagnostics$value[diagnostics$diagnostic == "rf_oob_r2"])),
  macro("valKmeansK", diagnostics$value[diagnostics$diagnostic == "kmeans_best_k"])
)
write_utf8(values, file.path(path_manuscript, "values.tex"))

# ---- Supplementary tables ----------------------------------------------------
typology_short_code <- c(
  "Critical vulnerability" = "CV",
  "Climate-exposed adaptive potential" = "CA",
  "Technology/service development gap" = "TD",
  "Relative resilience" = "RR"
)
geo_code <- c(
  "Urban core" = "Core", "Adjacent to an urban core" = "Adjacent",
  "Not adjacent to an urban core" = "Not adjacent"
)
gi_short <- function(x) {
  dplyr::case_when(
    grepl("^Hot.*0.05", x) ~ "Hot (0.05)",
    grepl("^Hot.*0.10", x) ~ "Hot (0.10)",
    grepl("^Cold.*0.05", x) ~ "Cold (0.05)",
    grepl("^Cold.*0.10", x) ~ "Cold (0.10)",
    TRUE ~ "n.s."
  )
}

table_s1 <- idx |>
  dplyr::left_join(
    spatial |> dplyr::select(adm2_id, gi_class_queen),
    by = "adm2_id"
  ) |>
  dplyr::arrange(ctvi_rank) |>
  dplyr::transmute(
    rank = ctvi_rank,
    adm2_id,
    unit = display_name,
    unit_type,
    region = adm1_name,
    environment = unname(.env$environment_short_labels[environment_type]),
    environment_basis = environment_classification_basis,
    environment_contiguity = unname(.env$geo_code[as.character(environment_geographic)]),
    population = round(population),
    climate_exposure = round(climate_exposure_score, 3),
    access_gap = round(technology_gap_score, 3),
    ctvi = round(ctvi_score, 3),
    typology = unname(.env$typology_short_code[as.character(resilience_typology)]),
    gi_star = gi_short(gi_class_queen),
    priority = round(priority_pop_weighted, 3)
  )

table_s2 <- population |>
  dplyr::left_join(idx |> dplyr::select(adm2_id, adm1_name), by = "adm2_id") |>
  dplyr::arrange(dplyr::desc(.data[[esh_col]] > 0), display_name) |>
  dplyr::transmute(
    adm2_id,
    unit = display_name,
    region = adm1_name,
    area_km2 = round(area_km2),
    population_mar_raster = round(population_mar),
    population_esh_raster = round(.data[[esh_col]]),
    population_total = round(population),
    density_per_km2 = round(population_density_per_km2, 1),
    flag = population_flag
  )

table_s3_agreement <- agreement |>
  dplyr::transmute(
    check = variant,
    spearman_rho = round(spearman_rho, 3),
    top10_retained,
    typology_agreement = ifelse(is.na(typology_agreement_n), "",
                                paste0(typology_agreement_n, "/", units)),
    max_abs_rank_change
  )
table_s3_distribution <- distribution |>
  dplyr::transmute(
    indicator = label, domain,
    skewness = round(skewness, 2),
    excess_kurtosis = round(excess_kurtosis, 2),
    skewness_after_log1p = round(skewness_log1p, 2),
    jrc_treatment_suggested
  )
table_s3_geo <- geo_pairwise |>
  dplyr::transmute(
    measure = dplyr::recode(
      measure,
      climate_exposure_score = "Climate exposure",
      technology_gap_score = "Access gap",
      ctvi_score = "CTVI"
    ),
    comparison = paste(group_1, "vs", tolower(group_2)),
    p_adjusted = signif(p_adjusted, 3),
    rank_biserial_r = round(rank_biserial_r, 3)
  )
table_s3_spatial <- dplyr::bind_rows(
  moran |> dplyr::transmute(
    check = paste0("Global Moran's I (", weights, ")"),
    value = round(moran_i, 3),
    p_value = signif(p_value, 3)
  ),
  spatial_sens |> dplyr::transmute(check, value, p_value = NA_real_)
)

safe_write_csv(table_s1, file.path(path_manuscript, "Table_S1_adm2_results.csv"))
safe_write_csv(table_s2, file.path(path_manuscript, "Table_S2_population_extraction.csv"))
safe_write_csv(table_s3_agreement, file.path(path_manuscript, "Table_S3a_rank_agreement.csv"))
safe_write_csv(table_s3_distribution, file.path(path_manuscript, "Table_S3b_indicator_distributions.csv"))
safe_write_csv(table_s3_geo, file.path(path_manuscript, "Table_S3c_contiguity_environment_tests.csv"))
safe_write_csv(table_s3_spatial, file.path(path_manuscript, "Table_S3d_spatial_checks.csv"))

tex_rows <- function(df) {
  apply(df, 1, function(r) paste(latex_escape(trimws(r)), collapse = " & ")) |>
    paste0(" \\\\")
}

s1_tex <- table_s1 |>
  dplyr::transmute(
    rank, unit, region = region_tex(region), environment,
    environment_contiguity,
    population = fint(population),
    climate_exposure = f3(climate_exposure), access_gap = f3(access_gap), ctvi = f3(ctvi),
    typology, gi_star, priority = f3(priority)
  )
s2_tex <- table_s2 |>
  dplyr::transmute(
    unit, region = region_tex(region), area_km2 = fint(area_km2),
    population_mar_raster = fint(population_mar_raster),
    population_esh_raster = fint(population_esh_raster),
    population_total = fint(population_total),
    density_per_km2 = f1(density_per_km2),
    flag = ifelse(flag == "OK", "--", flag)
  )

supp <- c(
  "\\documentclass[10pt]{article}",
  "\\usepackage[T1]{fontenc}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage{lmodern}",
  "\\usepackage[a4paper,landscape,margin=1.6cm]{geometry}",
  "\\usepackage{booktabs,longtable,array}",
  "\\usepackage[hidelinks]{hyperref}",
  "\\renewcommand{\\arraystretch}{1.05}",
  "\\begin{document}",
  "\\begin{center}{\\large\\bfseries Supplementary material}\\\\[4pt]",
  "Mapping Climate--Technology Vulnerability across Morocco: An Open-Data Provincial Index, Spatial Typologies, and Urban--Rural Inequalities\\end{center}",
  "",
  "\\noindent All tables are generated by \\texttt{07\\_manuscript\\_values.R}; CSV versions are archived with the code and data.",
  "",
  "\\scriptsize",
  "\\begin{longtable}{@{}r>{\\raggedright\\arraybackslash}p{2.9cm}>{\\raggedright\\arraybackslash}p{3.3cm}llrrrrllr@{}}",
  "\\caption*{\\normalsize\\textbf{Supplementary Table S1.} ADM2 results and analytical environment lookup, ordered by CTVI rank. Environment: primary (name anchors plus population-density percentiles); Contiguity: geographic check (Section 3.10). Typology: CV, Critical vulnerability; CA, Climate-exposed adaptive potential; TD, Technology/service development gap; RR, Relative resilience. Gi*: Benjamini--Hochberg-adjusted class (n.s., not significant). Population: WorldPop 2020, 1 km, UN-adjusted (MAR and ESH rasters).}\\\\",
  "\\toprule",
  "Rank & Unit & Region & Environment & Contiguity & Population & CE & TG & CTVI & Typ. & Gi* & Priority \\\\",
  "\\midrule\\endfirsthead",
  "\\toprule",
  "Rank & Unit & Region & Environment & Contiguity & Population & CE & TG & CTVI & Typ. & Gi* & Priority \\\\",
  "\\midrule\\endhead",
  tex_rows(s1_tex),
  "\\bottomrule",
  "\\end{longtable}",
  "",
  "\\begin{longtable}{@{}>{\\raggedright\\arraybackslash}p{3.0cm}>{\\raggedright\\arraybackslash}p{3.6cm}rrrrrl@{}}",
  "\\caption*{\\normalsize\\textbf{Supplementary Table S2.} Population extraction by WorldPop source raster. WorldPop distributes the area south of 27$^{\\circ}$40$'$N as a separate raster (ISO code ESH); both rasters were summed for each unit. Units are listed with ESH contributions first. Flag: totals of zero or below 1,000 people would indicate incomplete coverage.}\\\\",
  "\\toprule",
  "Unit & Region & Area (km$^2$) & MAR raster & ESH raster & Total & Density (km$^{-2}$) & Flag \\\\",
  "\\midrule\\endfirsthead",
  "\\toprule",
  "Unit & Region & Area (km$^2$) & MAR raster & ESH raster & Total & Density (km$^{-2}$) & Flag \\\\",
  "\\midrule\\endhead",
  tex_rows(s2_tex),
  "\\bottomrule",
  "\\end{longtable}",
  "",
  "\\begin{longtable}{@{}>{\\raggedright\\arraybackslash}p{6.5cm}rrll@{}}",
  "\\caption*{\\normalsize\\textbf{Supplementary Table S3a.} Agreement of alternative index constructions with the primary equal-weight CTVI (75 units).}\\\\",
  "\\toprule",
  "Variant & Spearman $\\rho$ & Top-10 retained & Typology agreement & Max.\\ rank change \\\\",
  "\\midrule",
  tex_rows(table_s3_agreement |> dplyr::mutate(spearman_rho = f3(spearman_rho))),
  "\\bottomrule",
  "\\end{longtable}",
  "",
  "\\begin{longtable}{@{}>{\\raggedright\\arraybackslash}p{6.5cm}lrrrl@{}}",
  "\\caption*{\\normalsize\\textbf{Supplementary Table S3b.} Distribution of the raw indicators. Sample skewness and excess kurtosis use small-sample corrections; JRC-COIN guidance suggests treatment when absolute skewness exceeds 2 and kurtosis exceeds 3.5.}\\\\",
  "\\toprule",
  "Indicator & Domain & Skewness & Excess kurtosis & Skewness after $\\log(1+x)$ & Treatment suggested \\\\",
  "\\midrule",
  tex_rows(table_s3_distribution |> dplyr::mutate(
    skewness = f2(skewness), excess_kurtosis = f2(excess_kurtosis),
    skewness_after_log1p = ifelse(is.na(skewness_after_log1p), "--", f2(skewness_after_log1p)),
    jrc_treatment_suggested = ifelse(jrc_treatment_suggested, "Yes", "No")
  )),
  "\\bottomrule",
  "\\end{longtable}",
  "",
  "\\begin{longtable}{@{}l>{\\raggedright\\arraybackslash}p{9.5cm}rr@{}}",
  "\\caption*{\\normalsize\\textbf{Supplementary Table S3c.} Contiguity-based environment check: pairwise Mann--Whitney tests with Benjamini--Hochberg adjustment and rank-biserial effect sizes.}\\\\",
  "\\toprule",
  "Measure & Comparison & Adjusted $p$ & $r$ \\\\",
  "\\midrule",
  tex_rows(table_s3_geo |> dplyr::mutate(
    p_adjusted = ifelse(p_adjusted < 0.001, "$<$0.001", f3(p_adjusted)),
    rank_biserial_r = f3(rank_biserial_r)
  )),
  "\\bottomrule",
  "\\end{longtable}",
  "",
  "\\begin{longtable}{@{}>{\\raggedright\\arraybackslash}p{10cm}rr@{}}",
  "\\caption*{\\normalsize\\textbf{Supplementary Table S3d.} Global spatial autocorrelation and local-statistic sensitivity checks. Counts refer to units significant at Benjamini--Hochberg-adjusted $p\\le0.10$ for Gi* and $p\\le0.05$ for Local Moran's I.}\\\\",
  "\\toprule",
  "Check & Value & $p$ \\\\",
  "\\midrule",
  tex_rows(table_s3_spatial |> dplyr::mutate(
    check = gsub("_", " ", check),
    value = ifelse(value == round(value), fint(value), f3(value)),
    p_value = ifelse(is.na(p_value), "--", ifelse(p_value < 0.001, "$<$0.001", f3(p_value)))
  )),
  "\\bottomrule",
  "\\end{longtable}",
  "\\end{document}"
)
write_utf8(supp, file.path(path_manuscript, "Supplementary_material.tex"))

# ---- Consistency report against the manuscript text ---------------------------
summ <- function(group_var, group, score, fun) {
  fun(idx[[score]][as.character(idx[[group_var]]) == group])
}
type_n <- function(t) sum(idx$resilience_typology == t)
env_n <- function(e) sum(idx$environment_type == e)
comp_n <- function(e, t) sum(idx$environment_type == e & idx$resilience_typology == t)
kw_h <- function(score) first_or_na(kruskal$statistic[kruskal$measure == score])
pw <- function(score, a, b, col) first_or_na(get_pair(pairwise, score, a, b)[[col]])
gi_hot <- spatial$display_name[grepl("^Hot", spatial$gi_class_queen)]
gi_hot05 <- spatial$display_name[grepl("^Hot.*0.05", spatial$gi_class_queen)]
gi_cold <- spatial$display_name[grepl("^Cold", spatial$gi_class_queen)]
gi_cold05 <- spatial$display_name[grepl("^Cold.*0.05", spatial$gi_class_queen)]
q_breaks <- function(x) as.numeric(stats::quantile(x, probs = seq(0, 1, 0.2), type = 8))
top5 <- idx |> dplyr::arrange(ctvi_rank) |> dplyr::slice_head(n = 5) |> dplyr::pull(display_name)
rf_order <- rf_importance |> dplyr::arrange(dplyr::desc(`%IncMSE`)) |> dplyr::pull(variable)

expected_numeric <- tibble::tribble(
  ~check, ~location, ~expected, ~observed, ~tolerance,
  "Units", "Throughout", 75, nrow(idx), 0,
  "Critical vulnerability n", "Sec 4.3, Table 3, abstract", 27, type_n("Critical vulnerability"), 0,
  "Climate-exposed adaptive potential n", "Sec 4.3, Table 3", 11, type_n("Climate-exposed adaptive potential"), 0,
  "Technology/service development gap n", "Sec 4.3, Table 3, abstract", 11, type_n("Technology/service development gap"), 0,
  "Relative resilience n", "Sec 4.3, Table 3", 26, type_n("Relative resilience"), 0,
  "Critical CE mean", "Table 3", 0.626, summ("resilience_typology", "Critical vulnerability", "climate_exposure_score", mean), 0.0015,
  "Critical TG mean", "Table 3", 0.994, summ("resilience_typology", "Critical vulnerability", "technology_gap_score", mean), 0.0015,
  "Critical CTVI mean", "Table 3", 0.810, summ("resilience_typology", "Critical vulnerability", "ctvi_score", mean), 0.0015,
  "Climate-exposed CE mean", "Table 3", 0.512, summ("resilience_typology", "Climate-exposed adaptive potential", "climate_exposure_score", mean), 0.0015,
  "Climate-exposed TG mean", "Table 3", 0.958, summ("resilience_typology", "Climate-exposed adaptive potential", "technology_gap_score", mean), 0.0015,
  "Climate-exposed TG SD", "Table 3", 0.039, summ("resilience_typology", "Climate-exposed adaptive potential", "technology_gap_score", stats::sd), 0.0015,
  "Tech gap CE mean", "Table 3", 0.268, summ("resilience_typology", "Technology/service development gap", "climate_exposure_score", mean), 0.0015,
  "Tech gap TG mean", "Table 3", 0.989, summ("resilience_typology", "Technology/service development gap", "technology_gap_score", mean), 0.0015,
  "Relative resilience CE mean", "Table 3", 0.286, summ("resilience_typology", "Relative resilience", "climate_exposure_score", mean), 0.0015,
  "Relative resilience TG mean", "Table 3", 0.860, summ("resilience_typology", "Relative resilience", "technology_gap_score", mean), 0.0015,
  "Relative resilience TG SD", "Table 3", 0.254, summ("resilience_typology", "Relative resilience", "technology_gap_score", stats::sd), 0.0015,
  "Urban core n", "Sec 3.6, Table 4", 11, env_n(core), 0,
  "Peri-urban n", "Sec 3.6, Table 4", 34, env_n(peri), 0,
  "Rural n", "Sec 3.6, Table 4", 30, env_n(rural), 0,
  "Urban core CE mean", "Table 4, Sec 4.4, 5.2", 0.314, summ("environment_type", core, "climate_exposure_score", mean), 0.0015,
  "Peri-urban CE mean", "Table 4, Sec 4.4, 5.2", 0.359, summ("environment_type", peri, "climate_exposure_score", mean), 0.0015,
  "Rural CE mean", "Table 4", 0.576, summ("environment_type", rural, "climate_exposure_score", mean), 0.0015,
  "Urban core TG mean", "Table 4, Sec 5.2", 0.705, summ("environment_type", core, "technology_gap_score", mean), 0.0015,
  "Peri-urban TG mean", "Table 4, Sec 5.2", 0.972, summ("environment_type", peri, "technology_gap_score", mean), 0.0015,
  "Rural TG mean", "Table 4", 0.994, summ("environment_type", rural, "technology_gap_score", mean), 0.0015,
  "Urban core CTVI mean", "Table 4, abstract", 0.510, summ("environment_type", core, "ctvi_score", mean), 0.0015,
  "Peri-urban CTVI mean", "Table 4, abstract", 0.665, summ("environment_type", peri, "ctvi_score", mean), 0.0015,
  "Rural CTVI mean", "Table 4, abstract", 0.785, summ("environment_type", rural, "ctvi_score", mean), 0.0015,
  "Urban core CTVI median", "Table 4", 0.561, summ("environment_type", core, "ctvi_score", stats::median), 0.0015,
  "Peri-urban CTVI median", "Table 4", 0.655, summ("environment_type", peri, "ctvi_score", stats::median), 0.0015,
  "Rural CTVI median", "Table 4", 0.778, summ("environment_type", rural, "ctvi_score", stats::median), 0.0015,
  "Urban core TG SD", "Table 4", 0.340, summ("environment_type", core, "technology_gap_score", stats::sd), 0.0015,
  "Kruskal-Wallis H, CE", "Sec 4.4", 22.73, kw_h("climate_exposure_score"), 0.006,
  "Kruskal-Wallis H, TG", "Sec 4.4", 56.38, kw_h("technology_gap_score"), 0.006,
  "Kruskal-Wallis H, CTVI", "Sec 4.4, abstract", 30.95, kw_h("ctvi_score"), 0.006,
  "CE rural vs peri r", "Sec 4.4", 0.622, pw("climate_exposure_score", rural, peri, "rank_biserial_r"), 0.0015,
  "CE rural vs core r", "Sec 4.4", 0.691, pw("climate_exposure_score", rural, core, "rank_biserial_r"), 0.0015,
  "CE rural vs core adjusted p", "Sec 4.4", 0.001, pw("climate_exposure_score", rural, core, "p_adjusted"), 0.0005,
  "CE peri vs core adjusted p", "Sec 4.4, 5.2", 0.157, pw("climate_exposure_score", peri, core, "p_adjusted"), 0.0015,
  "TG rural vs core r", "Sec 4.4", 1.000, pw("technology_gap_score", rural, core, "rank_biserial_r"), 0.0015,
  "CTVI peri vs core adjusted p", "Sec 4.4, 5.2", 0.006, pw("ctvi_score", peri, core, "p_adjusted"), 0.0015,
  "CTVI peri vs core r", "Sec 4.4", 0.556, pw("ctvi_score", peri, core, "rank_biserial_r"), 0.0015,
  "Urban core: Relative resilience n", "Sec 4.4", 9, comp_n(core, "Relative resilience"), 0,
  "Urban core: Climate-exposed n", "Sec 4.4", 2, comp_n(core, "Climate-exposed adaptive potential"), 0,
  "Peri-urban: Relative resilience n", "Sec 4.4", 17, comp_n(peri, "Relative resilience"), 0,
  "Peri-urban: Climate-exposed n", "Sec 4.4", 8, comp_n(peri, "Climate-exposed adaptive potential"), 0,
  "Peri-urban: Tech gap n", "Sec 4.4", 6, comp_n(peri, "Technology/service development gap"), 0,
  "Peri-urban: Critical n", "Sec 4.4", 3, comp_n(peri, "Critical vulnerability"), 0,
  "Rural: Critical n", "Sec 4.4, abstract, highlights", 24, comp_n(rural, "Critical vulnerability"), 0,
  "Rural: Tech gap n", "Sec 4.4", 5, comp_n(rural, "Technology/service development gap"), 0,
  "CTVI minimum", "Sec 4.1", 0.182, min(idx$ctvi_score), 0.0015,
  "CTVI maximum", "Sec 4.1", 1.000, max(idx$ctvi_score), 0.0015,
  "CE top quintile lower bound", "Sec 4.2", 0.576, q_breaks(idx$climate_exposure_score)[5], 0.0015,
  "TG minimum", "Sec 4.2", 0.093, min(idx$technology_gap_score), 0.0015,
  "TG lowest quintile upper bound", "Sec 4.2", 0.961, q_breaks(idx$technology_gap_score)[2], 0.0015,
  "Gi* hot spots", "Sec 4.5, abstract, highlights", 12, length(gi_hot), 0,
  "Gi* hot spots at adj. p <= 0.05", "Sec 4.5", 6, length(gi_hot05), 0,
  "Gi* cold spots", "Sec 4.5", 6, length(gi_cold), 0,
  "Gi* cold spots at adj. p <= 0.05", "Sec 4.5", 4, length(gi_cold05), 0,
  "Local Moran significant (BH 0.05)", "Sec 4.5", 0, sens_value("lisa_significant_analytical_bh05"), 0,
  "Spearman rho, PCA", "Sec 4.7, abstract", 0.987, agree("pca", "spearman_rho"), 0.0015,
  "Spearman rho, robust", "Sec 4.7, abstract", 0.936, agree("robust", "spearman_rho"), 0.0015,
  "Top-10 retained, PCA", "Sec 4.7", 10, agree("pca", "top10_retained"), 0,
  "Top-10 retained, robust", "Sec 4.7", 10, agree("robust", "top10_retained"), 0,
  "k-means k", "Sec 4.7", 3, diagnostics$value[diagnostics$diagnostic == "kmeans_best_k"], 0,
  "k-means smallest cluster n", "Sec 4.7", 3, min(clusters$n), 0,
  "k-means largest cluster n", "Sec 4.7", 42, max(clusters$n), 0,
  "Random Forest OOB R2", "Sec 4.7", 0.938, diagnostics$value[diagnostics$diagnostic == "rf_oob_r2"], 0.0015
) |>
  dplyr::mutate(
    difference = observed - expected,
    status = dplyr::if_else(
      is.finite(difference) & abs(difference) <= tolerance + 1e-9, "PASS", "CHECK"
    )
  )

set_check <- function(check, location, expected, observed) {
  tibble::tibble(
    check = check, location = location,
    expected_text = paste(sort(expected), collapse = "; "),
    observed_text = paste(sort(observed), collapse = "; "),
    status = if (setequal(expected, observed)) "PASS" else "CHECK"
  )
}
expected_sets <- dplyr::bind_rows(
  set_check("Top five CTVI units", "Sec 4.1",
            c("Aousserd", "Oued Ed-Dahab", "Errachidia", "Assa-Zag", "Tata"), top5),
  set_check("Gi* hot spot units", "Sec 4.5",
            c("Aousserd", "Oued Ed-Dahab", "Boujdour", "Laayoune", "Es-Semara", "Tarfaya",
              "Guelmim", "Assa-Zag", "Tata", "Taroudant", "Zagora", "Errachidia"), gi_hot),
  set_check("Gi* cold spot units", "Sec 4.5",
            c("Casablanca", "Mohammedia", "Nouaceur", "Mediouna", "Rabat", "Tétouan"), gi_cold),
  set_check("Zagora is a hot spot at adj. p <= 0.05", "Sec 5.1", "Zagora",
            intersect("Zagora", gi_hot05)),
  set_check("Random Forest importance order", "Sec 4.7",
            c("1 precipitation_total_mm_5yr", "2 dry_days_lt_1mm_5yr", "3 heat_days_gt_35c_5yr"),
            paste(1:3, head(rf_order, 3))),
  set_check("New-analysis narrative: fringe contrast reproduced under contiguity", "Sec 4.4, 5.2",
            "TRUE", as.character(geo_pattern_reproduced)),
  set_check("No population flags after combining rasters", "Sec 3.3, 4.6",
            "0", as.character(nrow(pop_flagged)))
)

consistency <- dplyr::bind_rows(
  expected_numeric |>
    dplyr::transmute(check, location,
                     expected_text = as.character(expected),
                     observed_text = formatC(observed, format = "f", digits = 3),
                     status),
  expected_sets
)
safe_write_csv(consistency, file.path(path_manuscript, "consistency_report.csv"))

n_check <- sum(consistency$status == "CHECK")
if (n_check == 0) {
  message("Consistency report: all ", nrow(consistency),
          " manuscript checks PASS. values.tex and supplementary tables are ready.")
} else {
  warning(
    n_check, " manuscript check(s) differ from this run. See ",
    file.path(path_manuscript, "consistency_report.csv"),
    ". Update the manuscript text for: ",
    paste(consistency$check[consistency$status == "CHECK"], collapse = "; ")
  )
}
