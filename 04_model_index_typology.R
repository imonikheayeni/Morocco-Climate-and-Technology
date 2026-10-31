# =============================================================================
# 04_model_index_typology_revised.R
# Constructs the primary transparent CTVI, sensitivity indices, four resilience
# typologies, smart-city/urban-edge/rural comparisons and publication outputs.
#
# Primary method:
#   * direction-align and min-max scale selected indicators;
#   * equal weight within climate and technology domains;
#   * equal weight for the two domain scores.
#
# Sensitivity analyses:
#   * robust 5th-95th percentile scaling;
#   * PCA-derived index;
#   * data-driven 3-4 cluster solution;
#   * Random Forest index-reconstruction sensitivity diagnostic.
# =============================================================================

feature_file <- file.path(path_ml, "morocco_ctvi_adm2_features_imputed.csv")
adm2_file <- file.path(path_int_spatial, "morocco_adm2.gpkg")
stopifnot(file.exists(feature_file), file.exists(adm2_file))

ml <- readr::read_csv(feature_file, show_col_types = FALSE)
adm2_map <- sf::st_read(adm2_file, quiet = TRUE)

ml <- ml |>
  dplyr::mutate(
    environment_type = factor(
      as.character(environment_type),
      levels = allowed_environment_types
    )
  )

# ---- 1. Explicit indicator QC and selection --------------------------------
climate_candidates <- unique(c(
  preferred_climate_features,
  climate_fallback_features
))
technology_candidates <- preferred_technology_features

climate_qc <- feature_qc_table(ml, climate_candidates, "climate")
technology_qc <- feature_qc_table(ml, technology_candidates, "technology")
model_qc <- dplyr::bind_rows(climate_qc, technology_qc)
safe_write_csv(model_qc, file.path(path_logs, "model_feature_qc_final.csv"))

preferred_climate_usable <- climate_qc |>
  dplyr::filter(variable %in% preferred_climate_features, usable) |>
  dplyr::pull(variable)

fallback_climate_usable <- climate_qc |>
  dplyr::filter(variable %in% climate_fallback_features, usable) |>
  dplyr::pull(variable)

climate_vars <- unique(c(preferred_climate_usable, fallback_climate_usable))
climate_vars <- climate_vars[seq_len(min(
  length(climate_vars),
  max(min_valid_climate_features, length(preferred_climate_features))
))]

technology_vars <- technology_qc |>
  dplyr::filter(usable) |>
  dplyr::pull(variable)

if (length(climate_vars) < min_valid_climate_features) {
  stop(
    "Only ", length(climate_vars),
    " usable climate indicators were found; ",
    min_valid_climate_features, " are required.",
    call. = FALSE
  )
}
if (length(technology_vars) < min_valid_technology_features) {
  stop(
    "Only ", length(technology_vars),
    " usable technology/service-access indicators were found; ",
    min_valid_technology_features, " are required.",
    call. = FALSE
  )
}

model_vars <- c(climate_vars, technology_vars)
message("Climate indicators: ", paste(climate_vars, collapse = ", "))
message("Technology indicators: ", paste(technology_vars, collapse = ", "))

# Population density is explicitly prohibited from the technology-gap domain.
if ("population_density_per_km2" %in% technology_vars) {
  stop("Population density must not be used as a technology-gap indicator.")
}

# Correlation diagnostics: report high redundancy rather than hiding it.
correlation_matrix <- stats::cor(
  ml[, model_vars],
  use = "pairwise.complete.obs"
)
safe_write_csv(
  tibble::as_tibble(correlation_matrix, rownames = "variable"),
  file.path(path_logs, "selected_indicator_correlations.csv")
)

high_correlation_pairs <- as.data.frame(as.table(correlation_matrix)) |>
  tibble::as_tibble() |>
  dplyr::rename(variable_1 = Var1, variable_2 = Var2, correlation = Freq) |>
  dplyr::mutate(
    variable_1 = as.character(variable_1),
    variable_2 = as.character(variable_2)
  ) |>
  dplyr::filter(
    variable_1 < variable_2,
    abs(correlation) >= 0.90
  ) |>
  dplyr::arrange(dplyr::desc(abs(correlation)))
safe_write_csv(
  high_correlation_pairs,
  file.path(path_logs, "high_indicator_correlations_ge_0_90.csv")
)
if (nrow(high_correlation_pairs) > 0) {
  warning(
    "At least one selected indicator pair has |r| >= 0.90. Review the correlation report."
  )
}

# ---- 2. Direction alignment and scaling ------------------------------------
aligned <- ml |>
  dplyr::select(
    adm2_id,
    adm2_name,
    adm1_name,
    environment_type,
    environment_classification_basis,
    environment_classification_notes,
    population,
    population_density_per_km2,
    dplyr::all_of(model_vars)
  )

aligned_robust <- aligned
for (variable in model_vars) {
  direction <- indicator_direction[[variable]]
  if (is.null(direction) || is.na(direction)) {
    stop("No direction was defined for indicator: ", variable)
  }
  aligned[[variable]] <- scale_01(direction * ml[[variable]])
  aligned_robust[[variable]] <- robust_scale_01(direction * ml[[variable]])
}

if (any(vapply(aligned[model_vars], function(x) all(is.na(x)), logical(1)))) {
  stop("At least one aligned indicator became entirely missing.", call. = FALSE)
}

X <- as.matrix(aligned[, model_vars])
X_robust <- as.matrix(aligned_robust[, model_vars])

# ---- 3. Primary equal-weight CTVI ------------------------------------------
climate_exposure_score <- rowMeans(
  X[, climate_vars, drop = FALSE],
  na.rm = TRUE
)
technology_gap_score <- rowMeans(
  X[, technology_vars, drop = FALSE],
  na.rm = TRUE
)
ctvi_equal_weight <- rowMeans(
  cbind(climate_exposure_score, technology_gap_score),
  na.rm = TRUE
)

climate_exposure_robust <- rowMeans(
  X_robust[, climate_vars, drop = FALSE],
  na.rm = TRUE
)
technology_gap_robust <- rowMeans(
  X_robust[, technology_vars, drop = FALSE],
  na.rm = TRUE
)
ctvi_robust_sensitivity <- rowMeans(
  cbind(climate_exposure_robust, technology_gap_robust),
  na.rm = TRUE
)

# ---- 4. PCA sensitivity index ----------------------------------------------
pca <- stats::prcomp(X, center = TRUE, scale. = TRUE)
pca_variance <- summary(pca)$importance[2, ]
pca_pc1 <- pca$x[, 1]

if (stats::cor(pca_pc1, ctvi_equal_weight, use = "complete.obs") < 0) {
  pca_pc1 <- -pca_pc1
  pca$rotation[, 1] <- -pca$rotation[, 1]
}
ctvi_pca_sensitivity <- scale_01(pca_pc1)

safe_write_csv(
  tibble::tibble(
    component = names(pca_variance),
    variance_explained = as.numeric(pca_variance),
    cumulative_variance = cumsum(as.numeric(pca_variance))
  ),
  file.path(path_ml, "pca_variance_explained.csv")
)
safe_write_csv(
  tibble::as_tibble(pca$rotation, rownames = "variable"),
  file.path(path_ml, "pca_loadings.csv")
)

# ---- 5. Four interpretable resilience/vulnerability typologies -------------
climate_threshold <- switch(
  typology_threshold_method,
  median = stats::median(climate_exposure_score, na.rm = TRUE),
  stop("Unsupported typology_threshold_method: ", typology_threshold_method)
)
technology_threshold <- switch(
  typology_threshold_method,
  median = stats::median(technology_gap_score, na.rm = TRUE),
  stop("Unsupported typology_threshold_method: ", typology_threshold_method)
)

typology_levels <- c(
  "Critical vulnerability",
  "Climate-exposed adaptive potential",
  "Technology/service development gap",
  "Relative resilience"
)

typology_interpretation_lookup <- c(
  "Critical vulnerability" =
    "High climate exposure and high technology/service-access gap; highest adaptation priority.",
  "Climate-exposed adaptive potential" =
    "High climate exposure but comparatively lower technology gap; adaptation can build on existing access.",
  "Technology/service development gap" =
    "Lower climate exposure but comparatively high technology/service-access gap; infrastructure strengthening is central.",
  "Relative resilience" =
    "Lower climate exposure and lower technology/service-access gap relative to other Moroccan ADM2 areas."
)

index_table <- aligned |>
  dplyr::select(
    adm2_id,
    adm2_name,
    adm1_name,
    environment_type,
    environment_classification_basis,
    environment_classification_notes,
    population,
    population_density_per_km2
  ) |>
  dplyr::mutate(
    climate_exposure_score = climate_exposure_score,
    technology_gap_score = technology_gap_score,
    ctvi_score = ctvi_equal_weight,
    ctvi_equal_weight = ctvi_equal_weight,
    ctvi_robust_sensitivity = ctvi_robust_sensitivity,
    ctvi_pca_sensitivity = ctvi_pca_sensitivity,
    high_climate_exposure = climate_exposure_score >= climate_threshold,
    high_technology_gap = technology_gap_score >= technology_threshold,
    resilience_typology = dplyr::case_when(
      high_climate_exposure & high_technology_gap ~
        "Critical vulnerability",
      high_climate_exposure & !high_technology_gap ~
        "Climate-exposed adaptive potential",
      !high_climate_exposure & high_technology_gap ~
        "Technology/service development gap",
      TRUE ~ "Relative resilience"
    ),
    typology_code = dplyr::case_when(
      high_climate_exposure & high_technology_gap ~ "HH",
      high_climate_exposure & !high_technology_gap ~ "HL",
      !high_climate_exposure & high_technology_gap ~ "LH",
      TRUE ~ "LL"
    ),
    resilience_typology = factor(
      resilience_typology,
      levels = typology_levels
    ),
    typology_interpretation = unname(
      typology_interpretation_lookup[as.character(resilience_typology)]
    ),
    ctvi_rank = dplyr::min_rank(dplyr::desc(ctvi_score)),
    pca_rank = dplyr::min_rank(dplyr::desc(ctvi_pca_sensitivity)),
    robust_rank = dplyr::min_rank(dplyr::desc(ctvi_robust_sensitivity)),
    pca_rank_change = pca_rank - ctvi_rank,
    robust_rank_change = robust_rank - ctvi_rank,
    # Relative priority, not an estimate of people actually affected.
    population_scaled_log = if (all(is.finite(population)) && all(population > 0)) scale_01(log1p(population)) else rep(NA_real_, dplyr::n()),
    priority_pop_weighted = ctvi_score * population_scaled_log
  )

# ---- 6. Data-driven 3-4 cluster sensitivity analysis -----------------------
set.seed(random_seed)
X_standardised <- scale(X)

candidate_k <- 3:min(4, nrow(X_standardised) - 1)
if (length(candidate_k) == 0) {
  stop("Insufficient ADM2 areas for 3-4 cluster sensitivity analysis.")
}

silhouette_score <- function(k) {
  set.seed(random_seed)
  fit <- stats::kmeans(X_standardised, centers = k, nstart = 50)
  mean(cluster::silhouette(
    fit$cluster,
    stats::dist(X_standardised)
  )[, 3])
}

silhouette_results <- tibble::tibble(
  k = candidate_k,
  average_silhouette = vapply(candidate_k, silhouette_score, numeric(1))
)
best_k <- silhouette_results$k[
  which.max(silhouette_results$average_silhouette)
]

set.seed(random_seed)
kmeans_fit <- stats::kmeans(
  X_standardised,
  centers = best_k,
  nstart = 100
)
index_table$kmeans_cluster <- kmeans_fit$cluster

hierarchical_fit <- stats::hclust(
  stats::dist(X_standardised),
  method = "ward.D2"
)
index_table$hclust_cluster <- stats::cutree(
  hierarchical_fit,
  k = best_k
)

cluster_order <- index_table |>
  dplyr::group_by(kmeans_cluster) |>
  dplyr::summarise(
    mean_ctvi = mean(ctvi_score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(mean_ctvi) |>
  dplyr::mutate(cluster_rank = dplyr::row_number())

cluster_labels <- if (best_k == 3) {
  c("Lower", "Moderate", "Higher")
} else {
  c("Lower", "Moderate", "High", "Higher")
}
cluster_order$data_driven_typology <- paste(
  cluster_labels[cluster_order$cluster_rank],
  "data-driven vulnerability"
)

index_table <- index_table |>
  dplyr::left_join(
    cluster_order |>
      dplyr::select(kmeans_cluster, data_driven_typology),
    by = "kmeans_cluster"
  )

cluster_profiles <- index_table |>
  dplyr::group_by(kmeans_cluster, data_driven_typology) |>
  dplyr::summarise(
    n = dplyr::n(),
    climate_exposure_mean = mean(climate_exposure_score, na.rm = TRUE),
    technology_gap_mean = mean(technology_gap_score, na.rm = TRUE),
    ctvi_mean = mean(ctvi_score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(ctvi_mean)

safe_write_csv(
  silhouette_results,
  file.path(path_ml, "cluster_silhouette_3_to_4.csv")
)
safe_write_csv(
  cluster_profiles,
  file.path(path_tables, "data_driven_cluster_profiles.csv")
)

# ---- 7. Random Forest sensitivity diagnostic -------------------------------
# This reconstructs an index from its own inputs. It is not external validation
# and must not be described as causal driver analysis.
rf_data <- cbind(
  ctvi_score = index_table$ctvi_score,
  as.data.frame(X)
)
rf_data <- rf_data[stats::complete.cases(rf_data), ]

set.seed(random_seed)
rf_fit <- randomForest::randomForest(
  ctvi_score ~ .,
  data = rf_data,
  importance = TRUE,
  ntree = 500
)
rf_importance <- tibble::as_tibble(
  randomForest::importance(rf_fit),
  rownames = "variable"
) |>
  dplyr::arrange(dplyr::desc(`%IncMSE`))
rf_reconstruction_r2 <- unname(tail(rf_fit$rsq, 1))

safe_write_csv(
  rf_importance,
  file.path(path_ml, "rf_index_sensitivity_importance.csv")
)

rf_plot <- rf_importance |>
  dplyr::mutate(
    variable = forcats::fct_reorder(variable, `%IncMSE`)
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = `%IncMSE`, y = variable)) +
  ggplot2::geom_col(fill = "#2c7fb8") +
  ggplot2::labs(
    title = "CTVI sensitivity to input indicators",
    subtitle = glue::glue(
      "Random Forest index reconstruction diagnostic; OOB R² ≈ {round(rf_reconstruction_r2, 3)}"
    ),
    x = "% increase in MSE after permutation",
    y = NULL,
    caption = "Diagnostic only: the same indicators were used to construct the CTVI."
  ) +
  ggplot2::theme_minimal(base_size = 11)

ggplot2::ggsave(
  file.path(path_maps, "ctvi_rf_index_sensitivity.png"),
  rf_plot,
  width = 8,
  height = 5,
  dpi = 400,
  bg = "white"
)

# ---- 8. Typology classification and summaries ------------------------------
typology_classification <- index_table |>
  dplyr::arrange(ctvi_rank) |>
  dplyr::select(
    ctvi_rank,
    adm2_id,
    adm2_name,
    adm1_name,
    environment_type,
    climate_exposure_score,
    technology_gap_score,
    ctvi_score,
    resilience_typology,
    typology_code,
    typology_interpretation,
    data_driven_typology,
    population,
    priority_pop_weighted,
    pca_rank_change,
    robust_rank_change
  )

summary_statistics <- function(data, group_variables, score_variables) {
  data |>
    tidyr::pivot_longer(
      dplyr::all_of(score_variables),
      names_to = "measure",
      values_to = "value"
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_variables)),
      measure
    ) |>
    dplyr::summarise(
      n = sum(is.finite(value)),
      mean = mean(value, na.rm = TRUE),
      sd = stats::sd(value, na.rm = TRUE),
      minimum = min(value, na.rm = TRUE),
      maximum = max(value, na.rm = TRUE),
      median = stats::median(value, na.rm = TRUE),
      iqr = stats::IQR(value, na.rm = TRUE),
      cv_pct = dplyr::if_else(
        is.finite(mean) & abs(mean) > .Machine$double.eps,
        sd / abs(mean) * 100,
        NA_real_
      ),
      .groups = "drop"
    )
}

score_variables <- c(
  "climate_exposure_score",
  "technology_gap_score",
  "ctvi_score"
)

environment_summary_long <- summary_statistics(
  index_table,
  "environment_type",
  score_variables
)

typology_summary_long <- summary_statistics(
  index_table,
  "resilience_typology",
  score_variables
)

typology_summary <- index_table |>
  dplyr::group_by(resilience_typology) |>
  dplyr::summarise(
    n = dplyr::n(),
    climate_exposure_mean = mean(climate_exposure_score, na.rm = TRUE),
    climate_exposure_sd = stats::sd(climate_exposure_score, na.rm = TRUE),
    technology_gap_mean = mean(technology_gap_score, na.rm = TRUE),
    technology_gap_sd = stats::sd(technology_gap_score, na.rm = TRUE),
    ctvi_mean = mean(ctvi_score, na.rm = TRUE),
    ctvi_sd = stats::sd(ctvi_score, na.rm = TRUE),
    ctvi_median = stats::median(ctvi_score, na.rm = TRUE),
    population_sum = if (any(!is.finite(population))) NA_real_ else sum(population),
    interpretation = dplyr::first(typology_interpretation),
    .groups = "drop"
  )

environment_summary <- index_table |>
  dplyr::group_by(environment_type) |>
  dplyr::summarise(
    n = dplyr::n(),
    climate_exposure_mean = mean(climate_exposure_score, na.rm = TRUE),
    climate_exposure_sd = stats::sd(climate_exposure_score, na.rm = TRUE),
    technology_gap_mean = mean(technology_gap_score, na.rm = TRUE),
    technology_gap_sd = stats::sd(technology_gap_score, na.rm = TRUE),
    ctvi_mean = mean(ctvi_score, na.rm = TRUE),
    ctvi_sd = stats::sd(ctvi_score, na.rm = TRUE),
    ctvi_median = stats::median(ctvi_score, na.rm = TRUE),
    population_sum = if (any(!is.finite(population))) NA_real_ else sum(population),
    .groups = "drop"
  )

safe_write_csv(
  typology_classification,
  file.path(path_tables, "typology_classification_table.csv")
)
safe_write_csv(
  typology_summary,
  file.path(path_tables, "typology_summary_table.csv")
)
safe_write_csv(
  typology_summary_long,
  file.path(path_tables, "typology_enhanced_statistics_long.csv")
)
safe_write_csv(
  environment_summary,
  file.path(path_tables, "environment_comparison_summary.csv")
)
safe_write_csv(
  environment_summary_long,
  file.path(path_tables, "environment_enhanced_statistics_long.csv")
)

# ---- 9. Smart-city vs urban-edge vs rural statistical comparisons ----------
kruskal_results <- purrr::map_dfr(
  score_variables,
  function(score) {
    formula <- stats::reformulate("environment_type", response = score)
    test <- stats::kruskal.test(formula, data = index_table)
    tibble::tibble(
      measure = score,
      method = unname(test$method),
      statistic = unname(test$statistic),
      df = unname(test$parameter),
      p_value = test$p.value
    )
  }
)

pairwise_results <- purrr::map_dfr(
  score_variables,
  function(score) {
    test <- stats::pairwise.wilcox.test(
      x = index_table[[score]],
      g = index_table$environment_type,
      p.adjust.method = "BH",
      exact = FALSE
    )

    as.data.frame(as.table(test$p.value)) |>
      tibble::as_tibble() |>
      dplyr::rename(group_1 = Var1, group_2 = Var2, p_adjusted = Freq) |>
      dplyr::filter(!is.na(p_adjusted)) |>
      dplyr::mutate(
        measure = score,
        method = "Pairwise Wilcoxon rank-sum; Benjamini-Hochberg adjustment",
        .before = 1
      )
  }
)

safe_write_csv(
  kruskal_results,
  file.path(path_tables, "environment_kruskal_wallis_tests.csv")
)
safe_write_csv(
  pairwise_results,
  file.path(path_tables, "environment_pairwise_wilcoxon_bh.csv")
)

# ---- 10. Comparative visuals ------------------------------------------------
measure_labels <- c(
  climate_exposure_score = "Climate exposure",
  technology_gap_score = "Technology gap",
  ctvi_score = "CTVI"
)

comparison_plot_data <- environment_summary_long |>
  dplyr::mutate(
    measure_label = factor(
      unname(measure_labels[measure]),
      levels = unname(measure_labels)
    ),
    se = sd / sqrt(n),
    ci_multiplier = stats::qt(0.975, pmax(n - 1, 1)),
    ci_low = pmax(0, mean - ci_multiplier * se),
    ci_high = pmin(1, mean + ci_multiplier * se)
  )

comparison_plot <- ggplot2::ggplot(
  comparison_plot_data,
  ggplot2::aes(
    x = environment_type,
    y = mean,
    fill = measure_label
  )
) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.82),
    width = 0.72
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = ci_low, ymax = ci_high),
    position = ggplot2::position_dodge(width = 0.82),
    width = 0.18,
    linewidth = 0.4
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = ggplot2::expansion(mult = c(0, 0.03))
  ) +
  ggplot2::labs(
    title = "Climate-technology vulnerability across analytical environments",
    subtitle = "Bars show group means; error bars show 95% confidence intervals",
    x = NULL,
    y = "Mean standardised score",
    fill = NULL,
    caption = paste(
      "Environment types are analytical classifications.",
      "Review the override table before publication."
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 25, hjust = 1),
    legend.position = "top",
    panel.grid.major.x = ggplot2::element_blank()
  )

ggplot2::ggsave(
  file.path(path_maps, "comparison_environment_scores_bar.png"),
  comparison_plot,
  width = 9,
  height = 5.5,
  dpi = 500,
  bg = "white"
)
ggplot2::ggsave(
  file.path(path_maps, "comparison_environment_scores_bar.pdf"),
  comparison_plot,
  width = 9,
  height = 5.5,
  bg = "white"
)

typology_composition <- index_table |>
  dplyr::count(environment_type, resilience_typology, name = "n") |>
  dplyr::group_by(environment_type) |>
  dplyr::mutate(percent = n / sum(n) * 100) |>
  dplyr::ungroup()

typology_composition_plot <- ggplot2::ggplot(
  typology_composition,
  ggplot2::aes(
    x = environment_type,
    y = percent,
    fill = resilience_typology
  )
) +
  ggplot2::geom_col(width = 0.72) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    labels = scales::label_percent(scale = 1)
  ) +
  ggplot2::labs(
    title = "Resilience typology composition by analytical environment",
    x = NULL,
    y = "Share of ADM2 areas",
    fill = "Typology"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 25, hjust = 1),
    legend.position = "right",
    panel.grid.major.x = ggplot2::element_blank()
  )

ggplot2::ggsave(
  file.path(path_maps, "comparison_typology_composition.png"),
  typology_composition_plot,
  width = 9,
  height = 5.5,
  dpi = 500,
  bg = "white"
)

# ---- 11. Dendrogram ---------------------------------------------------------
grDevices::png(
  file.path(path_maps, "ctvi_hclust_dendrogram_revised.png"),
  width = 1800,
  height = 1000,
  res = 160
)
plot(
  hierarchical_fit,
  labels = index_table$adm2_name,
  main = glue::glue("ADM2 data-driven vulnerability clusters (Ward; k = {best_k})"),
  xlab = "",
  sub = "Sensitivity analysis; primary typologies use the climate-gap matrix",
  cex = 0.55
)
stats::rect.hclust(
  hierarchical_fit,
  k = best_k,
  border = seq_len(best_k) + 1
)
grDevices::dev.off()

# ---- 12. Persist index and spatial layer -----------------------------------
safe_write_csv(
  index_table,
  file.path(path_ml, "morocco_ctvi_adm2_index_typologies.csv")
)

map_layer <- adm2_map |>
  dplyr::left_join(
    index_table,
    by = c("adm2_id", "adm2_name", "adm1_name")
  )

sf::st_write(
  map_layer,
  file.path(path_ml, "morocco_ctvi_adm2_index_map_layer.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

# ---- 13. Consolidated Excel workbook ---------------------------------------
workbook_file <- file.path(
  path_tables,
  "CTVI_typology_and_environment_comparison.xlsx"
)

wb <- openxlsx::createWorkbook()

write_sheet <- function(workbook, sheet_name, data) {
  openxlsx::addWorksheet(workbook, sheet_name)
  openxlsx::writeDataTable(
    workbook,
    sheet = sheet_name,
    x = data,
    tableStyle = "TableStyleMedium2",
    withFilter = TRUE
  )
  openxlsx::freezePane(workbook, sheet = sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(workbook, sheet = sheet_name, cols = 1:ncol(data), widths = "auto")
}

write_sheet(wb, "Typology classification", typology_classification)
write_sheet(wb, "Typology summary", typology_summary)
write_sheet(wb, "Environment summary", environment_summary)
write_sheet(wb, "Enhanced env statistics", environment_summary_long)
write_sheet(wb, "Kruskal-Wallis", kruskal_results)
write_sheet(wb, "Pairwise comparisons", pairwise_results)
write_sheet(wb, "Data-driven clusters", cluster_profiles)
write_sheet(wb, "Indicator QC", model_qc)
write_sheet(wb, "PCA variance", readr::read_csv(
  file.path(path_ml, "pca_variance_explained.csv"),
  show_col_types = FALSE
))

method_notes <- tibble::tibble(
  item = c(
    "Primary CTVI",
    "Technology domain",
    "Primary typologies",
    "Environment comparison",
    "PCA",
    "Random Forest",
    "Population priority"
  ),
  note = c(
    "Equal-weight mean of climate exposure and technology-gap sub-indices.",
    paste(technology_vars, collapse = "; "),
    "Four median-split climate-exposure × technology-gap typologies.",
    "Smart city/urban core, urban edge/peri-urban and rural/lower-density analytical classes; review overrides before publication.",
    "Sensitivity analysis only, not the headline CTVI.",
    "Index-reconstruction sensitivity diagnostic, not causal driver analysis or external validation.",
    "Relative CTVI × log-scaled population score; not an estimate of affected people."
  )
)
write_sheet(wb, "Method notes", method_notes)

openxlsx::saveWorkbook(wb, workbook_file, overwrite = TRUE)

message(
  "Model, typology and comparison outputs complete. Primary typologies: ",
  paste(levels(index_table$resilience_typology), collapse = "; ")
)