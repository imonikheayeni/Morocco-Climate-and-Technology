# =============================================================================
# 04_model_and_index.R
# Builds the CTVI with a DATA-DRIVEN (ML) methodology instead of fixed weights:
#   (a) direction-align + scale features so higher = more vulnerable
#   (b) PCA  -> data-driven composite index (replaces arbitrary 60/40)
#   (c) k-means + hierarchical clustering -> vulnerability typologies (k via silhouette)
#   (d) Random Forest -> driver attribution (which variables move the index)
# A transparent equal-weight index is also kept for comparison/auditing.
# =============================================================================

feat_file <- file.path(path_ml, "morocco_ctvi_adm2_features_imputed.csv")
adm2_gpkg <- file.path(path_int_spatial, "morocco_adm2.gpkg")
stopifnot(file.exists(feat_file), file.exists(adm2_gpkg))

ml   <- readr::read_csv(feat_file, show_col_types = FALSE)
adm2 <- sf::st_read(adm2_gpkg, quiet = TRUE)

# ---- 1. Define feature direction (higher value -> more vulnerable?) ---------
# +1 keeps the variable as-is; -1 flips it (so that "less access / less rain"
# becomes "more vulnerable"). Only features present are used.
direction <- c(
  heat_days_gt_35c_5yr        =  1,
  temp_max_mean_c_5yr         =  1,
  temp_mean_c_5yr             =  1,
  dry_days_lt_1mm_5yr         =  1,
  precipitation_total_mm_5yr  = -1,
  solar_mean_kwh_m2_day_5yr   =  1,
  osm_road_km_per_100km2      = -1,
  osm_poi_count_per_100km2    = -1,
  osm_health_education_poi_per_100km2 = -1,
  population_density_per_km2  = -1   # denser -> usually better serviced
)
use_vars <- intersect(names(direction), names(ml))
message("Modelling on features: ", paste(use_vars, collapse = ", "))

# Build a vulnerability-aligned, 0-1 scaled matrix.
aligned <- ml |> dplyr::select(adm2_id, adm2_name, adm1_name, dplyr::all_of(use_vars))
for (v in use_vars) aligned[[v]] <- scale_01(direction[[v]] * ml[[v]])
aligned <- aligned |> dplyr::select(where(~ !all(is.na(.x))))  # drop dead cols
model_vars <- setdiff(names(aligned), c("adm2_id", "adm2_name", "adm1_name"))

# Split into the two conceptual axes for transparent sub-scores.
climate_vars <- intersect(model_vars,
  c("heat_days_gt_35c_5yr","temp_max_mean_c_5yr","temp_mean_c_5yr",
    "dry_days_lt_1mm_5yr","precipitation_total_mm_5yr","solar_mean_kwh_m2_day_5yr"))
tech_vars <- intersect(model_vars,
  c("osm_road_km_per_100km2","osm_poi_count_per_100km2",
    "osm_health_education_poi_per_100km2","population_density_per_km2"))

X <- as.matrix(aligned[, model_vars])

# ---- 2. Transparent equal-weight index (audit baseline) --------------------
climate_exposure_score <- if (length(climate_vars))
  rowMeans(X[, climate_vars, drop = FALSE], na.rm = TRUE) else NA_real_
technology_gap_score   <- if (length(tech_vars))
  rowMeans(X[, tech_vars, drop = FALSE], na.rm = TRUE) else NA_real_
ctvi_equal <- rowMeans(cbind(climate_exposure_score, technology_gap_score), na.rm = TRUE)

# ---- 3. PCA -> data-driven composite (the ML weighting) --------------------
pca <- stats::prcomp(X, center = TRUE, scale. = TRUE)
ve  <- summary(pca)$importance[2, ]            # proportion of variance
pc1 <- pca$x[, 1]
# Sign convention: make PC1 point the same way as the transparent index.
if (stats::cor(pc1, ctvi_equal, use = "complete.obs") < 0) pc1 <- -pc1
ctvi_pca <- scale_01(pc1)

# Save PCA diagnostics (variance + loadings) for the write-up.
safe_write_csv(tibble::tibble(component = names(ve), variance_explained = as.numeric(ve)),
               file.path(path_ml, "pca_variance_explained.csv"))
safe_write_csv(tibble::as_tibble(pca$rotation, rownames = "variable"),
               file.path(path_ml, "pca_loadings.csv"))

# ---- 4. Assemble index table -----------------------------------------------
index_tbl <- aligned |>
  dplyr::select(adm2_id, adm2_name, adm1_name) |>
  dplyr::mutate(
    climate_exposure_score = climate_exposure_score,
    technology_gap_score   = technology_gap_score,
    ctvi_equal_weight      = ctvi_equal,
    ctvi_score             = ctvi_pca,                 # <- headline, PCA-derived
    population             = ml$population %||% NA_real_
  ) |>
  dplyr::mutate(
    ctvi_rank = dplyr::min_rank(dplyr::desc(ctvi_score)),
    # Priority = vulnerability x people exposed (where population available).
    priority_pop_weighted = ctvi_score * scale_01(population)
  )

# ---- 5. Clustering: choose k by silhouette, label by mean CTVI -------------
Xs <- scale(X)
sil_k <- function(k) {
  set.seed(123)
  km <- stats::kmeans(Xs, centers = k, nstart = 25)
  mean(cluster::silhouette(km$cluster, stats::dist(Xs))[, 3])
}
ks <- 2:min(8, nrow(Xs) - 1)
best_k <- ks[which.max(vapply(ks, sil_k, numeric(1)))]
set.seed(123)
km <- stats::kmeans(Xs, centers = best_k, nstart = 50)
index_tbl$kmeans_cluster <- km$cluster

# Hierarchical (Ward) as a cross-check + dendrogram.
hc <- stats::hclust(stats::dist(Xs), method = "ward.D2")
index_tbl$hclust_cluster <- stats::cutree(hc, k = best_k)

# Order clusters low->high mean CTVI and give readable typology labels.
ord <- index_tbl |> dplyr::group_by(kmeans_cluster) |>
  dplyr::summarise(m = mean(ctvi_score, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(m) |> dplyr::mutate(rank = dplyr::row_number())
labels_by_rank <- if (best_k <= 3) {
  c("Lower", "Moderate", "Higher")[seq_len(best_k)]
} else {
  c("Lowest","Low","Moderate","High","Higher","Highest","Extreme","Critical")[seq_len(best_k)]
}
ord$vulnerability_typology <- paste(labels_by_rank, "vulnerability")
index_tbl <- index_tbl |>
  dplyr::left_join(dplyr::select(ord, kmeans_cluster, vulnerability_typology),
                   by = "kmeans_cluster")

message("Silhouette-selected k = ", best_k)

# ---- 6. Random Forest: which variables drive the index? --------------------
rf_df <- cbind(ctvi_score = index_tbl$ctvi_score, as.data.frame(X))
rf_df <- rf_df[stats::complete.cases(rf_df), ]
set.seed(123)
rf <- randomForest::randomForest(ctvi_score ~ ., data = rf_df,
                                 importance = TRUE, ntree = 500)
imp <- tibble::as_tibble(randomForest::importance(rf), rownames = "variable") |>
  dplyr::arrange(dplyr::desc(`%IncMSE`))
safe_write_csv(imp, file.path(path_ml, "rf_variable_importance.csv"))
rf_r2 <- round(rf$rsq[length(rf$rsq)], 3)   # variance of the index explained
message("Random Forest pseudo-R^2 (index reconstruction): ", rf_r2)

# Correlation of the two index variants (sanity check).
message("cor(PCA index, equal-weight index) = ",
        round(stats::cor(index_tbl$ctvi_score, index_tbl$ctvi_equal_weight,
                         use = "complete.obs"), 3))

# ---- 7. Persist results -----------------------------------------------------
safe_write_csv(index_tbl, file.path(path_ml, "morocco_ctvi_adm2_index_and_clusters.csv"))
map_layer <- adm2 |> dplyr::left_join(index_tbl, by = c("adm2_id", "adm2_name", "adm1_name"))
sf::st_write(map_layer, file.path(path_ml, "morocco_ctvi_adm2_index_map_layer.gpkg"),
             delete_dsn = TRUE, quiet = TRUE)

# Variable-importance plot (a results chart, not a map).
imp_plot <- imp |> dplyr::mutate(variable = forcats::fct_reorder(variable, `%IncMSE`)) |>
  ggplot2::ggplot(ggplot2::aes(`%IncMSE`, variable)) +
  ggplot2::geom_col(fill = "#2c7fb8") +
  ggplot2::labs(title = "CTVI driver attribution (Random Forest)",
                subtitle = glue::glue("Index variance explained (OOB) approx {rf_r2}"),
                x = "% increase in MSE when permuted", y = NULL) +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave(file.path(path_maps, "ctvi_rf_variable_importance.png"),
                imp_plot, width = 8, height = 5, dpi = 300)

# Dendrogram.
grDevices::png(file.path(path_maps, "ctvi_hclust_dendrogram.png"),
               width = 1600, height = 900, res = 150)
plot(hc, labels = index_tbl$adm2_name, main = "ADM2 vulnerability typologies (Ward)",
     xlab = "", sub = "", cex = 0.6)
stats::rect.hclust(hc, k = best_k, border = 2:(best_k + 1))
grDevices::dev.off()

message("Modelling + index build complete.")
