# =============================================================================
# 05_hotspots_and_maps.R
# Spatial hotspot analysis + the Morocco maps that present the results.
#   * Getis-Ord Gi*  -> statistically significant hot / cold spots
#   * Local Moran's I (LISA) -> High-High / Low-Low / outlier clusters
#   * Choropleths: CTVI, typology, climate exposure, technology gap,
#     population-weighted priority, plus the two hotspot maps.
# =============================================================================

map_gpkg <- file.path(path_ml, "morocco_ctvi_adm2_index_map_layer.gpkg")
stopifnot(file.exists(map_gpkg))
m <- sf::st_read(map_gpkg, quiet = TRUE) |> sf::st_make_valid()

# Theme + a parent-region outline for cartographic context.
theme_map <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(axis.text = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 panel.grid = ggplot2::element_blank())
adm1_outline <- sf::st_read(file.path(path_int_spatial, "morocco_adm1.gpkg"), quiet = TRUE)

save_map <- function(p, name, w = 8, h = 9) {
  ggplot2::ggsave(file.path(path_maps, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
  ggplot2::ggsave(file.path(path_maps, paste0(name, ".pdf")), p, width = w, height = h)
}

# ---- 1. Spatial weights -----------------------------------------------------
# Queen contiguity; zero.policy tolerates provinces with no shared border.
nb <- spdep::poly2nb(m, queen = TRUE)
lw_W    <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)            # Moran
lw_self <- spdep::nb2listw(spdep::include.self(nb), style = "B", zero.policy = TRUE) # Gi*

x <- m$ctvi_score
x[is.na(x)] <- stats::median(x, na.rm = TRUE)  # guard against stray NA

# ---- 2. Getis-Ord Gi* (hotspots) -------------------------------------------
gi_z <- as.numeric(spdep::localG(x, lw_self, zero.policy = TRUE))
m$gi_z <- gi_z
m$hotspot <- dplyr::case_when(
  gi_z >=  2.58 ~ "Hot spot (99%)",
  gi_z >=  1.96 ~ "Hot spot (95%)",
  gi_z >=  1.65 ~ "Hot spot (90%)",
  gi_z <= -2.58 ~ "Cold spot (99%)",
  gi_z <= -1.96 ~ "Cold spot (95%)",
  gi_z <= -1.65 ~ "Cold spot (90%)",
  TRUE          ~ "Not significant")
m$hotspot <- factor(m$hotspot, levels = c(
  "Cold spot (99%)","Cold spot (95%)","Cold spot (90%)","Not significant",
  "Hot spot (90%)","Hot spot (95%)","Hot spot (99%)"))

# ---- 3. Local Moran's I (LISA clusters) ------------------------------------
lm  <- spdep::localmoran(x, lw_W, zero.policy = TRUE)
lag <- spdep::lag.listw(lw_W, x, zero.policy = TRUE)
p   <- lm[, ncol(lm)]                          # p-value column
xz  <- x - mean(x); lz <- lag - mean(lag)
m$lisa <- dplyr::case_when(
  p > 0.05            ~ "Not significant",
  xz > 0 & lz > 0     ~ "High-High (vulnerable cluster)",
  xz < 0 & lz < 0     ~ "Low-Low (resilient cluster)",
  xz > 0 & lz < 0     ~ "High-Low (outlier)",
  TRUE                ~ "Low-High (outlier)")
m$lisa <- factor(m$lisa, levels = c(
  "High-High (vulnerable cluster)","Low-Low (resilient cluster)",
  "High-Low (outlier)","Low-High (outlier)","Not significant"))

# Persist hotspot results (table + spatial layer).
safe_write_csv(sf::st_drop_geometry(
  dplyr::select(m, adm2_id, adm2_name, adm1_name, ctvi_score,
                gi_z, hotspot, lisa)),
  file.path(path_ml, "morocco_ctvi_adm2_hotspots.csv"))
sf::st_write(m, file.path(path_ml, "morocco_ctvi_adm2_index_hotspots.gpkg"),
             delete_dsn = TRUE, quiet = TRUE)

# ---- 4. Maps ----------------------------------------------------------------
base <- function(fill_aes, title, sub = NULL)
  ggplot2::ggplot(m) +
    ggplot2::geom_sf(ggplot2::aes(fill = {{ fill_aes }}), colour = "grey30", linewidth = 0.1) +
    ggplot2::geom_sf(data = adm1_outline, fill = NA, colour = "black", linewidth = 0.35) +
    ggplot2::labs(title = title, subtitle = sub) + theme_map

# 4a. Headline CTVI choropleth (PCA-derived).
p_ctvi <- base(ctvi_score, "Morocco Climate-Technology Vulnerability Index (ADM2)",
               "PCA-derived composite; higher = more vulnerable") +
  ggplot2::scale_fill_viridis_c(option = "C", na.value = "grey85", name = "CTVI")
save_map(p_ctvi, "map_01_ctvi_score")

# 4b. Vulnerability typology (clusters).
p_typ <- base(vulnerability_typology, "Vulnerability typologies (k-means, silhouette-selected)") +
  ggplot2::scale_fill_brewer(palette = "YlOrRd", na.value = "grey85", name = "Typology")
save_map(p_typ, "map_02_typology")

# 4c. Climate exposure sub-score.
p_exp <- base(climate_exposure_score, "Climate exposure sub-score (ADM2)") +
  ggplot2::scale_fill_viridis_c(option = "B", na.value = "grey85", name = "Exposure")
save_map(p_exp, "map_03_climate_exposure")

# 4d. Technology / services access gap sub-score.
p_gap <- base(technology_gap_score, "Technology & services access gap (ADM2)") +
  ggplot2::scale_fill_viridis_c(option = "D", na.value = "grey85", name = "Access gap")
save_map(p_gap, "map_04_technology_gap")

# 4e. Getis-Ord Gi* HOTSPOT MAP.
hot_cols <- c("Cold spot (99%)" = "#2166ac","Cold spot (95%)" = "#67a9cf",
              "Cold spot (90%)" = "#d1e5f0","Not significant" = "grey88",
              "Hot spot (90%)" = "#fddbc7","Hot spot (95%)" = "#ef8a62",
              "Hot spot (99%)" = "#b2182b")
p_hot <- base(hotspot, "Vulnerability hot spots (Getis-Ord Gi*)",
              "Red = significant clusters of HIGH vulnerability; blue = LOW") +
  ggplot2::scale_fill_manual(values = hot_cols, drop = FALSE, name = "Gi* class")
save_map(p_hot, "map_05_hotspots_getis_ord")

# 4f. LISA cluster map.
lisa_cols <- c("High-High (vulnerable cluster)" = "#b2182b",
               "Low-Low (resilient cluster)" = "#2166ac",
               "High-Low (outlier)" = "#ef8a62","Low-High (outlier)" = "#67a9cf",
               "Not significant" = "grey88")
p_lisa <- base(lisa, "Local Moran's I clusters (LISA)") +
  ggplot2::scale_fill_manual(values = lisa_cols, drop = FALSE, name = "LISA")
save_map(p_lisa, "map_06_lisa_clusters")

# 4g. Population-weighted priority (where vulnerability meets people).
p_pri <- base(priority_pop_weighted, "Adaptation priority: vulnerability x population",
              "Highlights where vulnerable conditions affect the most people") +
  ggplot2::scale_fill_viridis_c(option = "A", na.value = "grey85", name = "Priority")
save_map(p_pri, "map_07_population_priority")

message("Hotspot analysis + maps complete. See 04_maps/ (map_01 ... map_07).")
