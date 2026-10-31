# =============================================================================
# 05_hotspots_maps_reporting_revised.R
# Spatial hotspot analysis, publication maps, bivariate climate-technology map,
# final typology tables and automatically extracted comparative insights.
#
# Main revisions:
#   * Benjamini-Hochberg control for local Gi* and Local Moran p-values.
#   * Queen-contiguity primary weights plus 4-nearest-neighbour sensitivity.
#   * Explicit quantile-class maps with interval labels for close values.
#   * Four-type resilience map and a 3x3 bivariate climate-technology figure.
#   * Final tables combine typology, environment and spatial-cluster results.
# =============================================================================

map_file <- file.path(path_ml, "morocco_ctvi_adm2_index_map_layer.gpkg")
stopifnot(file.exists(map_file))

m <- sf::st_read(map_file, quiet = TRUE) |>
  sf::st_make_valid()
adm1_outline <- sf::st_read(
  file.path(path_int_spatial, "morocco_adm1.gpkg"),
  quiet = TRUE
) |>
  sf::st_transform(sf::st_crs(m))

x <- m$ctvi_score
if (all(!is.finite(x))) stop("CTVI score is entirely missing.")
x[!is.finite(x)] <- stats::median(x, na.rm = TRUE)

# ---- 1. Spatial weights -----------------------------------------------------
queen_neighbours <- spdep::poly2nb(m, queen = TRUE)
queen_weights <- spdep::nb2listw(
  queen_neighbours,
  style = "W",
  zero.policy = TRUE
)
queen_weights_with_self <- spdep::nb2listw(
  spdep::include.self(queen_neighbours),
  style = "B",
  zero.policy = TRUE
)

# 4-nearest-neighbour weights for sensitivity analysis.
representative_points <- sf::st_point_on_surface(sf::st_transform(m, 6933))
point_coordinates <- sf::st_coordinates(representative_points)
knn_k <- min(4L, nrow(m) - 1L)
knn_neighbours <- spdep::knn2nb(
  spdep::knearneigh(point_coordinates, k = knn_k)
)
knn_weights <- spdep::nb2listw(
  knn_neighbours,
  style = "W",
  zero.policy = TRUE
)

extract_moran_result <- function(test, weights_name) {
  tibble::tibble(
    weights = weights_name,
    moran_i = unname(test$estimate[["Moran I statistic"]]),
    expectation = unname(test$estimate[["Expectation"]]),
    variance = unname(test$estimate[["Variance"]]),
    statistic = unname(test$statistic),
    p_value = test$p.value,
    method = unname(test$method)
  )
}

moran_queen <- spdep::moran.test(
  x,
  queen_weights,
  zero.policy = TRUE
)
moran_knn <- spdep::moran.test(
  x,
  knn_weights,
  zero.policy = TRUE
)

moran_sensitivity <- dplyr::bind_rows(
  extract_moran_result(moran_queen, "Queen contiguity"),
  extract_moran_result(moran_knn, paste0(knn_k, "-nearest neighbours"))
)
safe_write_csv(
  moran_sensitivity,
  file.path(path_tables, "global_moran_spatial_weights_sensitivity.csv")
)

# ---- 2. Getis-Ord Gi* with FDR control -------------------------------------
gi_z <- as.numeric(spdep::localG(
  x,
  queen_weights_with_self,
  zero.policy = TRUE
))
gi_p_raw <- 2 * stats::pnorm(-abs(gi_z))
gi_p_fdr <- stats::p.adjust(gi_p_raw, method = "BH")

m$gi_z <- gi_z
m$gi_p_raw <- gi_p_raw
m$gi_p_fdr <- gi_p_fdr
m$hotspot <- dplyr::case_when(
  gi_z < 0 & gi_p_fdr <= 0.01 ~ "Cold spot (99%)",
  gi_z < 0 & gi_p_fdr <= 0.05 ~ "Cold spot (95%)",
  gi_z < 0 & gi_p_fdr <= 0.10 ~ "Cold spot (90%)",
  gi_z > 0 & gi_p_fdr <= 0.01 ~ "Hot spot (99%)",
  gi_z > 0 & gi_p_fdr <= 0.05 ~ "Hot spot (95%)",
  gi_z > 0 & gi_p_fdr <= 0.10 ~ "Hot spot (90%)",
  TRUE ~ "Not significant"
)
m$hotspot <- factor(
  m$hotspot,
  levels = c(
    "Cold spot (99%)",
    "Cold spot (95%)",
    "Cold spot (90%)",
    "Not significant",
    "Hot spot (90%)",
    "Hot spot (95%)",
    "Hot spot (99%)"
  )
)

# ---- 3. Local Moran's I with FDR control -----------------------------------
local_moran <- spdep::localmoran(
  x,
  queen_weights,
  zero.policy = TRUE
)
spatial_lag <- spdep::lag.listw(
  queen_weights,
  x,
  zero.policy = TRUE
)

local_p_columns <- grep("^Pr\\(", colnames(local_moran), value = TRUE)
if (length(local_p_columns) > 0) {
  lisa_p_raw <- local_moran[, tail(local_p_columns, 1)]
} else {
  lisa_p_raw <- local_moran[, ncol(local_moran)]
}
lisa_p_fdr <- stats::p.adjust(lisa_p_raw, method = "BH")

x_centered <- x - mean(x, na.rm = TRUE)
lag_centered <- spatial_lag - mean(spatial_lag, na.rm = TRUE)

m$lisa_i <- local_moran[, "Ii"]
m$lisa_p_raw <- lisa_p_raw
m$lisa_p_fdr <- lisa_p_fdr
m$lisa <- dplyr::case_when(
  lisa_p_fdr > spatial_alpha ~ "Not significant",
  x_centered > 0 & lag_centered > 0 ~
    "High-High (vulnerable cluster)",
  x_centered < 0 & lag_centered < 0 ~
    "Low-Low (resilient cluster)",
  x_centered > 0 & lag_centered < 0 ~ "High-Low (outlier)",
  TRUE ~ "Low-High (outlier)"
)
m$lisa <- factor(
  m$lisa,
  levels = c(
    "High-High (vulnerable cluster)",
    "Low-Low (resilient cluster)",
    "High-Low (outlier)",
    "Low-High (outlier)",
    "Not significant"
  )
)

# ---- 4. Persist spatial results --------------------------------------------
spatial_results <- m |>
  sf::st_drop_geometry() |>
  dplyr::select(
    adm2_id,
    adm2_name,
    adm1_name,
    environment_type,
    climate_exposure_score,
    technology_gap_score,
    ctvi_score,
    ctvi_rank,
    resilience_typology,
    typology_code,
    gi_z,
    gi_p_raw,
    gi_p_fdr,
    hotspot,
    lisa_i,
    lisa_p_raw,
    lisa_p_fdr,
    lisa,
    population,
    priority_pop_weighted
  )

safe_write_csv(
  spatial_results,
  file.path(path_ml, "morocco_ctvi_adm2_hotspots_typologies.csv")
)
sf::st_write(
  m,
  file.path(path_ml, "morocco_ctvi_adm2_index_hotspots.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

# ---- 5. Final typology and environment summary tables ----------------------
typology_spatial_summary <- spatial_results |>
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
    hot_spot_n = sum(stringr::str_detect(as.character(hotspot), "^Hot spot")),
    high_high_lisa_n = sum(
      lisa == "High-High (vulnerable cluster)",
      na.rm = TRUE
    ),
    population_sum = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

environment_spatial_summary <- spatial_results |>
  dplyr::group_by(environment_type) |>
  dplyr::summarise(
    n = dplyr::n(),
    climate_exposure_mean = mean(climate_exposure_score, na.rm = TRUE),
    technology_gap_mean = mean(technology_gap_score, na.rm = TRUE),
    ctvi_mean = mean(ctvi_score, na.rm = TRUE),
    ctvi_sd = stats::sd(ctvi_score, na.rm = TRUE),
    critical_typology_n = sum(
      resilience_typology == "Critical vulnerability",
      na.rm = TRUE
    ),
    critical_typology_pct = critical_typology_n / n * 100,
    hot_spot_n = sum(stringr::str_detect(as.character(hotspot), "^Hot spot")),
    high_high_lisa_n = sum(
      lisa == "High-High (vulnerable cluster)",
      na.rm = TRUE
    ),
    population_sum = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

safe_write_csv(
  typology_spatial_summary,
  file.path(path_tables, "typology_classification_summary_with_spatial_results.csv")
)
safe_write_csv(
  environment_spatial_summary,
  file.path(path_tables, "environment_comparison_with_spatial_results.csv")
)
safe_write_csv(
  spatial_results |>
    dplyr::arrange(ctvi_rank),
  file.path(path_tables, "typology_classification_table_with_spatial_results.csv")
)

# ---- 6. Automatically extract comparative insights -------------------------
highest_environment_ctvi <- environment_spatial_summary |>
  dplyr::slice_max(ctvi_mean, n = 1, with_ties = FALSE)
highest_environment_climate <- environment_spatial_summary |>
  dplyr::slice_max(climate_exposure_mean, n = 1, with_ties = FALSE)
highest_environment_gap <- environment_spatial_summary |>
  dplyr::slice_max(technology_gap_mean, n = 1, with_ties = FALSE)
highest_critical_share <- environment_spatial_summary |>
  dplyr::slice_max(critical_typology_pct, n = 1, with_ties = FALSE)
highest_typology <- typology_spatial_summary |>
  dplyr::slice_max(ctvi_mean, n = 1, with_ties = FALSE)

top_ranked_areas <- spatial_results |>
  dplyr::arrange(ctvi_rank) |>
  dplyr::slice_head(n = 5) |>
  dplyr::pull(adm2_name)

key_insights <- tibble::tibble(
  insight_id = c(
    "ENV_CTVI",
    "ENV_CLIMATE",
    "ENV_TECH_GAP",
    "ENV_CRITICAL_SHARE",
    "TYPOLOGY_CTVI",
    "TOP_ADM2",
    "SPATIAL_CLUSTERING"
  ),
  comparative_insight = c(
    glue::glue(
      "{highest_environment_ctvi$environment_type} had the highest mean CTVI ({round(highest_environment_ctvi$ctvi_mean, 3)})."
    ),
    glue::glue(
      "{highest_environment_climate$environment_type} had the highest mean climate-exposure score ({round(highest_environment_climate$climate_exposure_mean, 3)})."
    ),
    glue::glue(
      "{highest_environment_gap$environment_type} had the highest mean technology-gap score ({round(highest_environment_gap$technology_gap_mean, 3)})."
    ),
    glue::glue(
      "{highest_critical_share$environment_type} had the largest share of Critical vulnerability areas ({round(highest_critical_share$critical_typology_pct, 1)}%)."
    ),
    glue::glue(
      "The {highest_typology$resilience_typology} typology had the highest mean CTVI ({round(highest_typology$ctvi_mean, 3)})."
    ),
    paste0(
      "The five highest-ranked ADM2 areas were: ",
      paste(top_ranked_areas, collapse = "; "),
      "."
    ),
    glue::glue(
      "After Benjamini-Hochberg adjustment, {sum(stringr::str_detect(as.character(spatial_results$hotspot), '^Hot spot'))} ADM2 areas were Gi* hot spots and {sum(spatial_results$lisa == 'High-High (vulnerable cluster)', na.rm = TRUE)} were LISA High-High clusters."
    )
  ),
  reporting_note = c(
    "Compare with Kruskal-Wallis and pairwise results before describing the difference as statistically significant.",
    "Environment classes are analytical and must be reviewed against official settlement data.",
    "The technology gap currently represents OSM-derived service/access proxies, not direct broadband coverage.",
    "Percentages describe ADM2 units, not the percentage of Morocco's population.",
    "Typology thresholds are sample-relative medians.",
    "Ranks are relative to the included ADM2 units and selected indicators.",
    "Local significance uses FDR-adjusted p-values under queen contiguity."
  )
)

safe_write_csv(
  key_insights,
  file.path(path_tables, "key_comparative_insights.csv")
)

# ---- 7. Publication map helpers --------------------------------------------
make_quantile_factor <- function(x, n = map_quantile_classes, digits = 3) {
  finite <- x[is.finite(x)]
  if (length(finite) == 0) return(factor(rep(NA_character_, length(x))))

  breaks <- unique(as.numeric(stats::quantile(
    finite,
    probs = seq(0, 1, length.out = n + 1),
    na.rm = TRUE,
    type = 8
  )))

  if (length(breaks) < 3) {
    breaks <- unique(pretty(finite, n = n))
  }
  if (length(breaks) < 2) {
    return(factor(rep("No variation", length(x))))
  }

  labels <- paste0(
    formatC(head(breaks, -1), format = "f", digits = digits),
    "–",
    formatC(tail(breaks, -1), format = "f", digits = digits)
  )

  cut(
    x,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    ordered_result = TRUE
  )
}

quantile_rank_3 <- function(x) {
  finite <- x[is.finite(x)]
  breaks <- unique(as.numeric(stats::quantile(
    finite,
    probs = c(0, 1 / 3, 2 / 3, 1),
    na.rm = TRUE,
    type = 8
  )))

  if (length(breaks) == 4) {
    return(as.integer(cut(
      x,
      breaks = breaks,
      labels = 1:3,
      include.lowest = TRUE
    )))
  }

  dplyr::ntile(x, 3)
}

m <- m |>
  dplyr::mutate(
    ctvi_class = make_quantile_factor(ctvi_score),
    climate_class = make_quantile_factor(climate_exposure_score),
    technology_class = make_quantile_factor(technology_gap_score),
    priority_class = if(all(is.finite(priority_pop_weighted))) make_quantile_factor(priority_pop_weighted) else factor(rep(NA_character_,dplyr::n())),
    climate_tertile = quantile_rank_3(climate_exposure_score),
    technology_tertile = quantile_rank_3(technology_gap_score),
    bivariate_class = paste(climate_tertile, technology_tertile, sep = "-")
  )

sequential_colours <- c(
  "#fff7ec",
  "#fee8c8",
  "#fdbb84",
  "#fc8d59",
  "#b30000"
)

resilience_typology_colours <- c(
  "Critical vulnerability" = "#7f0000",
  "Climate-exposed adaptive potential" = "#f46d43",
  "Technology/service development gap" = "#4575b4",
  "Relative resilience" = "#91cf60"
)

environment_colours <- c(
  "Smart city / urban core" = "#2b8cbe",
  "Urban edge / peri-urban" = "#fdae61",
  "Rural / lower-density" = "#7f8c3a"
)

hotspot_colours <- c(
  "Cold spot (99%)" = "#2166ac",
  "Cold spot (95%)" = "#67a9cf",
  "Cold spot (90%)" = "#d1e5f0",
  "Not significant" = "#f0f0f0",
  "Hot spot (90%)" = "#fddbc7",
  "Hot spot (95%)" = "#ef8a62",
  "Hot spot (99%)" = "#b2182b"
)

lisa_colours <- c(
  "High-High (vulnerable cluster)" = "#b2182b",
  "Low-Low (resilient cluster)" = "#2166ac",
  "High-Low (outlier)" = "#ef8a62",
  "Low-High (outlier)" = "#67a9cf",
  "Not significant" = "#f0f0f0"
)

# 3x3 palette: first number = climate tertile; second = technology-gap tertile.
bivariate_colours <- c(
  "1-1" = "#e8e8e8",
  "2-1" = "#ace4e4",
  "3-1" = "#5ac8c8",
  "1-2" = "#dfb0d6",
  "2-2" = "#a5add3",
  "3-2" = "#5698b9",
  "1-3" = "#be64ac",
  "2-3" = "#8c62aa",
  "3-3" = "#3b4994"
)

theme_map_publication <- ggplot2::theme_void(base_size = 10) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 11,
      hjust = 0,
      margin = ggplot2::margin(b = 2)
    ),
    plot.subtitle = ggplot2::element_text(
      size = 8.8,
      colour = "grey30",
      hjust = 0,
      margin = ggplot2::margin(b = 7)
    ),
    plot.caption = ggplot2::element_text(
      size = 7.2,
      colour = "grey35",
      hjust = 0
    ),
    legend.position = "right",
    legend.title = ggplot2::element_text(face = "bold", size = 8.5),
    legend.text = ggplot2::element_text(size = 7.5),
    plot.margin = ggplot2::margin(5, 5, 5, 5)
  )

base_map <- function(fill_variable, title, subtitle = NULL, caption = NULL) {
  ggplot2::ggplot(m) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data[[fill_variable]]),
      colour = "grey75",
      linewidth = 0.08
    ) +
    ggplot2::geom_sf(
      data = adm1_outline,
      fill = NA,
      colour = "grey20",
      linewidth = 0.32
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    theme_map_publication
}

save_map <- function(plot, name, width = 7.3, height = 7.7) {
  ggplot2::ggsave(
    file.path(path_maps, paste0(name, ".png")),
    plot,
    width = width,
    height = height,
    dpi = 600,
    bg = "white"
  )
  ggplot2::ggsave(
    file.path(path_maps, paste0(name, ".pdf")),
    plot,
    width = width,
    height = height,
    bg = "white"
  )
}

apply_quantile_scale <- function(plot, variable_name, legend_title) {
  levels_n <- nlevels(m[[variable_name]])
  colours <- grDevices::colorRampPalette(sequential_colours)(max(levels_n, 1))
  plot +
    ggplot2::scale_fill_manual(
      values = colours,
      drop = FALSE,
      na.value = "grey90",
      name = legend_title
    )
}

# ---- 8. Publication maps ----------------------------------------------------
p_ctvi <- base_map(
  "ctvi_class",
  "Climate-Technology Vulnerability Index (ADM2)",
  "Equal-weight composite; quantile classes separate close values",
  "Intervals are relative to the Moroccan ADM2 distribution, not fixed risk thresholds."
) |>
  apply_quantile_scale("ctvi_class", "CTVI interval")
save_map(p_ctvi, "map_01_ctvi_score_revised")

p_typology <- base_map(
  "resilience_typology",
  "Climate-technology resilience typologies",
  "Four categories from the intersection of climate exposure and technology gap",
  "High/low thresholds are sample medians."
) +
  ggplot2::scale_fill_manual(
    values = resilience_typology_colours,
    drop = FALSE,
    na.value = "grey90",
    name = "Typology"
  )
save_map(p_typology, "map_02_resilience_typology_revised")

p_climate <- base_map(
  "climate_class",
  "Climate exposure sub-index (ADM2)",
  "Quantile intervals improve separation where scores are close"
) |>
  apply_quantile_scale("climate_class", "Exposure interval")
save_map(p_climate, "map_03_climate_exposure_revised")

p_technology <- base_map(
  "technology_class",
  "Technology and services access gap (ADM2)",
  "Higher intervals indicate larger relative gaps",
  "Population density is excluded from the technology-gap sub-index."
) |>
  apply_quantile_scale("technology_class", "Gap interval")
save_map(p_technology, "map_04_technology_gap_revised")

p_environment <- base_map(
  "environment_type",
  "Analytical environment classification",
  "Smart-city/urban-core, urban-edge/peri-urban and rural/lower-density contexts",
  "Review and amend the override table before publication."
) +
  ggplot2::scale_fill_manual(
    values = environment_colours,
    drop = FALSE,
    na.value = "grey90",
    name = "Environment"
  )
save_map(p_environment, "map_05_environment_type_review")

p_hotspot <- base_map(
  "hotspot",
  "Vulnerability hot spots (Getis-Ord Gi*)",
  "Red indicates high-value clusters; blue indicates low-value clusters",
  "Classes use Benjamini-Hochberg-adjusted local p-values."
) +
  ggplot2::scale_fill_manual(
    values = hotspot_colours,
    drop = FALSE,
    name = "Gi* class"
  )
save_map(p_hotspot, "map_06_hotspots_getis_ord_fdr")

p_lisa <- base_map(
  "lisa",
  "Local Moran's I clusters (LISA)",
  "High-High and Low-Low indicate significant spatial clusters",
  "Significance uses Benjamini-Hochberg-adjusted local p-values."
) +
  ggplot2::scale_fill_manual(
    values = lisa_colours,
    drop = FALSE,
    name = "LISA"
  )
save_map(p_lisa, "map_07_lisa_clusters_fdr")

if (all(is.finite(m$priority_pop_weighted))) {
p_priority <- base_map(
  "priority_class",
  "Relative adaptation priority: CTVI × population",
  "Highlights places combining vulnerability with larger population exposure",
  "Uses log-scaled relative population; it is not an estimate of people affected."
) |>
  apply_quantile_scale("priority_class", "Priority interval")
save_map(p_priority, "map_08_population_priority_relative")

} else {
  message("Population quality checks incomplete; population priority map suppressed.")
}

# ---- 9. Bivariate climate-technology comparison figure ---------------------
p_bivariate_map <- base_map(
  "bivariate_class",
  "Joint climate exposure and technology-gap pattern",
  "The darkest class combines high climate exposure with a high technology gap",
  "Both dimensions are divided into ADM2 tertiles."
) +
  ggplot2::scale_fill_manual(
    values = bivariate_colours,
    guide = "none",
    na.value = "grey90"
  )

bivariate_legend_data <- tidyr::expand_grid(
  climate_tertile = 1:3,
  technology_tertile = 1:3
) |>
  dplyr::mutate(
    bivariate_class = paste(
      climate_tertile,
      technology_tertile,
      sep = "-"
    )
  )

p_bivariate_legend <- ggplot2::ggplot(
  bivariate_legend_data,
  ggplot2::aes(
    x = climate_tertile,
    y = technology_tertile,
    fill = bivariate_class
  )
) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
  ggplot2::scale_fill_manual(values = bivariate_colours, guide = "none") +
  ggplot2::scale_x_continuous(
    breaks = 1:3,
    labels = c("Low", "Mid", "High"),
    position = "top"
  ) +
  ggplot2::scale_y_continuous(
    breaks = 1:3,
    labels = c("Low", "Mid", "High")
  ) +
  ggplot2::coord_fixed() +
  ggplot2::labs(
    x = "Climate exposure →",
    y = "Technology gap →"
  ) +
  ggplot2::theme_minimal(base_size = 8) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(face = "bold", size = 8),
    axis.text = ggplot2::element_text(size = 7),
    plot.margin = ggplot2::margin(30, 5, 30, 5)
  )

p_bivariate <- p_bivariate_map + p_bivariate_legend +
  patchwork::plot_layout(widths = c(4.2, 1.1))

save_map(
  p_bivariate,
  "map_09_bivariate_climate_technology",
  width = 9.2,
  height = 7.7
)

# ---- 10. Update consolidated workbook --------------------------------------
workbook_file <- file.path(
  path_tables,
  "CTVI_typology_and_environment_comparison.xlsx"
)

if (file.exists(workbook_file)) {
  wb <- openxlsx::loadWorkbook(workbook_file)
} else {
  wb <- openxlsx::createWorkbook()
}

write_or_replace_sheet <- function(workbook, sheet_name, data) {
  if (sheet_name %in% names(workbook)) {
    openxlsx::removeWorksheet(workbook, sheet_name)
  }
  openxlsx::addWorksheet(workbook, sheet_name)
  openxlsx::writeDataTable(
    workbook,
    sheet = sheet_name,
    x = data,
    tableStyle = "TableStyleMedium2",
    withFilter = TRUE
  )
  openxlsx::freezePane(workbook, sheet = sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(
    workbook,
    sheet = sheet_name,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )
}

write_or_replace_sheet(
  wb,
  "Spatial classification",
  spatial_results |>
    dplyr::arrange(ctvi_rank)
)
write_or_replace_sheet(
  wb,
  "Typology spatial summary",
  typology_spatial_summary
)
write_or_replace_sheet(
  wb,
  "Environment spatial summary",
  environment_spatial_summary
)
write_or_replace_sheet(wb, "Key insights", key_insights)
write_or_replace_sheet(wb, "Moran sensitivity", moran_sensitivity)

openxlsx::saveWorkbook(wb, workbook_file, overwrite = TRUE)

message(
  "Hotspot, typology and publication-map outputs complete. See 04_maps and 06_tables."
)