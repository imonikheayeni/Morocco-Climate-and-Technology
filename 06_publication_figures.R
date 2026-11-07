# =============================================================================
# 06_publication_figures.R
# Regenerates the manuscript figures (Figures 2-7) and the graphical abstract
# from the pipeline outputs. Figures are written to 07_manuscript/ as vector
# PDFs (cairo_pdf, fonts embedded) plus PNG previews.
#
# Figure 1 (analytical workflow) is a schematic and is not data-driven.
# The submitted manuscript uses single panels of these figures (Figs. 2-12) and a
# plain-language graphical abstract; see 07_manuscript/as_submitted/README.md.
# =============================================================================

hotspot_file <- file.path(path_ml, "morocco_ctvi_adm2_index_hotspots.gpkg")
stopifnot(file.exists(hotspot_file))

fig <- sf::st_read(hotspot_file, quiet = TRUE) |>
  sf::st_make_valid() |>
  sf::st_transform(4326)
fig_adm1 <- sf::st_read(file.path(path_int_spatial, "morocco_adm1.gpkg"), quiet = TRUE) |>
  sf::st_transform(4326)

# ---- Shared styling ---------------------------------------------------------
quintile_colours <- c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000")

typology_colours_fig <- c(
  "Critical vulnerability" = "#7f0000",
  "Climate-exposed adaptive potential" = "#f46d43",
  "Technology/service development gap" = "#4575b4",
  "Relative resilience" = "#91cf60"
)

measure_colours <- c(
  "Climate exposure" = "#3A78B5",
  "Technology/service-access gap" = "#3A9E8C",
  "CTVI" = "#6E5AA8"
)

bivariate_colours_fig <- c(
  "1-1" = "#e8e8e8", "2-1" = "#ace4e4", "3-1" = "#5ac8c8",
  "1-2" = "#dfb0d6", "2-2" = "#a5add3", "3-2" = "#5698b9",
  "1-3" = "#be64ac", "2-3" = "#8c62aa", "3-3" = "#3b4994"
)

gi_levels_fig <- c(
  "Hot spot (adj. p ≤ 0.05)",
  "Hot spot (adj. p ≤ 0.10)",
  "Not significant",
  "Cold spot (adj. p ≤ 0.10)",
  "Cold spot (adj. p ≤ 0.05)"
)
gi_colours_fig <- c("#ef8a62", "#fddbc7", "#f0f0f0", "#d1e5f0", "#67a9cf")
names(gi_colours_fig) <- gi_levels_fig

theme_fig_map <- ggplot2::theme_void(base_size = 9) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 10, hjust = 0, margin = ggplot2::margin(b = 3)),
    legend.title = ggplot2::element_text(face = "bold", size = 8),
    legend.text = ggplot2::element_text(size = 7.5),
    legend.key.size = grid::unit(0.38, "cm"),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.justification = "left",
    plot.margin = ggplot2::margin(2, 4, 2, 4)
  )

fig_quantile_classes <- function(x, n = 5, digits = 3) {
  breaks <- unique(as.numeric(stats::quantile(
    x[is.finite(x)], probs = seq(0, 1, length.out = n + 1), type = 8
  )))
  labels <- paste0(
    formatC(head(breaks, -1), format = "f", digits = digits),
    "–",
    formatC(tail(breaks, -1), format = "f", digits = digits)
  )
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE)
}

fig_tertile <- function(x) {
  breaks <- unique(as.numeric(stats::quantile(
    x[is.finite(x)], probs = c(0, 1 / 3, 2 / 3, 1), type = 8
  )))
  if (length(breaks) == 4) {
    as.integer(cut(x, breaks = breaks, labels = 1:3, include.lowest = TRUE))
  } else {
    dplyr::ntile(x, 3)
  }
}

fig_map <- function(fill_variable, title, values, legend_title, legend_ncol = 1) {
  ggplot2::ggplot(fig) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data[[fill_variable]]),
      colour = "grey55", linewidth = 0.06
    ) +
    ggplot2::geom_sf(data = fig_adm1, fill = NA, colour = "grey15", linewidth = 0.25) +
    ggplot2::coord_sf(datum = NA, expand = FALSE) +
    ggplot2::scale_fill_manual(
      values = values, drop = FALSE, na.value = "grey85", name = legend_title
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = legend_ncol)) +
    ggplot2::labs(title = title) +
    theme_fig_map
}

save_figure <- function(plot, name, width, height) {
  pdf_file <- file.path(path_manuscript, paste0(name, ".pdf"))
  ggplot2::ggsave(pdf_file, plot, width = width, height = height,
                  device = grDevices::cairo_pdf, bg = "white")
  ggplot2::ggsave(file.path(path_maps, paste0(name, "_preview.png")), plot,
                  width = width, height = height, dpi = 300, bg = "white")
  message("Saved ", basename(pdf_file))
}

# ---- Derived plotting variables ---------------------------------------------
fig <- fig |>
  dplyr::mutate(
    ctvi_class = fig_quantile_classes(ctvi_score),
    climate_class = fig_quantile_classes(climate_exposure_score),
    technology_class = fig_quantile_classes(technology_gap_score),
    priority_class = fig_quantile_classes(priority_pop_weighted),
    bivariate_class = paste(
      fig_tertile(climate_exposure_score),
      fig_tertile(technology_gap_score),
      sep = "-"
    ),
    resilience_typology = factor(resilience_typology, levels = names(typology_colours_fig)),
    gi_fig = factor(
      dplyr::case_when(
        gi_z > 0 & gi_p_fdr <= spatial_alpha ~ gi_levels_fig[1],
        gi_z > 0 & gi_p_fdr <= spatial_alpha_exploratory ~ gi_levels_fig[2],
        gi_z < 0 & gi_p_fdr <= spatial_alpha ~ gi_levels_fig[5],
        gi_z < 0 & gi_p_fdr <= spatial_alpha_exploratory ~ gi_levels_fig[4],
        TRUE ~ gi_levels_fig[3]
      ),
      levels = gi_levels_fig
    )
  )

# ---- Figure 2: CTVI and its components --------------------------------------
fig2 <- patchwork::wrap_plots(
  fig_map("ctvi_class", "(a)  CTVI", quintile_colours, "CTVI (quintiles)"),
  fig_map("climate_class", "(b)  Climate exposure", quintile_colours, "CE (quintiles)"),
  fig_map("technology_class", "(c)  Access gap", quintile_colours, "TG (quintiles)"),
  nrow = 1
)
save_figure(fig2, "Figure_2", width = 7.2, height = 4.5)

# ---- Figure 3: typologies and bivariate map ---------------------------------
fig3a <- fig_map(
  "resilience_typology", "(a)  Planning typologies",
  typology_colours_fig, "Typology (median splits)"
)

fig3b_map <- ggplot2::ggplot(fig) +
  ggplot2::geom_sf(ggplot2::aes(fill = bivariate_class), colour = "grey55", linewidth = 0.06) +
  ggplot2::geom_sf(data = fig_adm1, fill = NA, colour = "grey15", linewidth = 0.25) +
  ggplot2::coord_sf(datum = NA, expand = FALSE) +
  ggplot2::scale_fill_manual(values = bivariate_colours_fig, guide = "none") +
  ggplot2::labs(title = "(b)  Bivariate tertile classes") +
  theme_fig_map

bivariate_key <- tidyr::expand_grid(ce = 1:3, tg = 1:3) |>
  dplyr::mutate(bivariate_class = paste(ce, tg, sep = "-"))
fig3b_key <- ggplot2::ggplot(bivariate_key, ggplot2::aes(ce, tg, fill = bivariate_class)) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
  ggplot2::scale_fill_manual(values = bivariate_colours_fig, guide = "none") +
  ggplot2::scale_x_continuous(breaks = 1:3, labels = c("Low", "Mid", "High"), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(breaks = 1:3, labels = c("Low", "Mid", "High"), expand = c(0, 0)) +
  ggplot2::coord_fixed() +
  ggplot2::labs(x = "Climate exposure →", y = "Access gap →") +
  ggplot2::theme_minimal(base_size = 7.5) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(face = "bold", size = 7.5)
  )

fig3b <- fig3b_map / fig3b_key + patchwork::plot_layout(heights = c(4, 1.25))
fig3 <- patchwork::wrap_plots(fig3a, fig3b, nrow = 1)
save_figure(fig3, "Figure_3", width = 7.2, height = 5.0)

# ---- Figure 4: environment comparison ---------------------------------------
env_ci <- readr::read_csv(
  file.path(path_tables, "environment_summary_with_ci.csv"),
  show_col_types = FALSE
)
env_counts <- fig |>
  sf::st_drop_geometry() |>
  dplyr::count(environment_type, name = "n_units")

measure_names <- c(
  climate_exposure_score = "Climate exposure",
  technology_gap_score = "Technology/service-access gap",
  ctvi_score = "CTVI"
)
env_label <- function(x) {
  x <- unname(environment_short_labels[as.character(x)])
  stringr::str_replace(x, "Rural/lower-density", "Rural/\nlower-density")
}
env_levels <- env_label(allowed_environment_types)

fig4a_data <- env_ci |>
  dplyr::mutate(
    environment = factor(env_label(environment_type), levels = env_levels),
    measure = factor(unname(measure_names[measure]), levels = unname(measure_names))
  )

fig4a <- ggplot2::ggplot(fig4a_data, ggplot2::aes(environment, mean, fill = measure)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.84), width = 0.78) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = ci_low, ymax = ci_high),
    position = ggplot2::position_dodge(width = 0.84), width = 0.2, linewidth = 0.35
  ) +
  ggplot2::geom_text(
    ggplot2::aes(y = ci_high + 0.035, label = formatC(mean, format = "f", digits = 2)),
    position = ggplot2::position_dodge(width = 0.84), size = 2.5
  ) +
  ggplot2::scale_fill_manual(values = measure_colours, name = NULL) +
  ggplot2::scale_y_continuous(limits = c(0, 1.08), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
  ggplot2::labs(
    title = "(a) Mean scores by environment",
    x = NULL, y = "Mean normalised score (95% CI)"
  ) +
  ggplot2::theme_minimal(base_size = 8.5) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 9.5),
    legend.position = "bottom", legend.direction = "vertical",
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

composition <- readr::read_csv(
  file.path(path_tables, "typology_composition_by_environment.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(
    environment = factor(env_label(environment_type), levels = env_levels),
    resilience_typology = factor(resilience_typology, levels = rev(names(typology_colours_fig))),
    label = dplyr::if_else(percent >= 6, paste0(round(percent), "%"), "")
  )

fig4b <- ggplot2::ggplot(composition, ggplot2::aes(environment, percent, fill = resilience_typology)) +
  ggplot2::geom_col(width = 0.72, colour = "white", linewidth = 0.3) +
  # `group` is set explicitly so the labels stack in the same order as the bars.
  # Without it, the logical `colour` mapping changes the grouping and swaps the
  # labels of adjacent segments (seen in the peri-urban bar).
  ggplot2::geom_text(
    ggplot2::aes(label = label,
                 group = resilience_typology,
                 colour = resilience_typology %in% c("Critical vulnerability",
                                                     "Technology/service development gap")),
    position = ggplot2::position_stack(vjust = 0.5), size = 2.6, show.legend = FALSE
  ) +
  ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey10"), guide = "none") +
  ggplot2::scale_fill_manual(
    values = typology_colours_fig, name = NULL,
    breaks = names(typology_colours_fig)
  ) +
  ggplot2::scale_y_continuous(limits = c(0, 100.5), expand = c(0, 0)) +
  ggplot2::labs(title = "(b) Typology composition", x = NULL, y = "Share of ADM2 units (%)") +
  ggplot2::theme_minimal(base_size = 8.5) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 9.5),
    legend.position = "bottom", legend.direction = "vertical",
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

fig4 <- patchwork::wrap_plots(fig4a, fig4b, nrow = 1)
save_figure(fig4, "Figure_4", width = 7.2, height = 4.8)

# ---- Figure 5: local spatial statistics -------------------------------------
fig5a <- fig_map("gi_fig", "(a)  Getis–Ord Gi*", gi_colours_fig, "Gi* class (BH-adjusted)")

lisa_sig_n <- sum(fig$lisa != "Not significant", na.rm = TRUE)
lisa_values <- c(
  "High-High (vulnerable cluster)" = "#b2182b",
  "Low-Low (resilient cluster)" = "#2166ac",
  "High-Low (outlier)" = "#ef8a62",
  "Low-High (outlier)" = "#67a9cf",
  "Not significant" = "#f0f0f0"
)
if (lisa_sig_n == 0) {
  fig$lisa_fig <- factor(
    rep(glue::glue("Not significant at 5% (all {nrow(fig)} units)"), nrow(fig))
  )
  lisa_values <- stats::setNames("#f0f0f0", levels(fig$lisa_fig))
} else {
  fig$lisa_fig <- factor(fig$lisa, levels = names(lisa_values))
}
fig5b <- fig_map("lisa_fig", "(b)  Local Moran's I", lisa_values, "Local Moran's I (BH-adjusted)")

fig5 <- patchwork::wrap_plots(fig5a, fig5b, nrow = 1)
save_figure(fig5, "Figure_5", width = 7.2, height = 5.0)

# ---- Figure 6: population-weighted priority ---------------------------------
fig6 <- fig_map("priority_class", NULL, quintile_colours, "Priority score (quintiles)") +
  ggplot2::theme(legend.position = "right", legend.justification = "center")
save_figure(fig6, "Figure_6", width = 6.0, height = 4.4)

# ---- Figure 7: Random Forest diagnostic -------------------------------------
rf_importance_fig <- readr::read_csv(
  file.path(path_ml, "rf_index_sensitivity_importance.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(
    label = unname(indicator_labels[variable]),
    domain = dplyr::if_else(
      variable %in% preferred_technology_features,
      "Access gap (TG)", "Climate exposure (CE)"
    ),
    label = forcats::fct_reorder(label, `%IncMSE`)
  )

fig7 <- ggplot2::ggplot(rf_importance_fig, ggplot2::aes(`%IncMSE`, label, fill = domain)) +
  ggplot2::geom_col(width = 0.68) +
  ggplot2::scale_fill_manual(
    values = c("Climate exposure (CE)" = "#3A78B5", "Access gap (TG)" = "#3A9E8C"),
    breaks = c("Climate exposure (CE)", "Access gap (TG)"),
    name = NULL
  ) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
  ggplot2::labs(x = "Increase in prediction error after permutation (%)", y = NULL) +
  ggplot2::theme_minimal(base_size = 9) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )
save_figure(fig7, "Figure_7", width = 6.0, height = 3.1)

# ---- Graphical abstract -----------------------------------------------------
critical_n <- sum(fig$resilience_typology == "Critical vulnerability", na.rm = TRUE)
hot_n <- sum(grepl("^Hot", as.character(fig$gi_fig)))
ga_a <- fig_map("resilience_typology", "Planning typologies", typology_colours_fig, NULL) +
  ggplot2::theme(legend.position = "right", legend.justification = "center")
ga_b <- fig_map("gi_fig", "Spatial hot spots (Gi*)", gi_colours_fig, NULL) +
  ggplot2::theme(legend.position = "right", legend.justification = "center")
graphical_abstract <- patchwork::wrap_plots(ga_a, ga_b, nrow = 1) +
  patchwork::plot_annotation(
    title = "Where climate exposure meets weak mapped access in Morocco",
    subtitle = glue::glue(
      "Open-data index for {nrow(fig)} provinces and prefectures: ",
      "{critical_n} units combine above-median exposure and access gap; ",
      "{hot_n} FDR-adjusted Gi* hot spots form a southern corridor."
    ),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      plot.subtitle = ggplot2::element_text(size = 9)
    )
  )
ggplot2::ggsave(
  file.path(path_manuscript, "Graphical_abstract.pdf"), graphical_abstract,
  width = 10, height = 4.6, device = grDevices::cairo_pdf, bg = "white"
)
ggplot2::ggsave(
  file.path(path_manuscript, "Graphical_abstract.png"), graphical_abstract,
  width = 10, height = 4.6, dpi = 300, bg = "white"
)

message("Publication figures written to ", path_manuscript)
