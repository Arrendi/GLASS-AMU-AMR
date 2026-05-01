# ============================================================
# GLASS AMU/AMR Project
# Full end-to-end workflow
# - builds panel from raw CSVs
# - descriptive stewardship profiling
# - PCA-based stewardship types
# - nonlinear threshold modelling
# ============================================================

# -----------------------------
# 1. Packages
# -----------------------------
required_packages <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "readr",
  "stringr",
  "forcats",
  "janitor",
  "tibble",
  "cluster",
  "scales",
  "drc")

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed) install.packages(pkg, dependencies = TRUE)
}
invisible(lapply(required_packages, library, character.only = TRUE))

ggplot2::theme_set(ggplot2::theme_minimal(base_size = 12))

# -----------------------------
# 2. Paths
# -----------------------------
# Put these files in your working directory or edit the paths here
file_country_totals <- "optionA_country_totals.csv"
file_testing_cov    <- "Testing_coverage.csv"
file_aware          <- "amu_AWaRe.csv"
file_atc3           <- "amu_pharmacological.csv"
file_atc4           <- "amu_atc4.csv"

output_dir <- "amr_glass_project_outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 3. Helper functions
# -----------------------------
safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

clean_country <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("T<U\\+00FC>rkiye", "Türkiye") %>%
    stringr::str_replace_all("Netherlands \\(Kingdom of the\\)", "Netherlands") %>%
    stringr::str_replace_all("United Kingdom of Great Britain and Northern Ireland", "United Kingdom") %>%
    stringr::str_replace_all("Russian Federation", "Russia") %>%
    stringr::str_replace_all("Republic of Moldova", "Moldova")
}

save_plot <- function(plot_obj, filename, width = 9, height = 6) {
  ggplot2::ggsave(
    filename = file.path(output_dir, filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = 300
  )
}

# -----------------------------
# 4. Read raw files
# -----------------------------
country_totals_raw <- readr::read_csv(file_country_totals, show_col_types = FALSE) %>%
  janitor::clean_names()

testing_cov_raw <- readr::read_csv(file_testing_cov, show_col_types = FALSE) %>%
  janitor::clean_names()

aware_raw <- readr::read_csv(file_aware, show_col_types = FALSE) %>%
  janitor::clean_names()

atc3_raw <- readr::read_csv(file_atc3, show_col_types = FALSE) %>%
  janitor::clean_names()

atc4_raw <- readr::read_csv(file_atc4, show_col_types = FALSE) %>%
  janitor::clean_names()

# Optional audit
readr::write_lines(names(country_totals_raw), file.path(output_dir, "columns_country_totals.txt"))
readr::write_lines(names(testing_cov_raw), file.path(output_dir, "columns_testing_coverage.txt"))
readr::write_lines(names(aware_raw), file.path(output_dir, "columns_aware.txt"))
readr::write_lines(names(atc3_raw), file.path(output_dir, "columns_atc3.txt"))
readr::write_lines(names(atc4_raw), file.path(output_dir, "columns_atc4.txt"))

# -----------------------------
# 5. Clean and standardise datasets
# -----------------------------
country_totals <- country_totals_raw %>%
  dplyr::mutate(
    country = clean_country(country_name),
    who_region = who_region_name,
    did_total = safe_numeric(did_total),
    access_did = safe_numeric(access_did),
    access_pct = safe_numeric(access_pct)) %>%
  dplyr::transmute(
    country_code,
    country,
    who_region,
    did_total,
    access_did,
    access_pct)

testing_cov <- testing_cov_raw %>%
  dplyr::mutate(
    country = clean_country(country_territory_area),
    bci_per_million = safe_numeric(total_acinetobacter_spp_bc_is_per_million_population),
    abs_bci = safe_numeric(absolute_number_of_acinetobacter_spp_bc_is),
    ast_bci_per_million = safe_numeric(total_acinetobacter_spp_bc_is_with_ast_carbapenems_per_million_population),
    abs_bci_with_ast = safe_numeric(absolute_number_of_acinetobacter_spp_bc_is_with_ast_carbapenems),
    ast_pct = safe_numeric(percent_of_acinetobacter_spp_bc_is_with_ast)
  ) %>%
  dplyr::transmute(
    country,
    bci_per_million,
    abs_bci,
    ast_bci_per_million,
    abs_bci_with_ast,
    ast_pct)

aware <- aware_raw %>%
  dplyr::mutate(
    country = clean_country(country_territory_area),
    who_region = who_region_name,
    awr_name = stringr::str_squish(awr_name),
    did = safe_numeric(did),
    pct = safe_numeric(percentage_percent)) %>%
  dplyr::transmute(
    country,
    who_region,
    awr_name,
    did,
    pct)

atc3 <- atc3_raw %>%
  dplyr::mutate(
    country = clean_country(country_territory_area),
    who_region = who_region_name,
    atc3 = stringr::str_squish(atc3),
    atc_name = stringr::str_squish(atc_name),
    did = safe_numeric(did),
    pct = safe_numeric(percentage_percent)) %>%
  dplyr::transmute(
    country,
    who_region,
    atc3,
    atc_name,
    did,
    pct)

atc4 <- atc4_raw %>%
  dplyr::mutate(
    country = clean_country(country_territory_area),
    who_region = who_region_name,
    atc4 = stringr::str_squish(atc4),
    atc_name = stringr::str_squish(atc_name),
    did = safe_numeric(did),
    pct = safe_numeric(percentage_percent)) %>%
  dplyr::transmute(
    country,
    who_region,
    atc4,
    atc_name,
    did,
    pct)

# -----------------------------
# 6. AWaRe summaries
# -----------------------------
aware_summary <- aware %>%
  dplyr::mutate(
    aware_group = dplyr::case_when(
      stringr::str_to_lower(awr_name) == "access" ~ "Access",
      stringr::str_to_lower(awr_name) == "watch" ~ "Watch",
      stringr::str_to_lower(awr_name) == "reserve" ~ "Reserve",
      TRUE ~ awr_name)) %>%
  dplyr::group_by(country, who_region, aware_group) %>%
  dplyr::summarise(
    did = sum(did, na.rm = TRUE),
    pct = sum(pct, na.rm = TRUE),
    .groups = "drop" )

aware_wide <- aware_summary %>%
  dplyr::select(country, aware_group, did, pct) %>%
  tidyr::pivot_wider(
    names_from = aware_group,
    values_from = c(did, pct),
    values_fill = 0) %>%
  dplyr::rename(
    access_did_from_aware = did_Access,
    watch_did = did_Watch,
    reserve_did = did_Reserve,
    access_pct_from_aware = pct_Access,
    watch_pct = pct_Watch,
    reserve_pct = pct_Reserve)

# -----------------------------
# 7. ATC3 summaries and broad-spectrum proxy
# -----------------------------
atc3_summary <- atc3 %>%
  dplyr::group_by(country, atc3, atc_name) %>%
  dplyr::summarise(
    did = sum(did, na.rm = TRUE),
    pct = sum(pct, na.rm = TRUE),
    .groups = "drop")

top_atc3_wide <- atc3_summary %>%
  dplyr::group_by(country) %>%
  dplyr::arrange(dplyr::desc(did), .by_group = TRUE) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  dplyr::filter(rank <= 3) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    country,
    rank,
    label = paste0(atc_name, " [", round(pct, 1), "%]")) %>%
  tidyr::pivot_wider(
    names_from = rank,
    values_from = label,
    names_prefix = "top_atc3_")

broad_spectrum_atc3 <- c("J01C", "J01D", "J01F", "J01M")

broad_proxy <- atc3_summary %>%
  dplyr::mutate(is_broad_proxy = atc3 %in% broad_spectrum_atc3) %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(
    broad_proxy_did = sum(did[is_broad_proxy], na.rm = TRUE),
    broad_proxy_pct = sum(pct[is_broad_proxy], na.rm = TRUE),
    .groups = "drop")

# -----------------------------
# 8. ATC4 summaries
# -----------------------------
atc4_summary <- atc4 %>%
  dplyr::group_by(country, atc4, atc_name) %>%
  dplyr::summarise(
    did = sum(did, na.rm = TRUE),
    pct = sum(pct, na.rm = TRUE),
    .groups = "drop")

top_atc4_wide <- atc4_summary %>%
  dplyr::group_by(country) %>%
  dplyr::arrange(dplyr::desc(did), .by_group = TRUE) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  dplyr::filter(rank <= 5) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    country,
    rank,
    label = paste0(atc_name, " [", round(pct, 1), "%]")) %>%
  tidyr::pivot_wider(
    names_from = rank,
    values_from = label,
    names_prefix = "top_atc4_")

# -----------------------------
# 9. Build analytic panel from scratch
# -----------------------------
panel <- country_totals %>%
  dplyr::left_join(aware_wide, by = "country") %>%
  dplyr::left_join(broad_proxy, by = "country") %>%
  dplyr::left_join(testing_cov, by = "country") %>%
  dplyr::left_join(top_atc3_wide, by = "country") %>%
  dplyr::left_join(top_atc4_wide, by = "country") %>%
  dplyr::mutate(
    access_pct_final = dplyr::coalesce(access_pct, access_pct_from_aware),
    access_watch_ratio = dplyr::if_else(
      !is.na(watch_pct) & watch_pct > 0,
      access_pct_final / watch_pct,
      NA_real_),
    stewardship_score =
      0.30 * access_pct_final +
      0.20 * (100 - watch_pct) +
      0.10 * (100 - reserve_pct) +
      0.20 * (100 - broad_proxy_pct) +
      0.20 * ast_pct)

readr::write_csv(panel, file.path(output_dir, "glass_country_stewardship_panel.csv"))

# -----------------------------
# 10. Descriptive plots
# -----------------------------
p_access <- panel %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(country), !is.na(access_pct_final)) %>%
  dplyr::arrange(access_pct_final) %>%
  dplyr::mutate(country = factor(country, levels = unique(country))) %>%
  ggplot2::ggplot(ggplot2::aes(x = access_pct_final, y = country)) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::geom_vline(xintercept = 60, linetype = 2, colour = "firebrick") +
  ggplot2::labs(
    title = "Country ranking by Access antibiotic share",
    subtitle = "Red dashed line = 60% stewardship reference threshold",
    x = "Access share (%)",
    y = NULL)

save_plot(p_access, "access_share_ranking.png", width = 9, height = 8)

p_score <- panel %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(country), !is.na(stewardship_score)) %>%
  dplyr::arrange(stewardship_score) %>%
  dplyr::mutate(country = factor(country, levels = unique(country))) %>%
  ggplot2::ggplot(ggplot2::aes(x = stewardship_score, y = country)) +
  ggplot2::geom_point(size = 2.6) +
  ggplot2::labs(
    title = "Composite stewardship and surveillance-readiness score",
    x = "Stewardship score",
    y = NULL)

save_plot(p_score, "stewardship_score_ranking.png", width = 10, height = 8)

p_watch_testing <- panel %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(watch_pct), !is.na(ast_pct), !is.na(did_total)) %>%
  ggplot2::ggplot(ggplot2::aes(x = watch_pct, y = ast_pct, size = did_total)) +
  ggplot2::geom_point(alpha = 0.85) +
  ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = 2, colour = "grey40") +
  ggplot2::labs(
    title = "Watch share versus AST testing coverage",
    subtitle = "Bubble size reflects total DID",
    x = "Watch share (%)",
    y = "AST coverage (%)")

save_plot(p_watch_testing, "watch_vs_testing.png", width = 9, height = 6)

p_did_broad <- panel %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(did_total), !is.na(broad_proxy_pct)) %>%
  ggplot2::ggplot(ggplot2::aes(x = did_total, y = broad_proxy_pct)) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = 2, colour = "grey40") +
  ggplot2::labs(
    title = "Total antibiotic consumption versus broad-spectrum proxy",
    x = "Total DID",
    y = "Broad-spectrum proxy share (%)")

save_plot(p_did_broad, "did_vs_broad_proxy.png", width = 9, height = 6)

heat_dat <- panel %>%
  dplyr::ungroup() %>%
  dplyr::select(country, access_pct_final, watch_pct, reserve_pct, broad_proxy_pct, ast_pct) %>%
  tidyr::pivot_longer(-country, names_to = "indicator", values_to = "value")

country_order <- heat_dat %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(mean_value)) %>%
  dplyr::pull(country)

heat_dat <- heat_dat %>%
  dplyr::mutate(country = factor(country, levels = country_order))

p_heat <- ggplot2::ggplot(heat_dat, ggplot2::aes(x = indicator, y = country, fill = value)) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::scale_fill_viridis_c(na.value = "grey80") +
  ggplot2::labs(
    title = "Country heatmap of stewardship and readiness indicators",
    x = NULL,
    y = NULL,
    fill = "Value")

save_plot(p_heat, "indicator_heatmap.png", width = 10, height = 8)

# -----------------------------
# 11. PCA-based profile extraction
# -----------------------------
pca_dat <- panel %>%
  dplyr::ungroup() %>%
  dplyr::select(
    country,
    did_total,
    access_pct_final,
    watch_pct,
    reserve_pct,
    broad_proxy_pct,
    ast_pct,
    access_watch_ratio) %>%
  tidyr::drop_na()

X <- pca_dat %>%
  dplyr::select(-country) %>%
  scale(center = TRUE, scale = TRUE)

rownames(X) <- pca_dat$country

pca_fit <- stats::prcomp(X, center = FALSE, scale. = FALSE)

pca_var <- tibble::tibble(
  component = paste0("PC", seq_along(pca_fit$sdev)),
  eigenvalue = pca_fit$sdev^2,
  prop_var = (pca_fit$sdev^2) / sum(pca_fit$sdev^2),
  cum_var = cumsum((pca_fit$sdev^2) / sum(pca_fit$sdev^2)))

pca_scores <- as.data.frame(pca_fit$x) %>%
  tibble::rownames_to_column("country")

pca_loadings <- as.data.frame(pca_fit$rotation) %>%
  tibble::rownames_to_column("variable")

readr::write_csv(pca_var, file.path(output_dir, "pca_variance_explained.csv"))
readr::write_csv(pca_scores, file.path(output_dir, "pca_scores.csv"))
readr::write_csv(pca_loadings, file.path(output_dir, "pca_loadings.csv"))

p_scree <- ggplot2::ggplot(pca_var, ggplot2::aes(x = component, y = prop_var)) +
  ggplot2::geom_col() +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::labs(
    title = "Variance explained by principal components",
    x = NULL,
    y = "Proportion of variance")

save_plot(p_scree, "pca_scree_plot.png", width = 8, height = 5)

p_pca_space <- ggplot2::ggplot(pca_scores, ggplot2::aes(x = PC1, y = PC2)) +
  ggplot2::geom_point(size = 2.8) +
  ggplot2::labs(
    title = "Country stewardship profile space",
    subtitle = "PCA of GLASS stewardship and surveillance indicators",
    x = "PC1",
    y = "PC2")

save_plot(p_pca_space, "glass_pca_country_space.png", width = 9, height = 6)

p_loading <- pca_loadings %>%
  dplyr::select(variable, PC1, PC2) %>%
  ggplot2::ggplot(ggplot2::aes(x = PC1, y = PC2, label = variable)) +
  ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey60") +
  ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::geom_text(nudge_y = 0.03, check_overlap = TRUE) +
  ggplot2::labs(
    title = "PCA loading plot",
    x = "PC1 loading",
    y = "PC2 loading")

save_plot(p_loading, "pca_loading_plot.png", width = 8, height = 6)

# -----------------------------
# 12. Cluster countries into stewardship types
# -----------------------------
cluster_scores <- pca_scores %>%
  dplyr::select(country, dplyr::any_of(c("PC1", "PC2", "PC3")))

cluster_matrix <- cluster_scores %>%
  dplyr::select(-country) %>%
  as.matrix()

rownames(cluster_matrix) <- cluster_scores$country

silhouette_results <- lapply(2:6, function(k) {
  set.seed(123)
  km <- stats::kmeans(cluster_matrix, centers = k, nstart = 100)
  sil <- cluster::silhouette(km$cluster, dist(cluster_matrix))
  tibble::tibble(
    k = k,
    avg_sil_width = mean(sil[, "sil_width"])
  )
}) %>%
  dplyr::bind_rows()

readr::write_csv(silhouette_results, file.path(output_dir, "cluster_silhouette_results.csv"))

best_k <- silhouette_results %>%
  dplyr::arrange(dplyr::desc(avg_sil_width)) %>%
  dplyr::slice(1) %>%
  dplyr::pull(k)

set.seed(123)
km_best <- stats::kmeans(cluster_matrix, centers = best_k, nstart = 100)

country_clusters <- cluster_scores %>%
  dplyr::mutate(cluster = factor(km_best$cluster))

readr::write_csv(country_clusters, file.path(output_dir, "country_pca_clusters.csv"))

cluster_summary <- country_clusters %>%
  dplyr::left_join(pca_dat, by = "country") %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise(
    n_countries = dplyr::n(),
    did_total = mean(did_total, na.rm = TRUE),
    access_pct_final = mean(access_pct_final, na.rm = TRUE),
    watch_pct = mean(watch_pct, na.rm = TRUE),
    reserve_pct = mean(reserve_pct, na.rm = TRUE),
    broad_proxy_pct = mean(broad_proxy_pct, na.rm = TRUE),
    ast_pct = mean(ast_pct, na.rm = TRUE),
    access_watch_ratio = mean(access_watch_ratio, na.rm = TRUE),
    .groups = "drop")

readr::write_csv(cluster_summary, file.path(output_dir, "cluster_summary_on_original_indicators.csv"))

p_cluster_space <- ggplot2::ggplot(country_clusters, ggplot2::aes(x = PC1, y = PC2, colour = cluster)) +
  ggplot2::geom_point(size = 3) +
  ggplot2::labs(
    title = "Country stewardship types from PCA + clustering",
    subtitle = paste("Best cluster count selected by silhouette width:", best_k),
    x = "PC1",
    y = "PC2",
    colour = "Cluster")

save_plot(p_cluster_space, "country_stewardship_types_pca_clusters.png", width = 9, height = 6)

panel_with_types <- panel %>%
  dplyr::left_join(
    country_clusters %>% dplyr::select(country, cluster),
    by = "country")

readr::write_csv(panel_with_types, file.path(output_dir, "panel_with_stewardship_types.csv"))

# -----------------------------
# 13. Nonlinear threshold modelling
# -----------------------------
# A. Watch share -> AST coverage
dose_dat_watch_ast <- panel %>%
  dplyr::select(country, watch_pct, ast_pct) %>%
  dplyr::filter(!is.na(watch_pct), !is.na(ast_pct)) %>%
  dplyr::arrange(watch_pct)

m_watch_ll4 <- drc::drm(ast_pct ~ watch_pct, data = dose_dat_watch_ast, fct = drc::LL.4())
m_watch_w1  <- drc::drm(ast_pct ~ watch_pct, data = dose_dat_watch_ast, fct = drc::W1.4())
m_watch_ln4 <- drc::drm(ast_pct ~ watch_pct, data = dose_dat_watch_ast, fct = drc::LN.4())

watch_model_compare <- tibble::tibble(
  model = c("LL.4", "W1.4", "LN.4"),
  AIC = c(AIC(m_watch_ll4), AIC(m_watch_w1), AIC(m_watch_ln4))) %>%
  dplyr::arrange(AIC)

readr::write_csv(watch_model_compare, file.path(output_dir, "watch_ast_model_comparison.csv"))

best_watch_model_name <- watch_model_compare$model[1]
best_watch_model <- switch(
  best_watch_model_name,
  "LL.4" = m_watch_ll4,
  "W1.4" = m_watch_w1,
  "LN.4" = m_watch_ln4)

new_watch <- data.frame(
  watch_pct = seq(
    min(dose_dat_watch_ast$watch_pct, na.rm = TRUE),
    max(dose_dat_watch_ast$watch_pct, na.rm = TRUE),
    length.out = 200))

pred_watch <- tibble::tibble(
  watch_pct = new_watch$watch_pct,
  fitted = as.numeric(stats::predict(best_watch_model, newdata = new_watch)))

p_watch_threshold <- ggplot2::ggplot(dose_dat_watch_ast, ggplot2::aes(x = watch_pct, y = ast_pct)) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::geom_line(data = pred_watch, ggplot2::aes(x = watch_pct, y = fitted), linewidth = 1.2) +
  ggplot2::labs(
    title = "Nonlinear association between Watch share and AST coverage",
    subtitle = paste("Best model by AIC:", best_watch_model_name),
    x = "Watch share (%)",
    y = "AST coverage (%)")

save_plot(p_watch_threshold, "drc_watch_ast_curve.png", width = 9, height = 6)

# B. DID -> broad-spectrum proxy
dose_dat_did_broad <- panel %>%
  dplyr::select(country, did_total, broad_proxy_pct) %>%
  dplyr::filter(!is.na(did_total), !is.na(broad_proxy_pct)) %>%
  dplyr::arrange(did_total)

m_did_ll4 <- drc::drm(broad_proxy_pct ~ did_total, data = dose_dat_did_broad, fct = drc::LL.4())
m_did_w1  <- drc::drm(broad_proxy_pct ~ did_total, data = dose_dat_did_broad, fct = drc::W1.4())
m_did_ln4 <- drc::drm(broad_proxy_pct ~ did_total, data = dose_dat_did_broad, fct = drc::LN.4())

did_model_compare <- tibble::tibble(
  model = c("LL.4", "W1.4", "LN.4"),
  AIC = c(AIC(m_did_ll4), AIC(m_did_w1), AIC(m_did_ln4))) %>%
  dplyr::arrange(AIC)

readr::write_csv(did_model_compare, file.path(output_dir, "did_broad_model_comparison.csv"))

best_did_model_name <- did_model_compare$model[1]
best_did_model <- switch(
  best_did_model_name,
  "LL.4" = m_did_ll4,
  "W1.4" = m_did_w1,
  "LN.4" = m_did_ln4)

# Safer prediction block for DID -> broad-spectrum model
new_did <- data.frame(
  did_total = seq(
    min(dose_dat_did_broad$did_total, na.rm = TRUE),
    max(dose_dat_did_broad$did_total, na.rm = TRUE),
    length.out = 200))

pred_vals_did <- try(predict(best_did_model, newdata = new_did), silent = TRUE)

if (inherits(pred_vals_did, "try-error")) {
  message("Prediction failed for best_did_model. Falling back to LL.4 model.")
  best_did_model <- m_did_ll4
  best_did_model_name <- "LL.4 (fallback)"
  pred_vals_did <- predict(best_did_model, newdata = new_did)
}

# If predict returns a matrix, keep the first column
if (is.matrix(pred_vals_did) || is.data.frame(pred_vals_did)) {
  pred_vals_did <- pred_vals_did[, 1]
}

pred_did <- tibble::tibble(
  did_total = new_did$did_total,
  fitted = as.numeric(pred_vals_did))

p_did_threshold <- ggplot2::ggplot(dose_dat_did_broad, ggplot2::aes(x = did_total, y = broad_proxy_pct)) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::geom_line(data = pred_did, ggplot2::aes(x = did_total, y = fitted), linewidth = 1.2) +
  ggplot2::labs(
    title = "Nonlinear association between total DID and broad-spectrum proxy",
    subtitle = paste("Best model by AIC:", best_did_model_name),
    x = "Total DID",
    y = "Broad-spectrum proxy (%)")

save_plot(p_did_threshold, "drc_did_broad_curve.png", width = 9, height = 6)

capture.output(
  summary(best_watch_model),
  file = file.path(output_dir, "best_watch_ast_model_summary.txt"))

capture.output(
  summary(best_did_model),
  file = file.path(output_dir, "best_did_broad_model_summary.txt"))

# -----------------------------
# 14. Optional linear check for AST coverage
# -----------------------------
model_dat_ast <- panel %>%
  dplyr::select(
    country,
    ast_pct,
    did_total,
    access_pct_final,
    watch_pct,
    reserve_pct,
    broad_proxy_pct) %>%
  tidyr::drop_na()

fit_ast <- stats::lm(
  ast_pct ~ did_total + access_pct_final + watch_pct + reserve_pct + broad_proxy_pct,
  data = model_dat_ast)

capture.output(
  summary(fit_ast),
  file = file.path(output_dir, "lm_ast_coverage_summary.txt"))

# -----------------------------
# 15. Console summary
# -----------------------------
cat("\nPanel rows:\n")
print(nrow(panel))

cat("\nPCA variance explained:\n")
print(pca_var)

cat("\nBest cluster count:\n")
print(best_k)

cat("\nCluster summary:\n")
print(cluster_summary)

cat("\nBest Watch -> AST model by AIC:\n")
print(watch_model_compare)

cat("\nBest DID -> broad-spectrum model by AIC:\n")
print(did_model_compare)

cat("\nOutputs saved to:\n", output_dir, "\n")