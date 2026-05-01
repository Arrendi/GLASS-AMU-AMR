# ============================================================
# Nonlinear threshold modelling for country stewardship data
# ============================================================

# -----------------------------
# 1. Packages
# -----------------------------
required_packages <- c(
  "dplyr",
  "ggplot2",
  "readr",
  "tibble",
  "drc")

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed) install.packages(pkg, dependencies = TRUE)
}
invisible(lapply(required_packages, library, character.only = TRUE))

ggplot2::theme_set(ggplot2::theme_minimal(base_size = 12))

# -----------------------------
# 2. Output directory
# -----------------------------
output_dir <- "amr_glass_project_outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 3. Helper function
# -----------------------------
fit_threshold_models <- function(data, x_var, y_var, model_prefix, output_dir) {
  # Prepare modelling dataset
  dat <- data %>%
    dplyr::ungroup() %>%
    dplyr::select(country, dplyr::all_of(c(x_var, y_var))) %>%
    dplyr::filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]])) %>%
    dplyr::arrange(.data[[x_var]])
  
  if (nrow(dat) < 10) {
    stop("Too few complete observations for nonlinear modelling.")
  }
  
  # Build formula
  fml <- stats::as.formula(paste0(y_var, " ~ ", x_var))
  
  # Candidate models
  # LL.4 = log-logistic
  # W1.4 = Weibull type 1
  # LN.4 = log-normal
  # We fit all, but do not assume all will be stable
  fit_list <- list()
  
  fit_list[["LL.4"]] <- try(
    drc::drm(fml, data = dat, fct = drc::LL.4()),
    silent = TRUE
  )
  
  fit_list[["W1.4"]] <- try(
    drc::drm(fml, data = dat, fct = drc::W1.4()),
    silent = TRUE
  )
  
  fit_list[["LN.4"]] <- try(
    drc::drm(fml, data = dat, fct = drc::LN.4()),
    silent = TRUE
  )
  
  # Keep successful fits only
  good_fits <- fit_list[!vapply(fit_list, inherits, logical(1), what = "try-error")]
  
  if (length(good_fits) == 0) {
    stop("All nonlinear candidate models failed.")
  }
  
  # Compare AIC
  model_compare <- tibble::tibble(
    model = names(good_fits),
    AIC = vapply(good_fits, AIC, numeric(1))
  ) %>%
    dplyr::arrange(AIC)
  
  readr::write_csv(
    model_compare,
    file.path(output_dir, paste0(model_prefix, "_model_comparison.csv"))
  )
  
  # Start with lowest AIC model
  best_model_name <- model_compare$model[1]
  best_model <- good_fits[[best_model_name]]
  
  # Prediction grid as plain data.frame
  x_seq <- seq(
    min(dat[[x_var]], na.rm = TRUE),
    max(dat[[x_var]], na.rm = TRUE),
    length.out = 200
  )
  
  newdat <- data.frame(x_seq)
  names(newdat) <- x_var
  
  # Safe prediction helper
  safe_predict <- function(model, newdata) {
    pred <- try(predict(model, newdata = newdata), silent = TRUE)
    if (inherits(pred, "try-error")) return(NULL)
    if (is.matrix(pred) || is.data.frame(pred)) pred <- pred[, 1]
    as.numeric(pred)
  }
  
  pred_vals <- safe_predict(best_model, newdat)
  
  # Fallback to LL.4 if selected model predicts badly
  if (is.null(pred_vals) || any(!is.finite(pred_vals))) {
    if ("LL.4" %in% names(good_fits)) {
      best_model_name <- "LL.4 (fallback)"
      best_model <- good_fits[["LL.4"]]
      pred_vals <- safe_predict(best_model, newdat)
    }
  }
  
  if (is.null(pred_vals) || any(!is.finite(pred_vals))) {
    stop("Prediction failed even after fallback.")
  }
  
  pred_dat <- tibble::tibble(
    x = newdat[[x_var]],
    fitted = pred_vals
  )
  
  # Approximate threshold-style summary:
  # locate the point of steepest absolute slope on the fitted curve
  slope_dat <- pred_dat %>%
    dplyr::mutate(
      dx = c(NA, diff(x)),
      dy = c(NA, diff(fitted)),
      slope = dy / dx,
      abs_slope = abs(slope)
    )
  
  threshold_row <- slope_dat %>%
    dplyr::filter(!is.na(abs_slope)) %>%
    dplyr::arrange(dplyr::desc(abs_slope)) %>%
    dplyr::slice(1)
  
  threshold_summary <- tibble::tibble(
    x_var = x_var,
    y_var = y_var,
    selected_model = best_model_name,
    threshold_x = threshold_row$x,
    fitted_y_at_threshold = threshold_row$fitted,
    local_slope = threshold_row$slope
  )
  
  readr::write_csv(
    threshold_summary,
    file.path(output_dir, paste0(model_prefix, "_threshold_summary.csv"))
  )
  
  # Save fitted model summary
  capture.output(
    summary(best_model),
    file = file.path(output_dir, paste0(model_prefix, "_best_model_summary.txt"))
  )
  
  # Plot
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::geom_line(
      data = pred_dat,
      ggplot2::aes(x = x, y = fitted),
      linewidth = 1.2
    ) +
    ggplot2::geom_vline(
      xintercept = threshold_row$x,
      linetype = 2,
      colour = "firebrick"
    ) +
    ggplot2::labs(
      title = paste("Nonlinear threshold analysis:", x_var, "vs", y_var),
      subtitle = paste("Selected model:", best_model_name,
                       "| Approximate tipping point at", round(threshold_row$x, 2)),
      x = x_var,
      y = y_var
    )
  
  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(model_prefix, "_threshold_plot.png")),
    plot = p,
    width = 9,
    height = 6,
    dpi = 300
  )
  
  list(
    data = dat,
    model_compare = model_compare,
    best_model_name = best_model_name,
    best_model = best_model,
    predictions = pred_dat,
    threshold_summary = threshold_summary,
    plot = p
  )
}

# -----------------------------
# 4. Example analyses
# -----------------------------
# Assumes you already built `panel`

# A. Watch share -> AST coverage
res_watch_ast <- fit_threshold_models(
  data = panel,
  x_var = "watch_pct",
  y_var = "ast_pct",
  model_prefix = "watch_ast",
  output_dir = output_dir)

# B. Total DID -> broad-spectrum proxy
res_did_broad <- fit_threshold_models(
  data = panel,
  x_var = "did_total",
  y_var = "broad_proxy_pct",
  model_prefix = "did_broad",
  output_dir = output_dir)

# C. Access share -> AST coverage
res_access_ast <- fit_threshold_models(
  data = panel,
  x_var = "access_pct_final",
  y_var = "ast_pct",
  model_prefix = "access_ast",
  output_dir = output_dir)

# -----------------------------
# 5. Combined threshold summary
# -----------------------------
all_thresholds <- dplyr::bind_rows(
  res_watch_ast$threshold_summary,
  res_did_broad$threshold_summary,
  res_access_ast$threshold_summary)

readr::write_csv(
  all_thresholds,
  file.path(output_dir, "all_threshold_summaries.csv"))

print(all_thresholds)