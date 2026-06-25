# ==========================================================================
# helpers.R  --  shared, stateless helper + plotting functions
# Sourced by global.R so every module can use them. Ported verbatim from the
# legacy server. (AI/OpenAI helpers are intentionally NOT here yet.)
# ==========================================================================

# Null-coalescing: return a if non-null and non-empty, else b.
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# Render a plotting function to an off-screen PNG and return base64 (for AI vision).
# Returns NULL if there is nothing to draw or the plot errors.
capture_plot_as_base64 <- function(plot_fn) {
  if (!is.function(plot_fn)) return(NULL)
  tmp <- tempfile(fileext = ".png")
  ok <- TRUE
  grDevices::png(tmp, width = 900, height = 650)
  tryCatch(plot_fn(), error = function(e) { ok <<- FALSE })
  grDevices::dev.off()
  if (!ok) { unlink(tmp); return(NULL) }
  b64 <- tryCatch(base64enc::base64encode(tmp), error = function(e) NULL)
  unlink(tmp)
  b64
}

show_placeholder <- function(msg) {
  par(mar = c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, paste(msg), cex = 1.2, col = "#6c757d")
}

is_safe_cat <- function(vec) {
  if (!(is.factor(vec) || is.character(vec))) return(FALSE)
  lvls <- length(unique(vec[!is.na(vec)]))
  return(lvls > 1 && lvls <= 50)
}

init_data <- function(df) {
  if ("Organic_depth" %in% names(df)) {
    df$Organic_depth <- as.numeric(as.character(df$Organic_depth))
  }
  return(df)
}

plot_relationships <- function(df, num1, num2, cat_var, view_mode = "Grid View", target = NULL) {
  if (is.null(df) || !isTruthy(cat_var) || !isTruthy(num1) || !isTruthy(num2) || !(cat_var %in% names(df))) {
    show_placeholder("Awaiting valid numeric and categorical variables...")
    return()
  }

  plot_df <- df[complete.cases(df[, c(num1, num2, cat_var)]), ]
  if (nrow(plot_df) == 0) {
    show_placeholder("Data Error: No complete cases available.")
    return()
  }

  plot_df[[cat_var]] <- droplevels(as.factor(plot_df[[cat_var]]))
  fac <- plot_df[[cat_var]]
  num_levels <- length(levels(fac))
  pal <- if(num_levels > 8) rainbow(num_levels) else palette()[1:num_levels]

  wrap_text <- function(x) paste(strwrap(x, width = 15), collapse = "\n")
  wrapped_lvls <- sapply(levels(fac), wrap_text)
  counts <- as.numeric(table(fac))
  wrapped_lvls_with_n <- paste0(wrapped_lvls, "\n(n=", counts, ")")

  safe_num1 <- paste0("`", num1, "`")
  safe_num2 <- paste0("`", num2, "`")
  safe_cat <- paste0("`", cat_var, "`")

  if (view_mode == "Grid View") {
    rows <- 1 + ceiling(num_levels / 3)
    old_par <- par(mfrow = c(rows, 3), mar = c(6, 5, 4, 1) + 0.1, mgp = c(3, 1, 0))
    on.exit(par(old_par))

    form1 <- as.formula(paste(safe_num1, "~", safe_cat))
    boxplot(form1, data = plot_df, main = paste(num1, "by", cat_var), ylab = num1, col = "lightblue",
            names = rep("", num_levels), xlab = "", las = 1, cex.lab = 1.3, cex.axis = 1.1, cex.main = 1.4, outline = FALSE)
    stripchart(form1, data = plot_df, vertical = TRUE, method = "jitter", add = TRUE, pch = 16, col = rgb(0, 0, 0, 0.25), cex = 0.8)
    text(x = 1:num_levels, y = par("usr")[3] - (par("usr")[4] - par("usr")[3]) * 0.03, labels = wrapped_lvls_with_n, xpd = NA, srt = 0, adj = c(0.5, 1), cex = 1.1)

    form2 <- as.formula(paste(safe_num2, "~", safe_cat))
    boxplot(form2, data = plot_df, main = paste(num2, "by", cat_var), ylab = num2, col = "lightgreen",
            names = rep("", num_levels), xlab = "", las = 1, cex.lab = 1.3, cex.axis = 1.1, cex.main = 1.4, outline = FALSE)
    stripchart(form2, data = plot_df, vertical = TRUE, method = "jitter", add = TRUE, pch = 16, col = rgb(0, 0, 0, 0.25), cex = 0.8)
    text(x = 1:num_levels, y = par("usr")[3] - (par("usr")[4] - par("usr")[3]) * 0.03, labels = wrapped_lvls_with_n, xpd = NA, srt = 0, adj = c(0.5, 1), cex = 1.1)

    plot(plot_df[[num1]], plot_df[[num2]], col = pal[as.numeric(fac)], pch = 16,
         main = paste(num1, "vs", num2, "\n(All Data)"), xlab = num1, ylab = num2, cex.lab = 1.3, cex.axis = 1.2, cex.main = 1.4)
    legend("bottomright", legend = levels(fac), col = pal, pch = 16, cex = 1.1, bty = "n")

    for (i in seq_along(levels(fac))) {
      lvl <- levels(fac)[i]
      sub_df <- plot_df[plot_df[[cat_var]] == lvl, ]
      if(nrow(sub_df) > 0) {
        plot(sub_df[[num1]], sub_df[[num2]], col = pal[i], pch = 16,
             main = paste0(wrap_text(lvl), "\n(n=", nrow(sub_df), ")"), xlab = num1, ylab = num2, cex.lab = 1.3, cex.axis = 1.2, cex.main = 1.4)
      } else { show_placeholder("No data") }
    }
  } else {
    old_par <- par(mar = c(11, 7, 7, 2) + 0.1, mgp = c(5, 1.5, 0))
    on.exit(par(old_par))
    if (is.null(target)) { show_placeholder("Select a plot to zoom."); return() }

    if (target == paste("Boxplot:", num1)) {
      form1 <- as.formula(paste(safe_num1, "~", safe_cat))
      boxplot(form1, data = plot_df, main = paste(num1, "by", cat_var), ylab = num1, col = "lightblue",
              names = rep("", num_levels), xlab = "", las = 1, cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.8, outline = FALSE)
      stripchart(form1, data = plot_df, vertical = TRUE, method = "jitter", add = TRUE, pch = 16, col = rgb(0, 0, 0, 0.3), cex = 1.2)
      text(x = 1:num_levels, y = par("usr")[3] - (par("usr")[4] - par("usr")[3]) * 0.03, labels = wrapped_lvls_with_n, xpd = NA, srt = 0, adj = c(0.5, 1), cex = 1.2)

    } else if (target == paste("Boxplot:", num2)) {
      form2 <- as.formula(paste(safe_num2, "~", safe_cat))
      boxplot(form2, data = plot_df, main = paste(num2, "by", cat_var), ylab = num2, col = "lightgreen",
              names = rep("", num_levels), xlab = "", las = 1, cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.8, outline = FALSE)
      stripchart(form2, data = plot_df, vertical = TRUE, method = "jitter", add = TRUE, pch = 16, col = rgb(0, 0, 0, 0.3), cex = 1.2)
      text(x = 1:num_levels, y = par("usr")[3] - (par("usr")[4] - par("usr")[3]) * 0.03, labels = wrapped_lvls_with_n, xpd = NA, srt = 0, adj = c(0.5, 1), cex = 1.2)

    } else if (target == "Scatter: All Data") {
      plot(plot_df[[num1]], plot_df[[num2]], col = pal[as.numeric(fac)], pch = 16, cex = 1.5,
           main = paste(num1, "vs", num2, "\n(All Data)"), xlab = num1, ylab = num2, cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.8)
      legend("bottomright", legend = levels(fac), col = pal, pch = 16, cex = 1.2, bty = "n")
    } else {
      lvl <- target; idx <- match(lvl, levels(fac))
      sub_df <- plot_df[plot_df[[cat_var]] == lvl, ]
      if(nrow(sub_df) > 0) {
        plot(sub_df[[num1]], sub_df[[num2]], col = pal[idx], pch = 16, cex = 1.5,
             main = paste0(wrap_text(lvl), "\n(n=", nrow(sub_df), ")"), xlab = num1, ylab = num2, cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.8)
      } else { show_placeholder("No data") }
    }
  }
}

plot_log_diagnostics <- function(model_obj, target_var) {
  if (is.character(model_obj)) {
    show_placeholder(model_obj)
    return()
  }
  preds <- predict(model_obj$model)
  actual <- model_obj$data[[target_var]]
  tbl <- table(Actual = actual, Predicted = preds)

  old_par <- par(mar = c(4, 4, 2, 2))
  on.exit(par(old_par))
  mosaicplot(tbl, main = "Classification: Actual vs Predicted",
             color = c("lightgray", "lightblue", "lightgreen", "lightcoral"),
             las = 1, cex.axis = 1.1)
}

plot_aov_diagnostics <- function(model, view_mode, target) {
  if (is.character(model)) {
    show_placeholder(model)
    return()
  }

  if (view_mode == "Grid View") {
    old_par <- par(mfrow = c(1, 2), mar = c(6, 6, 5, 2) + 0.1, mgp = c(4, 1.2, 0))
    on.exit(par(old_par))
    plot(model, which = 1, cex.lab = 1.3, cex.main = 1.5)
    plot(model, which = 2, cex.lab = 1.3, cex.main = 1.5)
  } else {
    old_par <- par(mar = c(6, 6, 5, 2) + 0.1, mgp = c(4.5, 1.2, 0))
    on.exit(par(old_par))
    if (is.null(target)) return()
    if (target == "Residuals vs Fitted") plot(model, which = 1, cex.lab = 1.4, cex.axis = 1.2, cex.main = 1.6, cex = 1.5)
    else plot(model, which = 2, cex.lab = 1.4, cex.axis = 1.2, cex.main = 1.6, cex = 1.5)
  }
}

.quality_check <- function(df) {
  msgs <- character(0)
  pct_missing <- 100 * mean(is.na(df))
  if (pct_missing > 20)
    msgs <- c(msgs, sprintf("<b>%.1f%%</b> of values are missing — consider imputation or column removal.", pct_missing))
  else if (pct_missing > 5)
    msgs <- c(msgs, sprintf("<b>%.1f%%</b> of values are missing.", pct_missing))
  n_dup <- sum(duplicated(df))
  if (n_dup > 0)
    msgs <- c(msgs, sprintf("<b>%d duplicate row(s)</b> detected.", n_dup))
  near_const <- names(df)[sapply(df, function(x) {
    tbl <- table(x, useNA = "no")
    length(tbl) > 0 && max(tbl) / sum(tbl) > 0.95
  })]
  if (length(near_const) > 0)
    msgs <- c(msgs, sprintf("Near-constant column(s): <b>%s</b>", paste(near_const, collapse = ", ")))
  num_nms <- names(df)[sapply(df, is.numeric)]
  skewed <- sum(sapply(num_nms, function(v) {
    x <- na.omit(df[[v]])
    if (length(x) < 5 || sd(x) == 0) return(FALSE)
    abs(mean((x - mean(x))^3) / sd(x)^3) > 2
  }))
  if (skewed > 0)
    msgs <- c(msgs, sprintf("<b>%d numeric column(s)</b> are highly skewed — log transform may help.", skewed))
  msgs
}

plot_lm_diagnostics <- function(model, dataset, y_var, view_mode, target) {
  if (is.character(model)) {
    show_placeholder(model)
    return()
  }

  if (view_mode == "Grid View") {
    old_par <- par(mfrow = c(1, 3), mar = c(6, 6, 5, 2) + 0.1, mgp = c(4, 1.2, 0))
    on.exit(par(old_par))
    plot(model$fitted.values, model$model[[1]], main = "Actual vs. Fitted", xlab = "Fitted Values", ylab = "Actual Data", pch = 16, col = rgb(0.2, 0.5, 0.8, 0.5), cex.lab=1.3, cex.main=1.5)
    abline(0, 1, col = "red", lwd = 2, lty = 2)
    plot(model$fitted.values, resid(model), main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals", pch = 16, col = rgb(0.3, 0.3, 0.3, 0.5), cex.lab=1.3, cex.main=1.5)
    abline(h = 0, col = "red", lwd = 2)
    hist(dataset[[y_var]], main = paste("Distribution of", y_var), xlab = y_var, col = "lightblue", border = "white", cex.lab=1.3, cex.main=1.5)
  } else {
    old_par <- par(mar = c(6, 6, 5, 2) + 0.1, mgp = c(4.5, 1.2, 0))
    on.exit(par(old_par))
    if (is.null(target)) return()
    if (target == "Fitted vs Actual") {
      plot(model$fitted.values, model$model[[1]], main = "Actual vs. Fitted Values", xlab = "Model Predicted (Fitted)", ylab = "Actual Data", pch = 16, cex = 1.5, col = rgb(0.2, 0.5, 0.8, 0.5), cex.lab=1.4, cex.axis=1.2, cex.main=1.6)
      abline(0, 1, col = "red", lwd = 2, lty = 2)
    } else if (target == "Residual Plot") {
      plot(model$fitted.values, resid(model), main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals", pch = 16, cex = 1.5, col = rgb(0.3, 0.3, 0.3, 0.5), cex.lab=1.4, cex.axis=1.2, cex.main=1.6)
      abline(h = 0, col = "red", lwd = 2)
    } else {
      hist(dataset[[y_var]], main = paste("Distribution of", y_var), xlab = y_var, col = "lightblue", border = "white", cex.lab=1.4, cex.axis=1.2, cex.main=1.6)
    }
  }
}
