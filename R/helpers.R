#' PPS Sampling using data.table
#'
#' @keywords internal
#' @noRd
ppsDT <- function(data, clu = "cluster", n, sizevar = "HH_count") {
  data_in <- data.table::copy(data.table::as.data.table(data))
  data.table::setkeyv(data_in, clu)

  ## Preparing the data
  total_size <- sum(data_in[[sizevar]], na.rm = TRUE)
  int <- total_size / n
  data_in[, p1 := pmin(1, n * (data_in[[sizevar]] / total_size))]

  ## Creating Random Order
  data_in <- data_in[sample(.N)]

  ## Cumulative sum & systematic PPS selection
  data_in[, cumul := cumsum(data_in[[sizevar]])]
  rn <- stats::runif(1, 0, int)
  pts <- rn + (0:(n - 1)) * int
  idx <- findInterval(pts, data_in$cumul, left.open = TRUE) + 1L
  idx <- pmin(idx, nrow(data_in))

  data_out <- data_in[unique(idx)]
  data_out[, cumul := NULL]
  data.table::setkeyv(data_out, clu)
  return(data_out)
}

#' Allocation for Stratified Sampling
#'
#' @param n.tot Total sample size
#' @param Nh Vector of stratum population sizes
#' @param Sh Vector of stratum standard deviations
#' @param cost Total budget / cost
#' @param ch Vector of stratum unit costs
#' @param alloc Allocation type: "neyman", "totcost", or "prop"
#' @return A list with component `nh` containing stratum sample sizes
#' @keywords internal
#' @noRd
strAlloc <- function(n.tot = NULL, Nh = NULL, Sh = NULL, cost = NULL, ch = NULL, 
                     alloc = c("neyman", "totcost", "prop")) {
  alloc <- match.arg(alloc)
  if (is.null(Nh)) stop("Nh cannot be NULL.")
  N <- sum(Nh)
  Wh <- Nh / N
  
  if (alloc == "prop") {
    if (is.null(n.tot)) stop("n.tot is required for proportional allocation.")
    nh <- n.tot * Wh
  } else if (alloc == "neyman") {
    if (is.null(n.tot)) stop("n.tot is required for Neyman allocation.")
    if (is.null(Sh)) stop("Sh cannot be NULL for Neyman allocation.")
    denom <- sum(Wh * Sh)
    if (denom == 0) {
      nh <- rep(n.tot / length(Nh), length(Nh))
    } else {
      nh <- n.tot * Wh * Sh / denom
    }
  } else if (alloc == "totcost") {
    if (is.null(cost)) stop("cost must be specified for totcost allocation.")
    if (is.null(Sh)) stop("Sh cannot be NULL for totcost allocation.")
    if (is.null(ch)) stop("ch cannot be NULL for totcost allocation.")
    d1 <- sum(Wh * Sh / sqrt(ch))
    if (d1 == 0) {
      ph.cost <- rep(1 / length(Nh), length(Nh))
    } else {
      ph.cost <- (Wh * Sh / sqrt(ch)) / d1
    }
    denom2 <- sum(Wh * Sh * sqrt(ch))
    if (denom2 == 0) {
      n.cost <- cost
    } else {
      n.cost <- cost * d1 / denom2
    }
    nh <- n.cost * ph.cost
  }
  
  list(allocation = alloc, Nh = Nh, Sh = Sh, nh = nh)
}

#' Build a Plotly Histogram for Weighted vs. Unweighted Comparison
#'
#' @keywords internal
#' @noRd
build_comp_hist <- function(h_df, true_val, is_prop = TRUE, fill_col = "#009FDA",
                            x_lim = NULL, style_theme = NULL) {
  vals <- if (is.data.frame(h_df)) h_df[[1]] else as.numeric(h_df)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) {
    return(NULL)
  }
  df <- data.frame(mean = vals)
  est_m <- mean(vals, na.rm = TRUE)

  if (is_prop) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = mean, y = ggplot2::after_stat(count) / sum(ggplot2::after_stat(count)))) +
      ggplot2::geom_histogram(bins = 35, fill = fill_col, color = "#002244", alpha = 0.85)
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = mean, y = ggplot2::after_stat(density))) +
      ggplot2::geom_histogram(bins = 35, fill = fill_col, color = "#002244", alpha = 0.85)
  }

  p <- p +
    ggplot2::geom_vline(xintercept = true_val, color = "#D90429", linewidth = 1.1) +
    ggplot2::geom_vline(xintercept = est_m, color = "#002244", linetype = "dashed", linewidth = 0.9) +
    ggplot2::xlab("") +
    ggplot2::ylab("")

  if (!is.null(style_theme)) {
    p <- p + style_theme
  } else {
    p <- p + ggplot2::theme_minimal() + ggplot2::theme(legend.position = "none")
  }

  if (!is.null(x_lim) && length(x_lim) == 2 && all(is.finite(x_lim)) && diff(x_lim) > 0) {
    pad <- 0.05 * diff(x_lim)
    p <- p + ggplot2::coord_cartesian(xlim = c(x_lim[1] - pad, x_lim[2] + pad))
  }

  plotly::ggplotly(p)
}

#' Render Weighting & Bias Diagnostic Card UI
#'
#' @keywords internal
#' @noRd
render_weight_diag_ui <- function(wt_vals, unw_vals, true_val, pik_vec = NULL,
                                  is_prop = TRUE, design_note = "") {
  wt_vals <- wt_vals[!is.na(wt_vals)]
  unw_vals <- unw_vals[!is.na(unw_vals)]
  if (length(wt_vals) == 0 || length(unw_vals) == 0) {
    return(NULL)
  }

  wt_mean <- mean(wt_vals)
  unw_mean <- mean(unw_vals)
  wt_bias <- wt_mean - true_val
  unw_bias <- unw_mean - true_val
  wt_se <- if (length(wt_vals) > 1) stats::sd(wt_vals) else 0
  unw_se <- if (length(unw_vals) > 1) stats::sd(unw_vals) else 0
  wt_rmse <- sqrt(wt_bias^2 + wt_se^2)
  unw_rmse <- sqrt(unw_bias^2 + unw_se^2)

  fmt_val <- function(x) {
    if (is_prop) paste0(round(x * 100, 2), "%") else paste0("$", formatC(round(x, 1), format = "f", digits = 1, big.mark = ","))
  }
  fmt_diff <- function(x) {
    sign_ch <- ifelse(x >= 0, "+", "")
    if (is_prop) paste0(sign_ch, round(x * 100, 2), " pp") else paste0(sign_ch, "$", formatC(round(x, 1), format = "f", digits = 1, big.mark = ","))
  }

  w_info <- ""
  is_self_wt <- FALSE
  if (!is.null(pik_vec) && length(pik_vec) > 0 && all(is.finite(pik_vec)) && all(pik_vec > 0)) {
    w_vec <- 1 / pik_vec
    w_min <- round(min(w_vec), 1)
    w_max <- round(max(w_vec), 1)
    w_cv <- if (length(w_vec) > 1 && mean(w_vec) > 0) stats::sd(w_vec) / mean(w_vec) else 0
    deff_w <- round(1 + w_cv^2, 2)
    is_self_wt <- (w_cv < 0.05)
    badge_txt <- if (is_self_wt) "Self-Weighting Design" else "Unequal Selection Probabilities"
    w_info <- paste0(
      badge_txt, " | Base Weights (w = 1/pik): [", w_min, " to ", w_max,
      "] | Weight CV = ", round(w_cv, 2), " | Kish Weight DEFF_w = ", deff_w
    )
  }

  takeaway <- if (nzchar(design_note)) {
    design_note
  } else if (is_self_wt) {
    "Self-Weighting Design: Inclusion probabilities (pik) are equal across units, so the Weighted (HT) and Unweighted means produce identical distributions."
  } else {
    "Unequal Selection Probabilities: Because sampling rates vary across groups with different population averages, the Unweighted mean is biased (shifted from the red line), whereas Horvitz-Thompson weighting restores design unbiasedness!"
  }

  shiny::tags$div(
    style = "background-color: #F4F8FB; border: 1px solid #D9E2EC; border-left: 4px solid #002244; border-radius: 4px; padding: 10px 12px; margin-top: 8px; font-size: 13px;",
    shiny::tags$div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;",
      shiny::tags$strong(style = "color: #002244; font-size: 14px;", "Weighting & Design Bias Diagnostics"),
      shiny::tags$span(
        style = "font-size: 12px; color: #333;",
        shiny::HTML(paste0("<span style='color:#D90429; font-weight:bold;'>&#9473;</span> True Pop: <b>", fmt_val(true_val), "</b> &nbsp;|&nbsp; <span style='color:#002244; font-weight:bold;'>&#9480;</span> Dashed Line: Estimator Mean"))
      )
    ),
    shiny::tags$table(
      style = "width: 100%; border-collapse: collapse; background: #FFFFFF; margin-bottom: 6px; font-size: 12.5px;",
      shiny::tags$thead(
        style = "background-color: #002244; color: #FFFFFF;",
        shiny::tags$tr(
          shiny::tags$th(style = "padding: 4px 8px;", "Estimator"),
          shiny::tags$th(style = "padding: 4px 8px; text-align: right;", "Simulated Mean"),
          shiny::tags$th(style = "padding: 4px 8px; text-align: right;", "Empirical Bias"),
          shiny::tags$th(style = "padding: 4px 8px; text-align: right;", "Empirical SE"),
          shiny::tags$th(style = "padding: 4px 8px; text-align: right;", "RMSE")
        )
      ),
      shiny::tags$tbody(
        shiny::tags$tr(
          style = "border-bottom: 1px solid #E5E9F0;",
          shiny::tags$td(style = "padding: 4px 8px; font-weight: bold; color: #002244;", "Weighted (Horvitz-Thompson)"),
          shiny::tags$td(style = "padding: 4px 8px; text-align: right;", fmt_val(wt_mean)),
          shiny::tags$td(style = "padding: 4px 8px; text-align: right; color: #008000; font-weight: bold;", fmt_diff(wt_bias)),
          shiny::tags$td(style = "padding: 4px 8px; text-align: right;", fmt_val(wt_se)),
          shiny::tags$td(style = "padding: 4px 8px; text-align: right;", fmt_val(wt_rmse))
        ),
        shiny::tags$tr(
          shiny::tags$td(style = "padding: 4px 8px; font-weight: bold; color: #B73338;", "Unweighted (Naive Mean)"),
          shiny::tags$td(style = "padding: 4px 8px; text-align: right;", fmt_val(unw_mean)),
          shiny::tags$td(style = "padding: 4px 8px; text-align: right; color: #B73338; font-weight: bold;", fmt_diff(unw_bias)),
          shiny::tags$td(style = "padding: 4px 8px; text-align: right;", fmt_val(unw_se)),
          shiny::tags$td(style = "padding: 4px 8px; text-align: right;", fmt_val(unw_rmse))
        )
      )
    ),
    if (nzchar(w_info)) {
      shiny::tags$div(style = "font-size: 12px; color: #002244; font-weight: 600; margin-bottom: 2px;", w_info)
    },
    shiny::tags$div(style = "font-size: 12px; color: #444;", shiny::tags$b("Takeaway: "), takeaway)
  )
}


