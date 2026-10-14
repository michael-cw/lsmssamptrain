test_that("strAlloc computes proportional, Neyman, and cost-optimal allocations accurately", {
  Nh <- c(2385, 10286, 14016)
  Sh <- c(2282.18, 2415.99, 2302.59)
  ch <- c(536.78, 327.88, 306.89)

  # Proportional allocation
  res_prop <- lsmssamptrain:::strAlloc(n.tot = 600, Nh = Nh, alloc = "prop")
  expect_equal(sum(res_prop$nh), 600)
  expect_equal(res_prop$nh / 600, Nh / sum(Nh), tolerance = 1e-6)

  # Neyman allocation
  res_ney <- lsmssamptrain:::strAlloc(n.tot = 600, Nh = Nh, Sh = Sh, alloc = "neyman")
  expect_equal(sum(res_ney$nh), 600)
  expected_ney <- 600 * (Nh * Sh) / sum(Nh * Sh)
  expect_equal(res_ney$nh, expected_ney, tolerance = 1e-6)

  # Optimal cost allocation
  budget <- 150000
  res_opt <- lsmssamptrain:::strAlloc(Nh = Nh, Sh = Sh, cost = budget, ch = ch, alloc = "totcost")
  expect_equal(sum(res_opt$nh * ch), budget, tolerance = 1e-5)
})

test_that("ppsDT selects the requested number of clusters without mutating input data", {
  dt_clu <- data.table::data.table(
    cluster = factor(sprintf("C%03d", 1:100)),
    HH_count = sample(15:120, 100, replace = TRUE)
  )
  orig_cols <- data.table::copy(names(dt_clu))

  sampled <- lsmssamptrain:::ppsDT(dt_clu, clu = "cluster", n = 25, sizevar = "HH_count")
  expect_equal(names(dt_clu), orig_cols)
  expect_equal(nrow(sampled), 25)
  expect_true("p1" %in% names(sampled))
  expect_true(all(sampled$p1 > 0 & sampled$p1 <= 1))
})

test_that("hav_dist computes accurate Haversine distances in km", {
  # Distance from Addis Ababa to itself is 0
  expect_equal(lsmssamptrain:::hav_dist(38.7578, 8.9806), 0, tolerance = 1e-6)
  # Distance from Tigray (~39.5, 13.5) to Addis Ababa (~38.76, 8.98) is ~509 km
  d <- lsmssamptrain:::hav_dist(39.5, 13.5)
  expect_gt(d, 450)
  expect_lt(d, 550)
})

test_that("plot_normal_distribution_with_ci returns a ggplot object", {
  p <- lsmssamptrain:::plot_normal_distribution_with_ci(mu = 2400, sigma = 120, confidence_interval = 0.95)
  expect_s3_class(p, "ggplot")
  expect_error(lsmssamptrain:::plot_normal_distribution_with_ci(2400, 120, 0.85))
})

test_that("UI has no duplicate Shiny input IDs", {
  ui_html <- as.character(lsmssamptrain:::main_ui())
  ids <- regmatches(ui_html, gregexpr('id="[A-Za-z0-9_.-]+"', ui_html))[[1]]
  ids <- gsub('^id="|"$', "", ids)
  # Check that cludesign and n_clust are unique
  expect_equal(sum(ids == "cludesign"), 1)
  expect_equal(sum(ids == "n_clust"), 1)
  expect_equal(sum(ids == "cludesign_init"), 1)
  expect_equal(sum(ids == "n_clust_init"), 1)
})

test_that("Stratified and Cluster estimators are unbiased on package synthetic population", {
  set.seed(42)
  dataPP <- data.table::as.data.table(lsmssamptrain::population)
  dataHH <- data.table::as.data.table(lsmssamptrain::population.hh)
  data.table::setkeyv(dataPP, "hhidg")
  data.table::setkeyv(dataHH, "hhidg")
  data.table::setorderv(dataHH, "stratum")

  pop <- nrow(dataPP)
  pop_str <- dataPP[, list(N = .N), by = "stratum"][order(stratum)]
  Wh <- pop_str$N / pop
  true_inc <- mean(dataPP$income)
  true_emp <- mean(dataPP$employment.status)

  # 1. Equal stratified allocation (unequal weights across strata)
  n_str <- c(150, 150, 150)
  est_str <- numeric(40)
  for (i in seq_along(est_str)) {
    st <- sampling::strata(dataHH, stratanames = c("stratum"), size = n_str, method = "srswor")
    s_hh <- dataHH[c(st$ID_unit)]
    s_hh$pik <- st$Prob
    data.table::setkeyv(s_hh, "hhidg")
    s_pp <- dataPP[s_hh[, list(hhidg, pik)], nomatch = 0]
    str_stat <- s_pp[, list(
      y_h = as.numeric(sampling::HTestimator(income, pik)) /
        as.numeric(sampling::HTestimator(rep(1, .N), pik))
    ), by = "stratum"][order(stratum)]
    est_str[i] <- sum(Wh * str_stat$y_h)
  }
  expect_lt(abs(mean(est_str) - true_inc) / true_inc, 0.03)

  # 2. Two-Stage PPS Cluster Sampling
  size_hh <- 10
  size_clu <- 40
  data.table::setkeyv(dataHH, "cluster")
  dataCLU <- dataHH[, list(HH_count = length(unique(hhidg))), by = "cluster"]
  dataCLU <- dataCLU[dataCLU$HH_count >= size_hh, ]
  data.table::setkeyv(dataCLU, "cluster")
  dataHH_clu <- dataHH[dataCLU, nomatch = 0]
  dataHH_clu$p2 <- size_hh / dataHH_clu$HH_count

  est_clu_emp <- numeric(40)
  est_clu_inc <- numeric(40)
  for (i in seq_along(est_clu_emp)) {
    s_clu <- lsmssamptrain:::ppsDT(dataCLU, n = size_clu, sizevar = "HH_count")
    data.table::setkeyv(s_clu, "cluster")
    s_st1 <- dataHH_clu[s_clu, nomatch = 0]
    s_hh <- s_st1[, .SD[sample(.N, size_hh)], by = "cluster"]
    s_hh$pik <- s_hh$p1 * s_hh$p2
    s_hh <- s_hh[, list(hhidg, pik)]
    data.table::setkeyv(s_hh, "hhidg")
    s_pp <- dataPP[s_hh, nomatch = 0]
    est_clu_emp[i] <- as.numeric(sampling::HTestimator(s_pp$employment.status, s_pp$pik)) /
      as.numeric(sampling::HTestimator(rep(1, nrow(s_pp)), s_pp$pik))
    est_clu_inc[i] <- as.numeric(sampling::HTestimator(s_pp$income, s_pp$pik)) /
      as.numeric(sampling::HTestimator(rep(1, nrow(s_pp)), s_pp$pik))
  }
  expect_lt(abs(mean(est_clu_emp) - true_emp) / true_emp, 0.02)
  expect_lt(abs(mean(est_clu_inc) - true_inc) / true_inc, 0.05)
})

test_that("build_comp_hist and render_weight_diag_ui produce valid plotly and HTML UI objects", {
  set.seed(101)
  wt_df <- data.frame(mean = rnorm(100, mean = 2397, sd = 35))
  unw_df <- data.frame(mean = rnorm(100, mean = 2430, sd = 35))
  x_lim <- range(c(wt_df$mean, unw_df$mean, 2397))

  p_wt <- lsmssamptrain:::build_comp_hist(wt_df, true_val = 2397, is_prop = FALSE, fill_col = "#009FDA", x_lim = x_lim)
  p_unw <- lsmssamptrain:::build_comp_hist(unw_df, true_val = 2397, is_prop = FALSE, fill_col = "#E76F51", x_lim = x_lim)
  expect_s3_class(p_wt, "plotly")
  expect_s3_class(p_unw, "plotly")

  # Unequal weights (e.g., Equal Allocation across unequal strata)
  pik_unequal <- c(rep(150 / 2385, 150), rep(150 / 10286, 150), rep(150 / 14016, 150))
  diag_ui <- lsmssamptrain:::render_weight_diag_ui(
    wt_vals = wt_df$mean,
    unw_vals = unw_df$mean,
    true_val = 2397,
    pik_vec = pik_unequal,
    is_prop = FALSE,
    design_note = "Test note"
  )
  expect_s3_class(diag_ui, "shiny.tag")
  expect_match(as.character(diag_ui), "Unequal Selection Probabilities")

  # Equal weights (self-weighting design)
  pik_equal <- rep(450 / 26687, 450)
  diag_ui_eq <- lsmssamptrain:::render_weight_diag_ui(
    wt_vals = wt_df$mean,
    unw_vals = wt_df$mean,
    true_val = 2397,
    pik_vec = pik_equal,
    is_prop = FALSE
  )
  expect_s3_class(diag_ui_eq, "shiny.tag")
  expect_match(as.character(diag_ui_eq), "Self-Weighting Design")
})

