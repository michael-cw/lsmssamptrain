# lsmssamptrain 0.0.0.9000

## New Features

* Added an interactive **Sampling Weights & Unweighted vs. Weighted Estimation Demo** on the Stratification and Clustering tabs, featuring synchronized side-by-side Horvitz-Thompson vs. unweighted sampling distribution histograms, a unified SRS distribution monitor, base weight ($1/\pi_k$) reporting in regional tables, and a **Weighting & Design Bias Diagnostics** panel reporting empirical bias, SE, RMSE, weight CV, and Kish's weight design effect ($\text{DEFF}_w = 1 + \text{CV}(w)^2$).
* Completed interactive modules for **Stratified Random Sampling** (Equal, Proportional, Neyman, and Cost-Optimal allocations) and **Two-Stage Cluster Sampling** (SRS and PPS Stage-1 selection).
* Added a **Summary & Diagnostics** tab comparing theoretical and simulated survey designs across sample sizes, achieved precision (95% Relative Margin of Error), regional stratum allocations, spatial household distributions, and travel costs.
* Added automated **Word report export** (`.docx`) using `officer` and `flextable`, styled according to World Bank visual identity guidelines and including static spatial distribution maps.
* Added interactive training session summaries for Sessions 1–6 (`slides1`–`slides6`) covering foundational LSMS household survey sampling concepts.
* Exported `run_app()` as a convenient alias for `runSampleTrainer()`.

## Bug Fixes

* **Stratified Estimation**: Fixed biased unweighted averaging across strata by implementing the population-weighted Horvitz-Thompson ratio estimator ($\sum W_h \hat{\bar{Y}}_h$) and ensuring stratum ordering (`"1"`, `"3"`, `"4"`) is aligned between household and person frames.
* **Neyman & Optimal Allocation**: Implemented native `strAlloc()` helper and added stratum boundary clamping to prevent allocation errors or sample sizes exceeding stratum household counts.
* **Systematic PPS Cluster Sampling**: Fixed `ppsDT()` so that cluster order is properly randomized, input `data.table` objects are not mutated in place, and the first sampling interval group is preserved.
* **Two-Stage Cluster Estimation**: Implemented Horvitz-Thompson ratio estimation and ultimate-cluster variance estimation for both binary (employment status) and continuous (household income) indicators across SRS-SRS and PPS-SRS designs.
* **Margin of Error (MOE) Consistency**: Fixed inverted proportion (`1 - P` to `P`) in SRS precision calculations and standardized 95% Relative Margin of Error calculations across SRS, Stratified, and Cluster tabs.
* **Frame Summary Table**: Fixed stratum income standard deviation calculation in `frameTableSRV()` so it reports the unit standard deviation rather than dividing by stratum household count.
* **UI & Reactivity**: Resolved duplicate Shiny input IDs (`cludesign` and `n_clust`), fixed continuous-mode histogram reference lines and `ggplotly()` calls, corrected cross-wired simulation reset buttons, and updated dataset download handlers (`stata_str`, `stata_clu`) to export CSV files via `data.table::fwrite()`.

## Package & CRAN Infrastructure

* Removed dependency on `PracTools` and suppressed retired `sp`/`rgdal`/`rgeos` startup messages.
* Updated `DESCRIPTION` and `R/runapp.R` documentation to meet CRAN standards.
* Added automated `testthat` unit test suite covering allocation formulas, PPS cluster selection, Haversine distance calculations, UI input uniqueness, and Monte Carlo estimator unbiasedness.
