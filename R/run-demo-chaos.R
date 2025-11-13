#' Complete Extreme Value Analysis Workflow for Chaotic Systems
#'
#' @description
#' Runs a comprehensive extreme value analysis on a simulated logistic map,
#' applying both block maxima and peaks-over-threshold methods. This function
#' provides a complete workflow demonstration and returns all results in a
#' structured list for further exploration.
#'
#' @details
#' ## Overview
#' This function serves multiple purposes:
#' 1. **Educational**: Demonstrates a complete EVT workflow
#' 2. **Exploratory**: Quick analysis of chaotic system extremes
#' 3. **Template**: Shows how to combine package functions
#' 4. **Comparison**: Applies multiple methods to the same data
#'
#' The function simulates chaotic dynamics and then performs:
#' - Block maxima extraction and GEV fitting
#' - Peaks-over-threshold analysis and GPD fitting
#' - Extremal index estimation (runs and intervals methods)
#' - Cluster analysis
#' - Autocorrelation and mixing diagnostics
#' - Threshold selection diagnostics
#'
#' ## Workflow Steps
#' 1. **Simulate** logistic map with specified parameters
#' 2. **Extract extremes** using block maxima method
#' 3. **Fit GEV** distribution to block maxima
#' 4. **Identify exceedances** above high threshold
#' 5. **Fit GPD** distribution to exceedances
#' 6. **Estimate θ** using runs and intervals methods
#' 7. **Analyze clusters** of extreme events
#' 8. **Compute diagnostics** (ACF, mixing coefficients)
#' 9. **Generate report** (optional PDF output)
#'
#' ## When to Use
#' - Learning the package workflow
#' - Quick exploratory analysis
#' - Comparing different EVT methods
#' - Generating demonstration results
#'
#' ## When NOT to Use
#' - Production/research analysis (use individual functions instead)
#' - When you need fine control over parameters
#' - For non-logistic-map data (adapt the workflow manually)
#' - When computational efficiency is critical
#'
#' ## Customizing the Workflow
#' For production analysis, use individual functions:
#' ```r
#' # 1. Simulate or load your data
#' series <- simulate_logistic_map(n = 5000, r = 3.8, x0 = 0.2)
#'
#' # 2. Choose threshold carefully
#' threshold <- quantile(series, 0.95)
#'
#' # 3. Apply specific methods
#' bm <- block_maxima(series, block_size = 100)
#' gev_fit <- fit_gev(bm)
#' theta <- extremal_index_runs(series, threshold, run_length = 2)
#'
#' # 4. Validate and diagnose
#' # ... your custom analysis ...
#' ```
#'
#' @section Output Structure:
#' The returned list contains 11 components organized by analysis type:
#'
#' **Simulation**:
#' - `series`: The simulated time series
#'
#' **Threshold Selection**:
#' - `threshold`: The chosen threshold value
#' - `diagnostics`: Threshold selection diagnostics
#'
#' **Block Maxima Method**:
#' - `block_maxima`: Extracted maxima values
#' - `gev_fit`: Fitted GEV model object
#'
#' **Peaks-Over-Threshold Method**:
#' - `exceedances`: Values above threshold
#' - `gpd_fit`: Fitted GPD model object
#'
#' **Extremal Index**:
#' - `extremal_index`: Named vector with `runs` and `intervals` estimates
#'
#' **Cluster Analysis**:
#' - `cluster_sizes`: Vector of cluster sizes
#' - `cluster_summary`: Summary statistics for clusters
#'
#' **Diagnostics**:
#' - `acf`: Autocorrelation function values
#' - `mixing`: Mixing coefficient estimates
#'
#' @param n Integer. Number of iterations to simulate. Must be positive.
#'   Recommended: at least 2000 for reliable statistics. Larger values
#'   (5000-10000) provide better estimates but take longer. Default: 2000.
#'
#' @param r Numeric. The logistic map parameter. Valid range [0, 4].
#'   For chaotic behavior, use r ≥ 3.57. Default: 3.8 (robust chaotic regime).
#'
#' @param x0 Numeric. Initial condition in (0, 1). The specific value is
#'   typically not critical for chaotic parameters after transient behavior
#'   dies out. Default: 0.2.
#'
#' @param block_size Integer. Size of blocks for block maxima method. Should
#'   be chosen to ensure at least 20-50 blocks (so block_size ≤ n/20).
#'   Default: 50 (gives 40 blocks when n=2000).
#'
#' @param threshold_q Numeric. Quantile to use as threshold for POT analysis.
#'   Should be between 0.9 and 0.99. Higher values give fewer but more extreme
#'   exceedances. Default: 0.95 (recommended starting point).
#'
#' @param output_report Logical. If TRUE and rmarkdown package is available,
#'   generates a PDF report named "demo-chaos.pdf" in the current directory
#'   with key results. Default: FALSE.
#'
#' @return
#' A named list with 11 components (see **Output Structure** in Details).
#' Each component can be accessed using `$` notation, e.g.,
#' `results$extremal_index` or `results$gev_fit`.
#'
#' The list structure makes it easy to:
#' - Extract specific results: `results$extremal_index["runs"]`
#' - Examine fits: `summary(results$gev_fit)`
#' - Create custom plots: `hist(results$block_maxima)`
#' - Compare methods: `results$extremal_index["runs"]` vs `results$extremal_index["intervals"]`
#'
#' @references
#' Coles, S. (2001). *An Introduction to Statistical Modeling of Extreme Values*.
#' Springer. \doi{10.1007/978-1-4471-3675-0}
#'
#' This workflow implements methodology from multiple chapters of Coles (2001)
#' and demonstrates integration of different EVT approaches.
#'
#' @seealso
#' **Individual workflow steps**:
#' \code{\link{simulate_logistic_map}}, \code{\link{block_maxima}},
#' \code{\link{fit_gev}}, \code{\link{exceedances}}, \code{\link{fit_gpd}},
#' \code{\link{extremal_index_runs}}, \code{\link{extremal_index_intervals}},
#' \code{\link{cluster_sizes}}, \code{\link{threshold_diagnostics}}
#'
#' **Vignettes**:
#' \code{vignette("getting-started")} for guided tutorial,
#' \code{vignette("estimating-theta-logistic")} for extremal index details,
#' \code{vignette("block-maxima-vs-pot-henon")} for method comparison
#'
#' @family utility functions
#'
#' @examples
#' # Basic usage with defaults
#' demo_results <- run_demo()
#' names(demo_results)
#'
#' # Examine structure
#' str(demo_results, max.level = 1)
#'
#' # Access specific results
#' cat("Extremal index (runs):", demo_results$extremal_index["runs"], "\n")
#' cat("Extremal index (intervals):", demo_results$extremal_index["intervals"], "\n")
#'
#' # Examine GEV fit
#' print(demo_results$gev_fit)
#'
#' # Look at cluster statistics
#' print(demo_results$cluster_summary)
#'
#' # Customize parameters for different analysis
#' demo_r4 <- run_demo(n = 5000, r = 4.0, block_size = 100, threshold_q = 0.99)
#' cat("Higher threshold gives fewer exceedances:",
#'     length(demo_r4$exceedances), "\n")
#'
#' # Compare different thresholds
#' results_90 <- run_demo(n = 3000, threshold_q = 0.90)
#' results_95 <- run_demo(n = 3000, threshold_q = 0.95)
#' results_99 <- run_demo(n = 3000, threshold_q = 0.99)
#'
#' cat("\nThreshold sensitivity:\n")
#' cat("q=0.90: theta =", round(results_90$extremal_index["runs"], 3),
#'     "with", length(results_90$exceedances), "exceedances\n")
#' cat("q=0.95: theta =", round(results_95$extremal_index["runs"], 3),
#'     "with", length(results_95$exceedances), "exceedances\n")
#' cat("q=0.99: theta =", round(results_99$extremal_index["runs"], 3),
#'     "with", length(results_99$exceedances), "exceedances\n")
#'
#' # Visualize results
#' result <- run_demo(n = 2000, r = 3.8)
#'
#' # Plot time series with threshold
#' plot(result$series, type = "l", col = "gray",
#'      main = "Logistic Map with Threshold",
#'      xlab = "Time", ylab = "Value")
#' abline(h = result$threshold, col = "red", lty = 2, lwd = 2)
#' legend("topright", legend = c("Series", "Threshold (95th %ile)"),
#'        col = c("gray", "red"), lty = c(1, 2), bty = "n")
#'
#' # Compare block maxima distribution
#' hist(result$block_maxima, breaks = 15, probability = TRUE,
#'      col = "lightblue", border = "white",
#'      main = "Block Maxima Distribution",
#'      xlab = "Maximum Value")
#' lines(density(result$block_maxima), col = "darkblue", lwd = 2)
#'
#' # ACF plot
#' plot(1:length(result$acf), result$acf, type = "h", lwd = 6,
#'      col = "steelblue", main = "Autocorrelation Function",
#'      xlab = "Lag", ylab = "ACF", ylim = c(-0.2, 1))
#' abline(h = 0, col = "gray", lty = 2)
#'
#' \donttest{
#' # Generate PDF report (requires rmarkdown)
#' if (requireNamespace("rmarkdown", quietly = TRUE)) {
#'   demo_with_report <- run_demo(n = 3000, output_report = TRUE)
#'   # Report saved as "demo-chaos.pdf" in current directory
#' }
#'
#' # Long-running analysis with more data
#' detailed_results <- run_demo(
#'   n = 10000,
#'   r = 3.8,
#'   block_size = 200,
#'   threshold_q = 0.98
#' )
#' # More data gives more stable estimates
#' print(detailed_results$extremal_index)
#' }
#'
#' @importFrom stats quantile
#' @export
run_demo <- function(n = 2000L,
                     r = 3.8,
                     x0 = 0.2,
                     block_size = 50L,
                     threshold_q = 0.95,
                     output_report = FALSE) {
  series <- simulate_logistic_map(n, r, x0)

  thr <- quantile(series, threshold_q)
  diag <- threshold_diagnostics(series, seq(min(series), max(series), length.out = 20), 1:20)

  bm <- block_maxima(series, block_size)
  gev_fit <- tryCatch(fit_gev(bm), error = function(e) e)

  exc <- exceedances(series, thr)
  gpd_fit <- tryCatch(fit_gpd(series, thr), error = function(e) e)

  theta_runs <- extremal_index_runs(series, thr, run_length = 5)
  theta_int  <- extremal_index_intervals(series, thr)

  sizes <- cluster_sizes(series, thr, run_length = 5)
  size_summary <- cluster_summary(sizes)

  acf_vals <- acf_decay(series, 1:10)
  mix_vals <- mixing_coefficients(series, thr, 1:10)

  if (output_report && requireNamespace("rmarkdown", quietly = TRUE)) {
    report_file <- tempfile("demo-chaos", fileext = ".Rmd")
    rmd_lines <- c(
      "---",
      "title: 'Chaos EVT Demo'",
      "output: pdf_document",
      "---",
      "",
      "```{r setup, echo=FALSE}",
      "library(ggplot2)",
      "```",
      "",
      "## Block Maxima GEV Fit",
      "```{r}",
      "print(gev_fit)",
      "```",
      "",
      "## GPD Fit",
      "```{r}",
      "print(gpd_fit)",
      "```",
      "",
      "## Extremal Index",
      "```{r}",
      "theta_runs",
      "theta_int",
      "```"
    )
    writeLines(rmd_lines, report_file)
    rmarkdown::render(report_file, output_file = "demo-chaos.pdf", quiet = TRUE)
    message("Report written to demo-chaos.pdf")
  }

  list(series = series,
       threshold = thr,
       diagnostics = diag,
       block_maxima = bm,
       gev_fit = gev_fit,
       exceedances = exc,
       gpd_fit = gpd_fit,
       extremal_index = c(runs = theta_runs, intervals = theta_int),
       cluster_sizes = sizes,
       cluster_summary = size_summary,
       acf = acf_vals,
       mixing = mix_vals)
}
