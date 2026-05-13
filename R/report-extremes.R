#' Generate an Extreme-Value Analysis HTML Report
#'
#' One-command reporting utility that runs a complete univariate extremes
#' workflow and renders an HTML report with key diagnostics and model outputs.
#'
#' @param x Optional numeric vector. If `NULL`, a logistic-map series is
#'   simulated using `n`, `r`, and `x0`.
#' @param output_file Output HTML file path.
#' @param n Number of observations to simulate when `x` is `NULL`.
#' @param r Logistic-map parameter used when simulating.
#' @param x0 Logistic-map initial condition used when simulating.
#' @param block_size Block size for block maxima.
#' @param threshold_q Quantile level for POT threshold.
#' @param run_length Run parameter for extremal index and clustering.
#'
#' @return Invisibly returns the output report path.
#' @examples
#' \donttest{
#' report_extremes(output_file = "extremes-report.html")
#' }
#' @export
report_extremes <- function(
    x = NULL,
    output_file = "extremes-report.html",
    n = 5000L,
    r = 3.8,
    x0 = 0.2,
    block_size = 100L,
    threshold_q = 0.95,
    run_length = 3L
) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required to generate reports.", call. = FALSE)
  }

  if (is.null(x)) {
    checkmate::assert_count(n)
    checkmate::assert_number(r)
    checkmate::assert_number(x0, lower = 0, upper = 1)
    x <- simulate_logistic_map(n = n, r = r, x0 = x0)
    source_label <- sprintf("Simulated logistic map (n=%d, r=%.3f, x0=%.3f)", n, r, x0)
  } else {
    checkmate::assert_numeric(x, any.missing = FALSE, min.len = 100)
    source_label <- "User-supplied series"
  }

  checkmate::assert_int(block_size, lower = 2)
  checkmate::assert_number(threshold_q, lower = 0.8, upper = 0.999)
  checkmate::assert_int(run_length, lower = 1)

  threshold <- as.numeric(stats::quantile(x, threshold_q, names = FALSE))
  bm <- block_maxima(x, block_size)
  gev_fit <- tryCatch(fit_gev(bm), error = function(e) e)
  gpd_fit <- tryCatch(fit_gpd(x, threshold), error = function(e) e)
  theta_runs <- tryCatch(extremal_index_runs(x, threshold, run_length), error = function(e) NA_real_)
  theta_intervals <- tryCatch(extremal_index_intervals(x, threshold), error = function(e) NA_real_)
  to_scalar <- function(v) {
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v)) return(NA_real_)
    as.numeric(v)
  }
  theta_runs <- to_scalar(theta_runs)
  theta_intervals <- to_scalar(theta_intervals)
  sizes <- cluster_sizes(x, threshold, run_length)
  diag <- threshold_diagnostics(
    x,
    thresholds = as.numeric(stats::quantile(x, seq(0.85, 0.99, by = 0.02), names = FALSE)),
    k_values = 5:30
  )

  warnings <- character(0)
  if (length(exceedances(x, threshold)) < 30) {
    warnings <- c(warnings, "Low number of exceedances (< 30): POT fit may be unstable.")
  }
  if (length(bm) < 20) {
    warnings <- c(warnings, "Low number of block maxima (< 20): GEV fit may be unstable.")
  }
  if (is.na(theta_runs) || theta_runs <= 0 || theta_runs > 1) {
    warnings <- c(warnings, "Runs extremal-index estimate is invalid or unstable.")
  }
  if (is.na(theta_intervals) || theta_intervals <= 0 || theta_intervals > 1) {
    warnings <- c(warnings, "Intervals extremal-index estimate is invalid or unstable.")
  }

  report_data <- list(
    source_label = source_label,
    n = length(x),
    threshold = threshold,
    threshold_q = threshold_q,
    run_length = run_length,
    block_size = block_size,
    series = x,
    block_maxima = bm,
    exceedances = exceedances(x, threshold),
    gev_fit = gev_fit,
    gpd_fit = gpd_fit,
    theta_runs = theta_runs,
    theta_intervals = theta_intervals,
    cluster_sizes = sizes,
    cluster_summary = if (length(sizes) > 0) cluster_summary(sizes) else c(mean_size = NA_real_, var_size = NA_real_),
    diagnostics = diag,
    warnings = warnings
  )

  data_rds <- tempfile("chaoticds-report-data-", fileext = ".rds")
  saveRDS(report_data, data_rds)

  template <- system.file("rmarkdown", "report-extremes.Rmd", package = "chaoticds")
  if (template == "") {
    stop("Report template not found in package installation.", call. = FALSE)
  }

  rmarkdown::render(
    input = template,
    output_file = output_file,
    params = list(data_rds = data_rds),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )

  invisible(normalizePath(output_file, winslash = "/", mustWork = FALSE))
}
