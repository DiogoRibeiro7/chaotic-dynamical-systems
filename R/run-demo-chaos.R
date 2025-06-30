#' Run a full demo of extreme-value analysis for chaotic maps
#'
#' This function orchestrates several utilities from this package to
#' demonstrate basic extreme-value statistics on a simulated logistic map.
#' The function optionally compiles a short PDF report when the
#' `rmarkdown` package is available.
#'
#' @param n Integer. Number of iterations for the logistic map.
#' @param r Numeric. Growth rate parameter for the logistic map.
#' @param x0 Numeric. Initial condition in (0, 1).
#' @param block_size Integer. Block length for block maxima.
#' @param threshold_q Numeric. Quantile for high-threshold choice.
#' @param output_report Logical. Render PDF report if TRUE.
#'
#' @return A list containing simulation output, fitted models and diagnostics.
#'   The function is primarily for demonstration and should not be relied on for
#'   rigorous analysis.
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
