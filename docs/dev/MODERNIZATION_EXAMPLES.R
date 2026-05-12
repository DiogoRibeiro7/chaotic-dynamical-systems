# Modernization Examples for chaoticds Package
# Demonstrates before/after code improvements

# =============================================================================
# Example 1: S3 Class Implementation
# =============================================================================

# BEFORE: Function returns unnamed list
extremal_index_runs_old <- function(x, threshold, run_length = 1) {
  # ... computation ...
  theta <- compute_theta(...)

  list(
    theta = theta,
    threshold = threshold,
    n_exc = n_exceedances
  )
}

# Usage
result <- extremal_index_runs_old(x, threshold)
print(result$theta)  # User must know structure

# AFTER: Function returns S3 object with methods
#' @export
extremal_index_runs_new <- function(x, threshold, run_length = 1) {
  # ... computation ...
  theta <- compute_theta(...)

  structure(
    list(
      estimate = theta,
      method = "runs",
      threshold = threshold,
      n_exceedances = n_exceedances,
      n_clusters = n_clusters,
      run_length = run_length,
      data_summary = list(
        n = length(x),
        mean = mean(x),
        sd = sd(x)
      )
    ),
    class = "extremal_index"
  )
}

#' Print method for extremal_index objects
#' @export
print.extremal_index <- function(x, digits = 4, ...) {
  cat("\nExtremal Index Estimation\n")
  cat(strrep("=", 40), "\n")
  cat("Method:      ", x$method, "\n")
  cat("Estimate:    ", round(x$estimate, digits), "\n")
  cat("Threshold:   ", round(x$threshold, digits), "\n")
  cat("Exceedances: ", x$n_exceedances, "\n")
  cat("Clusters:    ", x$n_clusters, "\n\n")
  invisible(x)
}

#' Summary method for extremal_index objects
#' @export
summary.extremal_index <- function(object, ...) {
  structure(
    list(
      call = object$call,
      estimate = object$estimate,
      method = object$method,
      threshold = object$threshold,
      exceedance_stats = list(
        n_exceedances = object$n_exceedances,
        n_clusters = object$n_clusters,
        avg_cluster_size = object$n_exceedances / object$n_clusters
      ),
      data_summary = object$data_summary
    ),
    class = "summary.extremal_index"
  )
}

#' @export
print.summary.extremal_index <- function(x, ...) {
  cat("\nExtremal Index Summary\n")
  cat(strrep("=", 40), "\n\n")

  cat("Estimation Details:\n")
  cat("  Method:    ", x$method, "\n")
  cat("  Estimate:  ", round(x$estimate, 4), "\n")
  cat("  Threshold: ", round(x$threshold, 4), "\n\n")

  cat("Exceedance Statistics:\n")
  cat("  Total exceedances:    ", x$exceedance_stats$n_exceedances, "\n")
  cat("  Number of clusters:   ", x$exceedance_stats$n_clusters, "\n")
  cat("  Avg. cluster size:    ", round(x$exceedance_stats$avg_cluster_size, 2), "\n\n")

  cat("Data Summary:\n")
  cat("  Sample size: ", x$data_summary$n, "\n")
  cat("  Mean:        ", round(x$data_summary$mean, 4), "\n")
  cat("  Std. Dev.:   ", round(x$data_summary$sd, 4), "\n\n")

  invisible(x)
}

# Usage - much better!
result <- extremal_index_runs_new(x, threshold)
print(result)    # Automatic nice printing
summary(result)  # Comprehensive summary


# =============================================================================
# Example 2: Pipe-Friendly Design
# =============================================================================

# BEFORE: Not pipe-friendly
analyze_extremes <- function(x, threshold, block_size) {
  bm <- block_maxima(x, block_size)
  exc <- exceedances(x, threshold)
  theta <- extremal_index_runs(x, threshold)

  list(block_maxima = bm, exceedances = exc, theta = theta)
}

# AFTER: Pipe-friendly with data frame backbone
#' Comprehensive extreme value analysis
#'
#' @param .data Data frame or NULL
#' @param x Numeric vector (if .data is NULL)
#' @importFrom dplyr %>% mutate
#' @export
analyze_extremes <- function(.data = NULL, x = NULL,
                              threshold = NULL, block_size = 50) {
  # Handle piping
  if (is.null(.data)) {
    .data <- data.frame(x = x)
  }

  # Extract data
  if ("x" %in% names(.data)) {
    x_vec <- .data$x
  } else {
    stop("Data must contain column 'x'")
  }

  # Auto-select threshold if not provided
  if (is.null(threshold)) {
    threshold <- quantile(x_vec, 0.95)
  }

  # Perform analyses
  bm <- block_maxima(x_vec, block_size)
  exc <- exceedances(x_vec, threshold)
  theta_result <- extremal_index_runs(x_vec, threshold)

  # Return augmented data frame
  .data$threshold_used <- threshold
  .data$n_block_maxima <- length(bm)
  .data$n_exceedances <- length(exc)
  .data$extremal_index <- theta_result$estimate
  .data$n_clusters <- theta_result$n_clusters

  # Add results as attributes
  attr(.data, "block_maxima") <- bm
  attr(.data, "exceedances") <- exc
  attr(.data, "theta_object") <- theta_result

  .data
}

# Usage with pipes
library(dplyr)

data.frame(x = simulate_logistic_map(1000, 3.8, 0.2)) %>%
  analyze_extremes(threshold = quantile(.$x, 0.95), block_size = 50) %>%
  mutate(series_type = "logistic_chaotic")


# =============================================================================
# Example 3: Progress Bars for Long Operations
# =============================================================================

# BEFORE: Silent operation, user doesn't know progress
logistic_bifurcation_old <- function(r_seq, n_iter = 200, discard = 100, x0 = 0.2) {
  results <- vector("list", length(r_seq))

  for (i in seq_along(r_seq)) {
    # Long computation...
    results[[i]] <- compute_bifurcation_point(r_seq[i], n_iter, discard, x0)
  }

  do.call(rbind, results)
}

# AFTER: Optional progress bar
#' @param .progress Show progress bar? (logical)
#' @export
logistic_bifurcation_new <- function(r_seq, n_iter = 200, discard = 100,
                                      x0 = 0.2, .progress = interactive()) {
  # Validate inputs
  checkmate::assert_numeric(r_seq, min.len = 1)
  checkmate::assert_int(n_iter, lower = 1)
  checkmate::assert_int(discard, lower = 0)
  checkmate::assert_number(x0, lower = 0, upper = 1)

  # Initialize progress bar
  if (.progress && requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      format = "Computing bifurcation [:bar] :percent eta: :eta",
      total = length(r_seq),
      clear = FALSE
    )
  }

  results <- vector("list", length(r_seq))

  for (i in seq_along(r_seq)) {
    results[[i]] <- compute_bifurcation_point(r_seq[i], n_iter, discard, x0)

    if (.progress && exists("pb")) {
      pb$tick()
    }
  }

  do.call(rbind, results)
}


# =============================================================================
# Example 4: Factory Pattern for Map Creation
# =============================================================================

# BEFORE: Separate functions for each map
# simulate_logistic_map()
# simulate_henon_map()
# simulate_tent_map()
# ...

# AFTER: Unified interface with factory
#' Create a dynamical system map
#'
#' @param type Map type: "logistic", "henon", "tent", "lozi", "cat"
#' @param ... Parameters specific to the map
#' @return Function that implements one iteration of the map
#' @export
#' @examples
#' # Create logistic map with r = 3.8
#' logistic <- create_map("logistic", r = 3.8)
#' x <- 0.2
#' for (i in 1:10) {
#'   x <- logistic(x)
#' }
create_map <- function(type = c("logistic", "henon", "tent", "lozi", "cat"), ...) {
  type <- match.arg(type)
  params <- list(...)

  switch(
    type,
    logistic = {
      r <- params$r %||% 3.8
      function(x) r * x * (1 - x)
    },
    henon = {
      a <- params$a %||% 1.4
      b <- params$b %||% 0.3
      function(state) {
        list(
          x = 1 - a * state$x^2 + state$y,
          y = b * state$x
        )
      }
    },
    tent = {
      r <- params$r %||% 2
      function(x) ifelse(x < 0.5, r * x, r * (1 - x))
    }
  )
}

# Helper for NULL default
`%||%` <- function(x, y) if (is.null(x)) y else x

# Usage
logistic_map <- create_map("logistic", r = 3.9)
x <- 0.2
trajectory <- numeric(100)
trajectory[1] <- x
for (i in 2:100) {
  trajectory[i] <- logistic_map(trajectory[i - 1])
}


# =============================================================================
# Example 5: Functional Programming with Map Functions
# =============================================================================

# BEFORE: Manual loops everywhere
compute_statistics <- function(x_list) {
  means <- numeric(length(x_list))
  variances <- numeric(length(x_list))

  for (i in seq_along(x_list)) {
    means[i] <- mean(x_list[[i]])
    variances[i] <- var(x_list[[i]])
  }

  data.frame(mean = means, variance = variances)
}

# AFTER: Functional approach with purrr
#' @importFrom purrr map_dbl
compute_statistics <- function(x_list) {
  data.frame(
    mean = purrr::map_dbl(x_list, mean),
    variance = purrr::map_dbl(x_list, var),
    sd = purrr::map_dbl(x_list, sd),
    min = purrr::map_dbl(x_list, min),
    max = purrr::map_dbl(x_list, max)
  )
}

# Or using base R's vapply (no dependencies)
compute_statistics_base <- function(x_list) {
  data.frame(
    mean = vapply(x_list, mean, numeric(1)),
    variance = vapply(x_list, var, numeric(1)),
    sd = vapply(x_list, sd, numeric(1))
  )
}


# =============================================================================
# Example 6: Input Validation with Informative Errors
# =============================================================================

# BEFORE: Basic checks
simulate_logistic_map_old <- function(n, r, x0) {
  if (x0 <= 0 || x0 >= 1) stop("x0 must be in (0,1)")
  # ...
}

# AFTER: Comprehensive validation with clear messages
#' @export
simulate_logistic_map_new <- function(n, r, x0) {
  # Use checkmate for comprehensive validation
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_number(r, finite = TRUE)
  checkmate::assert_number(x0, lower = 0, upper = 1)

  # Custom validation with helpful error
  if (x0 <= 0 || x0 >= 1) {
    stop(
      "Initial value 'x0' must be strictly in the interval (0, 1).\n",
      "  Current value: ", x0, "\n",
      "  Suggestion: Try x0 = 0.2 for a typical starting point.",
      call. = FALSE
    )
  }

  # Warn about known problematic regimes
  if (r > 4) {
    warning(
      "Parameter 'r' > 4 may cause divergence.\n",
      "  The logistic map is typically studied for r in [0, 4].",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  # ... rest of function ...
}


# =============================================================================
# Example 7: Tidy Return Values
# =============================================================================

# BEFORE: Inconsistent return types
threshold_diagnostics_old <- function(x, thresholds, lags) {
  mrl <- compute_mrl(x, thresholds)
  acf_vals <- compute_acf(x, lags)

  list(mrl = mrl, acf = acf_vals)  # Mixed list structure
}

# AFTER: Consistent data frame returns
#' @return Tidy data frame with one row per threshold-lag combination
#' @export
threshold_diagnostics_new <- function(x, thresholds, lags) {
  # Compute MRL for each threshold
  mrl_df <- data.frame(
    threshold = thresholds,
    mean_excess = vapply(thresholds, function(u) {
      exc <- x[x > u] - u
      mean(exc)
    }, numeric(1)),
    n_exceedances = vapply(thresholds, function(u) sum(x > u), integer(1)),
    diagnostic = "MRL"
  )

  # Compute ACF for each lag
  acf_vals <- stats::acf(x, lag.max = max(lags), plot = FALSE)$acf
  acf_df <- data.frame(
    lag = lags,
    acf = acf_vals[lags + 1],
    diagnostic = "ACF"
  )

  # Return combined tidy data frame
  bind_rows(
    mrl_df %>% rename(value = mean_excess, param = threshold),
    acf_df %>% rename(value = acf, param = lag)
  )
}

# Usage with ggplot2
diagnostics <- threshold_diagnostics_new(x, quantile(x, c(0.9, 0.95, 0.99)), 1:10)

ggplot(diagnostics, aes(x = param, y = value)) +
  geom_line() +
  facet_wrap(~diagnostic, scales = "free") +
  labs(title = "Threshold Diagnostics")


# =============================================================================
# Example 8: Defensive Programming with Recovery
# =============================================================================

# BEFORE: Function fails completely on any error
fit_gev_old <- function(block_maxima) {
  evd::fgev(block_maxima)  # Fails if evd not installed or fitting fails
}

# AFTER: Graceful degradation with informative errors
#' @export
fit_gev_new <- function(block_maxima, method = c("evd", "ismev", "auto")) {
  method <- match.arg(method)

  # Validate input
  checkmate::assert_numeric(block_maxima, min.len = 10, any.missing = FALSE)

  # Try primary method
  if (method == "auto" || method == "evd") {
    if (requireNamespace("evd", quietly = TRUE)) {
      result <- tryCatch(
        {
          evd::fgev(block_maxima)
        },
        error = function(e) {
          warning(
            "GEV fitting with 'evd' failed: ", e$message,
            "\nTrying alternative method...",
            call. = FALSE
          )
          NULL
        }
      )

      if (!is.null(result)) {
        return(structure(result, method = "evd"))
      }
    }
  }

  # Try fallback method
  if (method == "auto" || method == "ismev") {
    if (requireNamespace("ismev", quietly = TRUE)) {
      result <- tryCatch(
        {
          ismev::gev.fit(block_maxima, show = FALSE)
        },
        error = function(e) {
          stop(
            "GEV fitting failed with both 'evd' and 'ismev'.\n",
            "Error: ", e$message,
            "\nTry: \n",
            "  1. Checking your data for outliers or missing values\n",
            "  2. Increasing the number of block maxima\n",
            "  3. Using a different block size",
            call. = FALSE
          )
        }
      )

      return(structure(result, method = "ismev"))
    }
  }

  # If we get here, no method worked
  stop(
    "No GEV fitting package available.\n",
    "Install one of: evd, ismev\n",
    "Example: install.packages('evd')",
    call. = FALSE
  )
}


# =============================================================================
# Example 9: Documentation Best Practices
# =============================================================================

# BEFORE: Minimal documentation
#' Simulate logistic map
#' @export
simulate_logistic_map <- function(n, r, x0) { }

# AFTER: Comprehensive documentation
#' Simulate the Logistic Map
#'
#' @description
#' Generates a time series following the discrete-time logistic map, a
#' classic example of a simple dynamical system that can exhibit chaotic
#' behavior. The map is defined by the iterative equation:
#' \deqn{x_{n+1} = r \cdot x_n \cdot (1 - x_n)}
#'
#' @details
#' The logistic map is one of the simplest examples of how complex, chaotic
#' behavior can arise from simple deterministic equations. The dynamics depend
#' critically on the parameter \eqn{r}:
#' \itemize{
#'   \item \eqn{r < 1}: Trajectory converges to 0
#'   \item \eqn{1 < r < 3}: Converges to a non-zero fixed point
#'   \item \eqn{3 < r < 1 + \sqrt{6} \approx 3.57}: Periodic oscillations
#'   \item \eqn{r > 3.57}: Chaotic regime with periodic windows
#'   \item \eqn{r = 4}: Fully chaotic
#' }
#'
#' @param n Integer. Number of iterations to generate. Must be positive.
#' @param r Numeric. Growth rate parameter. Typically in [0, 4], but the
#'   function accepts any value. Values outside this range may lead to
#'   divergence.
#' @param x0 Numeric. Initial value, must be strictly in the interval (0, 1).
#'   Common choices are 0.2 or 0.5.
#'
#' @return Numeric vector of length \code{n} containing the orbit starting
#'   from \code{x0}. The first element is always \code{x0}, and subsequent
#'   values follow the logistic map iteration.
#'
#' @section Mathematical Background:
#' The logistic map was popularized by Robert May (1976) as a model for
#' population growth with limited resources. It demonstrates the route to
#' chaos through period-doubling bifurcations.
#'
#' @references
#' May, R. M. (1976). Simple mathematical models with very complicated
#' dynamics. \emph{Nature}, 261(5560), 459-467.
#' \doi{10.1038/261459a0}
#'
#' @seealso
#' \code{\link{logistic_bifurcation}} for exploring parameter space,
#' \code{\link{simulate_henon_map}} for 2D chaotic dynamics,
#' \code{\link{estimate_lyapunov_exponent}} for quantifying chaos.
#'
#' @examples
#' # Basic usage: chaotic regime
#' series <- simulate_logistic_map(100, r = 3.8, x0 = 0.2)
#' plot(series, type = "l", main = "Logistic Map (r = 3.8, chaotic)")
#'
#' # Compare chaotic vs periodic regimes
#' par(mfrow = c(1, 2))
#' plot(simulate_logistic_map(100, r = 3.2, x0 = 0.2), type = "l",
#'      main = "Periodic (r = 3.2)", ylab = "x")
#' plot(simulate_logistic_map(100, r = 3.9, x0 = 0.2), type = "l",
#'      main = "Chaotic (r = 3.9)", ylab = "x")
#' par(mfrow = c(1, 1))
#'
#' \donttest{
#' # Visualize sensitivity to initial conditions
#' n <- 50
#' x1 <- simulate_logistic_map(n, r = 3.9, x0 = 0.2)
#' x2 <- simulate_logistic_map(n, r = 3.9, x0 = 0.200001)
#'
#' plot(abs(x1 - x2), type = "l", log = "y",
#'      main = "Divergence of Nearby Trajectories",
#'      xlab = "Iteration", ylab = "|x1 - x2|")
#' }
#'
#' @family simulation functions
#' @export
simulate_logistic_map <- function(n, r, x0) {
  # Implementation...
}

# NOTE: Family tag groups related functions in documentation!
```
