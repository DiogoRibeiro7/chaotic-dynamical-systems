#' Optimized Wrapper Functions
#'
#' These functions automatically choose between R and C++ implementations
#' based on data size and performance characteristics.
#'
#' @name optimized_wrappers
NULL

#' Smart logistic map simulation
#'
#' Automatically selects C++ implementation for large n.
#'
#' @param n Number of iterations
#' @param r Parameter r
#' @param x0 Initial value
#' @param force_r Logical, force R implementation
#' @return Numeric vector
#' @export
simulate_logistic_map_smart <- function(n, r, x0, force_r = FALSE) {
  checkmate::assert_count(n)
  checkmate::assert_number(r)
  checkmate::assert_number(x0, lower = 0, upper = 1)

  # Use C++ for n > 1000 unless forced to use R
  if (n > 1000 && !force_r) {
    simulate_logistic_map_cpp(n, r, x0)
  } else {
    simulate_logistic_map(n, r, x0)
  }
}

#' Smart block maxima computation
#'
#' Automatically selects C++ implementation for large data.
#'
#' @param x Numeric vector
#' @param block_size Block size
#' @param force_r Logical, force R implementation
#' @return Numeric vector of block maxima
#' @export
block_maxima_smart <- function(x, block_size, force_r = FALSE) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_int(block_size, lower = 1)

  if (length(x) < block_size) {
    stop("length(x) must be >= block_size")
  }

  # Use C++ for large datasets
  if (length(x) > 10000 && !force_r) {
    block_maxima_cpp(x, block_size)
  } else {
    block_maxima(x, block_size)
  }
}

#' Smart threshold exceedances
#'
#' Automatically selects optimal implementation.
#'
#' @param x Numeric vector
#' @param threshold Threshold value
#' @param force_r Logical, force R implementation
#' @return Integer vector of indices
#' @export
threshold_exceedances_smart <- function(x, threshold, force_r = FALSE) {
  checkmate::assert_numeric(x)
  checkmate::assert_number(threshold)

  if (length(x) > 5000 && !force_r) {
    threshold_exceedances_cpp(x, threshold)
  } else {
    threshold_exceedances(x, threshold)
  }
}

#' Smart extremal index estimation
#'
#' Automatically selects optimal implementation.
#'
#' @param x Numeric vector
#' @param threshold Threshold value
#' @param run_length Run length parameter
#' @param force_r Logical, force R implementation
#' @return Numeric extremal index estimate
#' @export
extremal_index_runs_smart <- function(x, threshold, run_length, force_r = FALSE) {
  checkmate::assert_numeric(x)
  checkmate::assert_number(threshold)
  checkmate::assert_int(run_length, lower = 1)

  if (length(x) > 5000 && !force_r) {
    extremal_index_runs_cpp(x, threshold, run_length)
  } else {
    extremal_index_runs(x, threshold, run_length)
  }
}

#' Parallel bootstrap extremal index
#'
#' Parallel implementation of bootstrap extremal index estimation.
#'
#' @param x Numeric vector
#' @param threshold Threshold value
#' @param run_length Run length parameter
#' @param B Number of bootstrap samples
#' @param n_cores Number of cores (default: detectCores() - 1)
#' @param conf_level Confidence level (default: 0.95)
#' @return List with estimate and confidence interval
#' @export
bootstrap_extremal_index_parallel <- function(x, threshold, run_length, B = 1000,
                                                n_cores = NULL, conf_level = 0.95) {
  checkmate::assert_numeric(x)
  checkmate::assert_number(threshold)
  checkmate::assert_int(run_length, lower = 1)
  checkmate::assert_int(B, lower = 100)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)

  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }

  # Point estimate
  theta_hat <- extremal_index_runs_cpp(x, threshold, run_length)

  # Parallel bootstrap
  if (requireNamespace("parallel", quietly = TRUE) && n_cores > 1) {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))

    # Export necessary functions
    parallel::clusterExport(cl, c("extremal_index_runs_cpp"), envir = environment())

    boot_results <- parallel::parLapply(cl, 1:B, function(i) {
      # Resample
      boot_sample <- sample(x, length(x), replace = TRUE)
      # Estimate
      extremal_index_runs_cpp(boot_sample, threshold, run_length)
    })

    boot_theta <- unlist(boot_results)
  } else {
    # Sequential bootstrap
    boot_theta <- replicate(B, {
      boot_sample <- sample(x, length(x), replace = TRUE)
      extremal_index_runs_cpp(boot_sample, threshold, run_length)
    })
  }

  # Remove NA values
  boot_theta <- boot_theta[!is.na(boot_theta)]

  # Compute confidence interval
  alpha <- 1 - conf_level
  ci <- quantile(boot_theta, c(alpha/2, 1 - alpha/2))

  structure(
    list(
      estimate = theta_hat,
      boot_estimates = boot_theta,
      ci = ci,
      conf_level = conf_level,
      B = length(boot_theta),
      method = "parallel_bootstrap"
    ),
    class = "bootstrap_ei"
  )
}

#' Print method for bootstrap_ei
#' @param x Object of class bootstrap_ei
#' @param ... Additional arguments
#' @export
print.bootstrap_ei <- function(x, ...) {
  cat("\nBootstrap Extremal Index Estimation\n")
  cat("====================================\n\n")
  cat(sprintf("Point estimate:  %.4f\n", x$estimate))
  cat(sprintf("%d%% CI:         [%.4f, %.4f]\n",
              x$conf_level * 100, x$ci[1], x$ci[2]))
  cat(sprintf("Bootstrap samples: %d\n", x$B))
  cat(sprintf("Method: %s\n\n", x$method))
  invisible(x)
}

#' Chunked processing for large datasets
#'
#' Process very large datasets in chunks to manage memory.
#'
#' @param x Numeric vector (large dataset)
#' @param threshold Threshold value
#' @param run_length Run length parameter
#' @param chunk_size Size of each chunk
#' @return Numeric extremal index estimate
#' @export
extremal_index_chunked <- function(x, threshold, run_length, chunk_size = 100000) {
  checkmate::assert_numeric(x)
  checkmate::assert_number(threshold)
  checkmate::assert_int(run_length, lower = 1)
  checkmate::assert_int(chunk_size, lower = 1000)

  n <- length(x)

  if (n <= chunk_size) {
    # Small enough to process directly
    return(extremal_index_runs_cpp(x, threshold, run_length))
  }

  # Process in chunks
  n_chunks <- ceiling(n / chunk_size)
  chunk_results <- list()

  message(sprintf("Processing %d chunks of size ~%d...", n_chunks, chunk_size))

  for (i in 1:n_chunks) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, n)
    chunk <- x[start_idx:end_idx]

    # Find exceedances in chunk
    exc_indices <- threshold_exceedances_cpp(chunk, threshold)

    if (length(exc_indices) > 0) {
      # Adjust indices to global position
      exc_indices_global <- exc_indices + start_idx - 1
      chunk_results[[i]] <- exc_indices_global
    }
  }

  # Combine all exceedance indices
  all_exc_indices <- sort(unlist(chunk_results))

  if (length(all_exc_indices) == 0) {
    return(NA_real_)
  }

  # Compute clusters from combined indices
  sizes <- cluster_sizes_cpp(all_exc_indices, run_length)
  n_clusters <- length(sizes)

  return(n_clusters / length(all_exc_indices))
}

#' Adaptive threshold selection
#'
#' Automatically select optimal threshold based on mean residual life plot stability.
#'
#' @param x Numeric vector
#' @param quantile_range Numeric vector of length 2 (min, max quantiles to test)
#' @param n_candidates Number of candidate thresholds
#' @return List with optimal threshold and diagnostics
#' @export
select_threshold_adaptive <- function(x, quantile_range = c(0.9, 0.99),
                                       n_candidates = 20) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_numeric(quantile_range, len = 2)

  # Generate candidate thresholds
  probs <- seq(quantile_range[1], quantile_range[2], length.out = n_candidates)
  thresholds <- quantile(x, probs)

  # Compute mean excess for each threshold
  mean_excess <- mean_excess_cpp(x, thresholds)

  # Find most linear region (indicating GPD validity)
  # Use rolling correlation as measure of linearity
  window_size <- 5
  if (length(mean_excess) < window_size + 1) {
    warning("Too few candidates for adaptive selection")
    return(list(threshold = thresholds[1], diagnostics = NULL))
  }

  linearity_scores <- numeric(length(mean_excess) - window_size)

  for (i in 1:(length(mean_excess) - window_size)) {
    window_indices <- i:(i + window_size)
    window_thresh <- thresholds[window_indices]
    window_me <- mean_excess[window_indices]

    # Check if all values are non-NA
    if (all(!is.na(window_me))) {
      # Compute correlation (linearity measure)
      linearity_scores[i] <- abs(cor(window_thresh, window_me))
    } else {
      linearity_scores[i] <- 0
    }
  }

  # Select threshold with highest linearity score
  best_idx <- which.max(linearity_scores)
  optimal_threshold <- thresholds[best_idx]

  list(
    threshold = optimal_threshold,
    threshold_quantile = probs[best_idx],
    diagnostics = data.frame(
      threshold = thresholds,
      quantile = probs,
      mean_excess = mean_excess
    ),
    linearity_scores = linearity_scores
  )
}

#' Memory-efficient block maxima for very large datasets
#'
#' Computes block maxima without loading entire dataset into memory at once.
#'
#' @param file_path Path to file containing data (one value per line)
#' @param block_size Block size
#' @param skip_lines Number of lines to skip at start
#' @return Numeric vector of block maxima
#' @export
block_maxima_from_file <- function(file_path, block_size, skip_lines = 0) {
  checkmate::assert_file_exists(file_path)
  checkmate::assert_int(block_size, lower = 1)
  checkmate::assert_int(skip_lines, lower = 0)

  con <- file(file_path, "r")
  on.exit(close(con))

  if (skip_lines > 0) {
    readLines(con, n = skip_lines)
  }

  block_maxima <- c()
  current_block <- numeric(0)
  block_count <- 0

  while (TRUE) {
    line <- readLines(con, n = 1)
    if (length(line) == 0) break

    value <- as.numeric(line)
    current_block <- c(current_block, value)

    if (length(current_block) == block_size) {
      block_maxima <- c(block_maxima, max(current_block))
      current_block <- numeric(0)
      block_count <- block_count + 1
    }
  }

  message(sprintf("Processed %d complete blocks", block_count))
  return(block_maxima)
}
