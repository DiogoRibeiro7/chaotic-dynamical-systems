#' Parallel processing utilities for chaoticds
#'
#' Functions to enable parallel processing for computationally intensive
#' operations in extreme value analysis and chaotic system simulation.
#'
#' @name parallel_utils
NULL

#' Parallel bootstrap for extremal index estimation
#'
#' Perform bootstrap estimation of extremal index using parallel processing
#' for improved performance with large datasets or many bootstrap samples.
#'
#' @param data Numeric vector of observations
#' @param threshold Numeric threshold value
#' @param method Character: "runs" or "intervals"
#' @param n_bootstrap Integer number of bootstrap samples
#' @param n_cores Integer number of CPU cores to use (NULL for auto-detection)
#' @param block_size Integer block size for block bootstrap
#'
#' @return List with bootstrap results
#' @export
parallel_bootstrap_ei <- function(data, threshold, method = "runs", 
                                  n_bootstrap = 1000, n_cores = NULL,
                                  block_size = NULL) {
  assertthat::assert_that(is.numeric(data), length(data) > 50)
  assertthat::assert_that(method %in% c("runs", "intervals"))
  
  # Auto-detect cores if not specified
  if (is.null(n_cores)) {
    n_cores <- min(parallel::detectCores() - 1, 4)  # Leave one core free, max 4
  }
  
  # Auto-select block size
  if (is.null(block_size)) {
    block_size <- max(10, floor(length(data)^(1/3)))
  }
  
  n <- length(data)
  n_blocks <- floor(n / block_size)
  
  # Original estimate
  if (method == "runs") {
    original_est <- extremal_index_runs(data, threshold, run_length = 3)
  } else {
    original_est <- extremal_index_intervals(data, threshold)
  }
  
  # Function for single bootstrap iteration
  bootstrap_iteration <- function(seed) {
    set.seed(seed)
    
    # Sample blocks with replacement
    block_indices <- sample(n_blocks, replace = TRUE)
    bootstrap_data <- numeric(0)
    
    for (j in block_indices) {
      start_idx <- (j - 1) * block_size + 1
      end_idx <- min(j * block_size, n)
      bootstrap_data <- c(bootstrap_data, data[start_idx:end_idx])
    }
    
    # Compute extremal index on bootstrap sample
    if (method == "runs") {
      extremal_index_runs(bootstrap_data, threshold, run_length = 3)
    } else {
      extremal_index_intervals(bootstrap_data, threshold)
    }
  }
  
  # Parallel execution
  if (requireNamespace("parallel", quietly = TRUE) && n_cores > 1) {
    # Use parallel processing
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))
    
    # Export necessary functions and data to cluster
    parallel::clusterExport(cl, c("extremal_index_runs", "extremal_index_intervals", 
                                 "threshold_exceedances", "cluster_exceedances",
                                 "data", "threshold", "method", "block_size", "n"), 
                           envir = environment())
    
    # Generate seeds for reproducibility
    seeds <- sample.int(.Machine$integer.max, n_bootstrap)
    
    # Run bootstrap in parallel
    bootstrap_estimates <- parallel::parLapply(cl, seeds, bootstrap_iteration)
    bootstrap_estimates <- unlist(bootstrap_estimates)
    
  } else {
    # Fall back to sequential processing
    seeds <- sample.int(.Machine$integer.max, n_bootstrap)
    bootstrap_estimates <- sapply(seeds, bootstrap_iteration)
  }
  
  # Remove non-finite estimates
  bootstrap_estimates <- bootstrap_estimates[is.finite(bootstrap_estimates)]
  
  if (length(bootstrap_estimates) < 10) {
    warning("Too few valid bootstrap estimates")
    return(list(estimate = original_est, ci = c(NA, NA), method = "parallel"))
  }
  
  # Calculate confidence intervals
  ci <- quantile(bootstrap_estimates, c(0.025, 0.975))
  
  list(
    estimate = original_est,
    bootstrap_estimates = bootstrap_estimates,
    ci = ci,
    n_cores_used = if (exists("cl")) n_cores else 1,
    method = paste0("parallel_", method),
    n_valid_bootstrap = length(bootstrap_estimates)
  )
}

#' Parallel simulation of multiple parameter sets
#'
#' Simulate chaotic maps for multiple parameter combinations in parallel.
#'
#' @param map_type Character: "logistic" or "henon"
#' @param param_grid Data frame with parameter combinations
#' @param n Integer number of iterations per simulation
#' @param n_cores Integer number of cores to use
#'
#' @return List of simulation results
#' @export
parallel_simulation <- function(map_type, param_grid, n = 1000, n_cores = NULL) {
  assertthat::assert_that(map_type %in% c("logistic", "henon"))
  assertthat::assert_that(is.data.frame(param_grid))
  
  if (is.null(n_cores)) {
    n_cores <- min(parallel::detectCores() - 1, nrow(param_grid))
  }
  
  # Function for single parameter set
  simulate_single <- function(i) {
    params <- param_grid[i, ]
    
    if (map_type == "logistic") {
      assertthat::assert_that(all(c("r", "x0") %in% names(params)))
      result <- simulate_logistic_map(n, params$r, params$x0)
      list(params = params, data = result, type = "logistic")
    } else {
      assertthat::assert_that(all(c("a", "b") %in% names(params)))
      x0 <- if ("x0" %in% names(params)) params$x0 else 0
      y0 <- if ("y0" %in% names(params)) params$y0 else 0
      result <- simulate_henon_map(n, params$a, params$b, x0, y0)
      list(params = params, data = result, type = "henon")
    }
  }
  
  # Parallel execution
  if (requireNamespace("parallel", quietly = TRUE) && n_cores > 1) {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))
    
    # Export functions
    parallel::clusterExport(cl, c("simulate_logistic_map", "simulate_henon_map",
                                 "map_type", "param_grid", "n"), 
                           envir = environment())
    
    results <- parallel::parLapply(cl, 1:nrow(param_grid), simulate_single)
  } else {
    results <- lapply(1:nrow(param_grid), simulate_single)
  }
  
  # Add metadata
  attr(results, "map_type") <- map_type
  attr(results, "n_simulations") <- nrow(param_grid)
  attr(results, "n_cores_used") <- if (exists("cl")) n_cores else 1
  
  results
}

#' Parallel cross-validation for threshold selection
#'
#' Perform cross-validation for threshold selection using parallel processing.
#'
#' @param data Numeric vector of observations
#' @param thresholds Numeric vector of candidate thresholds
#' @param k_fold Integer number of folds
#' @param criterion Character: "loglik" or "aic"
#' @param n_cores Integer number of cores to use
#'
#' @return List with CV results
#' @export
parallel_cv_threshold <- function(data, thresholds = NULL, k_fold = 5, 
                                  criterion = "loglik", n_cores = NULL) {
  assertthat::assert_that(is.numeric(data), length(data) > 20)
  assertthat::assert_that(criterion %in% c("loglik", "aic"))
  
  if (is.null(thresholds)) {
    quantiles <- seq(0.8, 0.98, by = 0.02)
    thresholds <- quantile(data, quantiles)
  }
  
  if (is.null(n_cores)) {
    n_cores <- min(parallel::detectCores() - 1, length(thresholds))
  }
  
  n <- length(data)
  folds <- split(sample(n), rep(1:k_fold, length.out = n))
  
  # Function to evaluate single threshold
  evaluate_threshold <- function(threshold) {
    cv_scores <- numeric(k_fold)
    
    for (j in 1:k_fold) {
      # Split data
      test_indices <- folds[[j]]
      train_data <- data[-test_indices]
      test_data <- data[test_indices]
      
      # Fit GPD on training data
      train_exceedances <- train_data[train_data > threshold] - threshold
      
      if (length(train_exceedances) < 5) {
        cv_scores[j] <- -Inf
        next
      }
      
      # Method-of-moments estimator
      mean_excess <- mean(train_exceedances)
      var_excess <- var(train_exceedances)
      
      if (var_excess <= 0) {
        cv_scores[j] <- -Inf
        next
      }
      
      # Moment estimators for GPD
      shape_est <- 0.5 * (mean_excess^2 / var_excess - 1)
      scale_est <- 0.5 * mean_excess * (mean_excess^2 / var_excess + 1)
      
      # Evaluate on test data
      test_exceedances <- test_data[test_data > threshold] - threshold
      
      if (length(test_exceedances) < 1) {
        cv_scores[j] <- -Inf
        next
      }
      
      # Calculate log-likelihood
      if (abs(shape_est) < 1e-8) {
        ll <- -length(test_exceedances) * log(scale_est) - sum(test_exceedances) / scale_est
      } else {
        if (any(1 + shape_est * test_exceedances / scale_est <= 0)) {
          ll <- -Inf
        } else {
          t <- 1 + shape_est * test_exceedances / scale_est
          ll <- -length(test_exceedances) * log(scale_est) - sum(log(t) * (1/shape_est + 1))
        }
      }
      
      if (criterion == "aic") {
        cv_scores[j] <- -2 * ll + 2 * 2  # 2 parameters
      } else {
        cv_scores[j] <- ll
      }
    }
    
    mean(cv_scores[is.finite(cv_scores)])
  }
  
  # Parallel execution
  if (requireNamespace("parallel", quietly = TRUE) && n_cores > 1) {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))
    
    parallel::clusterExport(cl, c("data", "folds", "k_fold", "criterion"), 
                           envir = environment())
    
    mean_scores <- parallel::parSapply(cl, thresholds, evaluate_threshold)
  } else {
    mean_scores <- sapply(thresholds, evaluate_threshold)
  }
  
  # Find optimal threshold
  if (criterion == "aic") {
    optimal_idx <- which.min(mean_scores)
  } else {
    optimal_idx <- which.max(mean_scores)
  }
  
  list(
    optimal_threshold = thresholds[optimal_idx],
    mean_scores = mean_scores,
    thresholds = thresholds,
    criterion = criterion,
    n_cores_used = if (exists("cl")) n_cores else 1
  )
}

#' Progress bar for long-running computations
#'
#' Simple progress indicator for batch operations.
#'
#' @param current Integer current iteration
#' @param total Integer total iterations
#' @param width Integer width of progress bar
#'
#' @return Invisible NULL (used for side effect)
#' @export
progress_bar <- function(current, total, width = 50) {
  percent <- current / total
  filled <- floor(percent * width)
  
  cat("\r[", paste(rep("=", filled), collapse = ""), 
      paste(rep(" ", width - filled), collapse = ""), "] ",
      sprintf("%3.0f%%", percent * 100), " (", current, "/", total, ")",
      sep = "")
  
  if (current == total) cat("\n")
  
  invisible(NULL)
}

#' Check if parallel processing is available
#'
#' @return Logical indicating if parallel package is available
#' @export
parallel_available <- function() {
  requireNamespace("parallel", quietly = TRUE) && parallel::detectCores() > 1
}

#' Get optimal number of cores for parallel processing
#'
#' @param max_cores Integer maximum cores to use
#'
#' @return Integer recommended number of cores
#' @export
optimal_cores <- function(max_cores = 4) {
  if (!parallel_available()) return(1)
  
  available_cores <- parallel::detectCores()
  
  # Use at most max_cores, leave one core free for system
  min(max_cores, max(1, available_cores - 1))
}