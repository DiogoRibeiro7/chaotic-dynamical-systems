#' Multi-dimensional extreme value analysis utilities
#'
#' Tools for analyzing extremes in higher-dimensional chaotic systems,
#' including multivariate threshold exceedances and dependence analysis.
#'
#' @name multidim_extremes
NULL

#' Multivariate threshold exceedances
#'
#' Identify exceedances above component-wise thresholds in multivariate data.
#'
#' @param X Matrix or data frame with observations in rows, variables in columns
#' @param thresholds Numeric vector of thresholds for each variable
#' @param method Character string: "componentwise" or "radial"
#'
#' @return List containing exceedance information
#' @export
multivariate_exceedances <- function(X, thresholds, method = "componentwise") {
  assertthat::assert_that(is.matrix(X) || is.data.frame(X))
  assertthat::assert_that(ncol(X) == length(thresholds))
  assertthat::assert_that(method %in% c("componentwise", "radial"))
  
  X <- as.matrix(X)
  n <- nrow(X)
  d <- ncol(X)
  
  if (method == "componentwise") {
    # At least one component exceeds its threshold
    exceed_mat <- sweep(X, 2, thresholds, ">")
    exceed_indices <- which(rowSums(exceed_mat) > 0)
  } else if (method == "radial") {
    # Radial exceedances based on L2 norm
    radial_threshold <- sqrt(sum(thresholds^2))
    norms <- sqrt(rowSums(X^2))
    exceed_indices <- which(norms > radial_threshold)
  }
  
  list(
    indices = exceed_indices,
    n_exceedances = length(exceed_indices),
    exceedances = X[exceed_indices, , drop = FALSE],
    method = method
  )
}

#' Extremal dependence measures
#'
#' Compute measures of extremal dependence for bivariate data.
#'
#' @param X Matrix or data frame with 2 columns
#' @param method Character: "chi" for chi statistic, "chibar" for chi-bar
#' @param quantile_level Numeric quantile level for threshold selection
#'
#' @return List with dependence measures
#' @export
extremal_dependence <- function(X, method = "chi", quantile_level = 0.95) {
  assertthat::assert_that(ncol(X) == 2, "Only bivariate data supported")
  assertthat::assert_that(method %in% c("chi", "chibar"))
  assertthat::assert_that(quantile_level > 0.5 && quantile_level < 1)
  
  X <- as.matrix(X)
  n <- nrow(X)
  
  # Transform to uniform margins using empirical CDFs
  U1 <- rank(X[, 1]) / (n + 1)
  U2 <- rank(X[, 2]) / (n + 1)
  
  threshold <- quantile_level
  
  if (method == "chi") {
    # Chi statistic: P(U2 > u | U1 > u) as u -> 1
    exceed1 <- U1 > threshold
    exceed2 <- U2 > threshold
    joint_exceed <- exceed1 & exceed2
    
    chi_est <- sum(joint_exceed) / sum(exceed1)
    chi_est <- 2 * chi_est - 1  # Transform to [-1, 1]
  } else {
    # Chi-bar statistic
    exceed1 <- U1 > threshold
    exceed2 <- U2 > threshold
    
    chi_bar_est <- 2 * log(sum(exceed1) / n) / log(sum(exceed1 & exceed2) / n) - 1
  }
  
  if (method == "chi") {
    list(chi = chi_est, threshold = threshold, method = method)
  } else {
    list(chi_bar = chi_bar_est, threshold = threshold, method = method)
  }
}

#' Adaptive threshold selection for multivariate extremes
#'
#' Select thresholds adaptively based on stability criteria.
#'
#' @param X Matrix or data frame of observations
#' @param target_exceedances Integer target number of exceedances per variable
#' @param stability_check Logical whether to perform stability checks
#'
#' @return List with selected thresholds and diagnostics
#' @export
adaptive_thresholds <- function(X, target_exceedances = 50, stability_check = TRUE) {
  assertthat::assert_that(is.matrix(X) || is.data.frame(X))
  assertthat::assert_that(target_exceedances > 10)
  
  X <- as.matrix(X)
  n <- nrow(X)
  d <- ncol(X)
  
  thresholds <- numeric(d)
  quantile_levels <- numeric(d)
  
  for (j in 1:d) {
    # Start with empirical quantile
    target_prob <- 1 - target_exceedances / n
    threshold_init <- quantile(X[, j], target_prob)
    
    if (stability_check) {
      # Check stability by varying threshold slightly
      thresholds_test <- seq(threshold_init * 0.95, threshold_init * 1.05, length.out = 10)
      stability_scores <- numeric(length(thresholds_test))
      
      for (i in seq_along(thresholds_test)) {
        exc <- X[X[, j] > thresholds_test[i], j]
        if (length(exc) > 10) {
          # Fit GPD and check parameter stability (simplified)
          mean_excess <- mean(exc - thresholds_test[i])
          stability_scores[i] <- 1 / abs(log(mean_excess))  # Simple stability measure
        } else {
          stability_scores[i] <- 0
        }
      }
      
      # Select threshold with highest stability
      best_idx <- which.max(stability_scores)
      thresholds[j] <- thresholds_test[best_idx]
    } else {
      thresholds[j] <- threshold_init
    }
    
    quantile_levels[j] <- target_prob
  }
  
  list(
    thresholds = thresholds,
    quantile_levels = quantile_levels,
    target_exceedances = target_exceedances,
    stability_check = stability_check
  )
}

#' Time-varying threshold selection
#'
#' Select thresholds that adapt to local characteristics of the time series.
#'
#' @param x Numeric vector or matrix of time series data
#' @param window_size Integer size of moving window
#' @param quantile_level Numeric quantile level for threshold
#' @param method Character: "moving" for moving quantile, "local_gp" for local GP fitting
#'
#' @return Vector or matrix of time-varying thresholds
#' @export
time_varying_thresholds <- function(x, window_size = 100, quantile_level = 0.95, 
                                    method = "moving") {
  assertthat::assert_that(method %in% c("moving", "local_gp"))
  assertthat::assert_that(window_size > 10)
  
  if (is.vector(x)) {
    x <- matrix(x, ncol = 1)
  }
  
  n <- nrow(x)
  d <- ncol(x)
  thresholds <- matrix(NA, nrow = n, ncol = d)
  
  for (j in 1:d) {
    for (i in 1:n) {
      # Define window around current observation
      start_idx <- max(1, i - window_size %/% 2)
      end_idx <- min(n, i + window_size %/% 2)
      window_data <- x[start_idx:end_idx, j]
      
      if (method == "moving") {
        thresholds[i, j] <- quantile(window_data, quantile_level)
      } else if (method == "local_gp") {
        # Local GP fitting (simplified implementation)
        initial_threshold <- quantile(window_data, quantile_level)
        exceedances <- window_data[window_data > initial_threshold]
        
        if (length(exceedances) > 5) {
          # Simple moment estimator for GPD scale parameter
          excess <- exceedances - initial_threshold
          mean_excess <- mean(excess)
          var_excess <- var(excess)
          
          # Adjust threshold based on GP parameter estimates
          gamma_est <- 0.5 * (mean_excess^2 / var_excess - 1)
          scale_est <- 0.5 * mean_excess * (mean_excess^2 / var_excess + 1)
          
          # Adjust threshold to maintain consistent exceedance rate
          adjustment <- scale_est * gamma_est * 0.1  # Small adjustment
          thresholds[i, j] <- initial_threshold + adjustment
        } else {
          thresholds[i, j] <- initial_threshold
        }
      }
    }
  }
  
  if (ncol(thresholds) == 1) {
    return(as.vector(thresholds))
  } else {
    return(thresholds)
  }
}