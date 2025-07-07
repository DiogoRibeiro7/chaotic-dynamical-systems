#' Advanced diagnostic tools for extreme value models
#'
#' Enhanced diagnostic functions for validating extreme value models
#' and assessing model assumptions.
#'
#' @name advanced_diagnostics
NULL

#' Enhanced model diagnostics for GEV fits
#'
#' Comprehensive diagnostic plots and statistics for GEV model validation.
#'
#' @param data Numeric vector of block maxima
#' @param fit_result Result from fit_gev() or similar
#' @param plot_type Character vector of plots to generate
#' @param return_plots Logical whether to return plot objects
#'
#' @return List of diagnostic statistics and optionally plots
#' @export
enhanced_gev_diagnostics <- function(data, fit_result, 
                                     plot_type = c("qq", "pp", "return_level", "residuals"),
                                     return_plots = TRUE) {
  assertthat::assert_that(is.numeric(data))
  assertthat::assert_that(is.list(fit_result))
  
  # Extract parameters
  if ("mle" %in% names(fit_result)) {
    params <- fit_result$mle
  } else if ("estimate" %in% names(fit_result)) {
    params <- fit_result$estimate
  } else {
    stop("Cannot extract parameters from fit_result")
  }
  
  location <- params[1]
  scale <- params[2]
  shape <- params[3]
  
  n <- length(data)
  
  # Diagnostic statistics
  diagnostics <- list()
  
  # 1. Probability integral transforms
  if (abs(shape) < 1e-8) {
    # Gumbel case
    u <- exp(-exp(-(data - location) / scale))
  } else {
    # General GEV
    z <- (data - location) / scale
    if (any(1 + shape * z <= 0)) {
      warning("Some observations outside support of fitted distribution")
      u <- rep(NA, length(data))
    } else {
      u <- exp(-(1 + shape * z)^(-1/shape))
    }
  }
  
  # Kolmogorov-Smirnov test
  ks_test <- ks.test(u[!is.na(u)], "punif")
  diagnostics$ks_test <- ks_test
  
  # Anderson-Darling test for uniformity (approximation)
  u_clean <- u[!is.na(u)]
  u_sorted <- sort(u_clean)
  n_clean <- length(u_clean)
  i <- seq_len(n_clean)
  ad_stat <- -n_clean - sum((2*i - 1) * (log(u_sorted) + log(1 - u_sorted[n_clean + 1 - i]))) / n_clean
  diagnostics$anderson_darling <- ad_stat
  
  # 2. Information criteria
  loglik <- sum(gev_loglik_vectorized(data, location, scale, shape))
  aic <- -2 * loglik + 2 * 3  # 3 parameters
  bic <- -2 * loglik + log(n) * 3
  diagnostics$loglik <- loglik
  diagnostics$aic <- aic
  diagnostics$bic <- bic
  
  # 3. Return level confidence intervals (profile likelihood)
  return_periods <- c(10, 50, 100, 500, 1000)
  rl_estimates <- numeric(length(return_periods))
  
  for (i in seq_along(return_periods)) {
    p <- 1 - 1/return_periods[i]
    if (abs(shape) < 1e-8) {
      rl_estimates[i] <- location - scale * log(-log(p))
    } else {
      rl_estimates[i] <- location + scale * ((-log(p))^(-shape) - 1) / shape
    }
  }
  
  diagnostics$return_levels <- data.frame(
    period = return_periods,
    estimate = rl_estimates
  )
  
  # 4. Generate plots if requested
  plots <- list()
  
  if ("qq" %in% plot_type && return_plots) {
    # Q-Q plot
    theoretical_quantiles <- qunif(ppoints(n_clean))
    qq_data <- data.frame(
      theoretical = theoretical_quantiles,
      observed = sort(u_clean)
    )
    
    plots$qq <- ggplot2::ggplot(qq_data, ggplot2::aes(x = theoretical, y = observed)) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      ggplot2::labs(title = "Q-Q Plot (Uniform Transform)",
                   x = "Theoretical Quantiles",
                   y = "Sample Quantiles") +
      ggplot2::theme_minimal()
  }
  
  if ("pp" %in% plot_type && return_plots) {
    # P-P plot
    pp_data <- data.frame(
      theoretical = seq_len(n_clean) / (n_clean + 1),
      observed = sort(u_clean)
    )
    
    plots$pp <- ggplot2::ggplot(pp_data, ggplot2::aes(x = theoretical, y = observed)) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      ggplot2::labs(title = "P-P Plot (Uniform Transform)",
                   x = "Theoretical Probabilities",
                   y = "Empirical Probabilities") +
      ggplot2::theme_minimal()
  }
  
  if ("return_level" %in% plot_type && return_plots) {
    # Return level plot
    emp_return_periods <- n / (n + 1 - rank(sort(data, decreasing = TRUE)))
    rl_data <- data.frame(
      period = emp_return_periods,
      level = sort(data, decreasing = TRUE)
    )
    
    # Theoretical curve
    theory_periods <- exp(seq(log(1.1), log(max(emp_return_periods) * 2), length.out = 100))
    theory_levels <- numeric(length(theory_periods))
    
    for (i in seq_along(theory_periods)) {
      p <- 1 - 1/theory_periods[i]
      if (abs(shape) < 1e-8) {
        theory_levels[i] <- location - scale * log(-log(p))
      } else {
        theory_levels[i] <- location + scale * ((-log(p))^(-shape) - 1) / shape
      }
    }
    
    theory_data <- data.frame(
      period = theory_periods,
      level = theory_levels
    )
    
    plots$return_level <- ggplot2::ggplot() +
      ggplot2::geom_point(data = rl_data, ggplot2::aes(x = period, y = level)) +
      ggplot2::geom_line(data = theory_data, ggplot2::aes(x = period, y = level), 
                        color = "red") +
      ggplot2::scale_x_log10() +
      ggplot2::labs(title = "Return Level Plot",
                   x = "Return Period",
                   y = "Return Level") +
      ggplot2::theme_minimal()
  }
  
  # 5. Residual analysis
  if ("residuals" %in% plot_type && return_plots) {
    # Standardized residuals
    z_residuals <- (data - location) / scale
    
    residual_data <- data.frame(
      index = seq_along(z_residuals),
      residuals = z_residuals
    )
    
    plots$residuals <- ggplot2::ggplot(residual_data, ggplot2::aes(x = index, y = residuals)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      ggplot2::labs(title = "Standardized Residuals",
                   x = "Index",
                   y = "Standardized Residuals") +
      ggplot2::theme_minimal()
  }
  
  result <- list(diagnostics = diagnostics)
  if (return_plots) {
    result$plots <- plots
  }
  
  result
}

#' Vectorized GEV log-likelihood for individual observations
#'
#' @param x Numeric vector of observations
#' @param location Numeric location parameter
#' @param scale Numeric scale parameter
#' @param shape Numeric shape parameter
#'
#' @return Numeric vector of log-likelihood contributions
gev_loglik_vectorized <- function(x, location, scale, shape) {
  if (scale <= 0) return(rep(-Inf, length(x)))
  
  z <- (x - location) / scale
  
  if (abs(shape) < 1e-8) {
    # Gumbel case
    ll <- -log(scale) - z - exp(-z)
  } else {
    # General GEV case
    if (any(1 + shape * z <= 0)) {
      ll <- rep(-Inf, length(x))
    } else {
      t <- 1 + shape * z
      ll <- -log(scale) - log(t) * (1/shape + 1) - t^(-1/shape)
    }
  }
  
  ll[!is.finite(ll)] <- -Inf
  ll
}

#' Cross-validation for threshold selection
#'
#' Use cross-validation to select optimal threshold for POT analysis.
#'
#' @param data Numeric vector of observations
#' @param thresholds Numeric vector of candidate thresholds
#' @param k_fold Integer number of folds for cross-validation
#' @param criterion Character: "loglik" or "aic"
#'
#' @return List with optimal threshold and CV results
#' @export
cv_threshold_selection <- function(data, thresholds = NULL, k_fold = 5, 
                                   criterion = "loglik") {
  assertthat::assert_that(is.numeric(data), length(data) > 20)
  assertthat::assert_that(criterion %in% c("loglik", "aic"))
  assertthat::assert_that(k_fold >= 3)
  
  if (is.null(thresholds)) {
    # Default threshold grid
    quantiles <- seq(0.8, 0.98, by = 0.02)
    thresholds <- quantile(data, quantiles)
  }
  
  n <- length(data)
  fold_size <- floor(n / k_fold)
  cv_scores <- matrix(NA, nrow = length(thresholds), ncol = k_fold)
  
  # Create fold indices
  folds <- split(sample(n), rep(1:k_fold, length.out = n))
  
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    
    for (j in 1:k_fold) {
      # Split data
      test_indices <- folds[[j]]
      train_data <- data[-test_indices]
      test_data <- data[test_indices]
      
      # Fit GPD on training data
      train_exceedances <- train_data[train_data > threshold] - threshold
      
      if (length(train_exceedances) < 5) {
        cv_scores[i, j] <- -Inf
        next
      }
      
      # Simple method-of-moments estimator for GPD
      mean_excess <- mean(train_exceedances)
      var_excess <- var(train_exceedances)
      
      if (var_excess <= 0) {
        cv_scores[i, j] <- -Inf
        next
      }
      
      # Moment estimators
      shape_est <- 0.5 * (mean_excess^2 / var_excess - 1)
      scale_est <- 0.5 * mean_excess * (mean_excess^2 / var_excess + 1)
      
      # Evaluate on test data
      test_exceedances <- test_data[test_data > threshold] - threshold
      
      if (length(test_exceedances) < 1) {
        cv_scores[i, j] <- -Inf
        next
      }
      
      # Calculate log-likelihood on test exceedances
      ll <- sum(gpd_loglik_vectorized(test_exceedances, scale_est, shape_est))
      
      if (criterion == "aic") {
        cv_scores[i, j] <- -2 * ll + 2 * 2  # 2 parameters
      } else {
        cv_scores[i, j] <- ll
      }
    }
  }
  
  # Average across folds
  mean_scores <- rowMeans(cv_scores, na.rm = TRUE)
  
  if (criterion == "aic") {
    optimal_idx <- which.min(mean_scores)
  } else {
    optimal_idx <- which.max(mean_scores)
  }
  
  list(
    optimal_threshold = thresholds[optimal_idx],
    cv_scores = cv_scores,
    mean_scores = mean_scores,
    thresholds = thresholds,
    criterion = criterion
  )
}

#' GPD log-likelihood for individual observations
#'
#' @param x Numeric vector of exceedances
#' @param scale Numeric scale parameter
#' @param shape Numeric shape parameter
#'
#' @return Numeric vector of log-likelihood contributions
gpd_loglik_vectorized <- function(x, scale, shape) {
  if (scale <= 0) return(rep(-Inf, length(x)))
  
  if (abs(shape) < 1e-8) {
    # Exponential case
    ll <- -log(scale) - x / scale
  } else {
    # General GPD case
    if (any(1 + shape * x / scale <= 0)) {
      ll <- rep(-Inf, length(x))
    } else {
      t <- 1 + shape * x / scale
      ll <- -log(scale) - log(t) * (1/shape + 1)
    }
  }
  
  ll[!is.finite(ll)] <- -Inf
  ll
}

#' Bootstrap confidence intervals for extremal index
#'
#' Enhanced bootstrap procedure with bias correction and acceleration.
#'
#' @param data Numeric vector of observations
#' @param threshold Numeric threshold value
#' @param method Character: "runs" or "intervals"
#' @param n_bootstrap Integer number of bootstrap samples
#' @param block_size Integer block size for block bootstrap
#' @param confidence_level Numeric confidence level
#'
#' @return List with bootstrap results and confidence intervals
#' @export
enhanced_bootstrap_ei <- function(data, threshold, method = "runs", 
                                  n_bootstrap = 1000, block_size = NULL,
                                  confidence_level = 0.95) {
  assertthat::assert_that(is.numeric(data), length(data) > 50)
  assertthat::assert_that(method %in% c("runs", "intervals"))
  
  n <- length(data)
  
  if (is.null(block_size)) {
    # Automatic block size selection
    block_size <- max(10, floor(n^(1/3)))
  }
  
  # Original estimate
  if (method == "runs") {
    original_est <- extremal_index_runs(data, threshold, run_length = 3)
  } else {
    original_est <- extremal_index_intervals(data, threshold)
  }
  
  # Block bootstrap
  n_blocks <- floor(n / block_size)
  bootstrap_estimates <- numeric(n_bootstrap)
  
  for (i in 1:n_bootstrap) {
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
      bootstrap_estimates[i] <- extremal_index_runs(bootstrap_data, threshold, run_length = 3)
    } else {
      bootstrap_estimates[i] <- extremal_index_intervals(bootstrap_data, threshold)
    }
  }
  
  # Remove non-finite estimates
  bootstrap_estimates <- bootstrap_estimates[is.finite(bootstrap_estimates)]
  
  if (length(bootstrap_estimates) < 10) {
    warning("Too few valid bootstrap estimates")
    return(list(estimate = original_est, ci = c(NA, NA)))
  }
  
  # Calculate confidence intervals
  alpha <- 1 - confidence_level
  ci_quantiles <- c(alpha/2, 1 - alpha/2)
  ci <- quantile(bootstrap_estimates, ci_quantiles)
  
  # BCa confidence intervals (simplified)
  bias_correction <- qnorm(mean(bootstrap_estimates < original_est))
  z_alpha2 <- qnorm(alpha/2)
  z_1alpha2 <- qnorm(1 - alpha/2)
  
  # Jackknife for acceleration
  jackknife_estimates <- numeric(n)
  for (i in 1:n) {
    jackknife_data <- data[-i]
    if (method == "runs") {
      jackknife_estimates[i] <- extremal_index_runs(jackknife_data, threshold, run_length = 3)
    } else {
      jackknife_estimates[i] <- extremal_index_intervals(jackknife_data, threshold)
    }
  }
  
  jackknife_mean <- mean(jackknife_estimates, na.rm = TRUE)
  acceleration <- sum((jackknife_mean - jackknife_estimates)^3, na.rm = TRUE) / 
                  (6 * (sum((jackknife_mean - jackknife_estimates)^2, na.rm = TRUE))^(3/2))
  
  # BCa-adjusted quantiles
  alpha1_adj <- pnorm(bias_correction + (bias_correction + z_alpha2) / 
                      (1 - acceleration * (bias_correction + z_alpha2)))
  alpha2_adj <- pnorm(bias_correction + (bias_correction + z_1alpha2) / 
                      (1 - acceleration * (bias_correction + z_1alpha2)))
  
  ci_bca <- quantile(bootstrap_estimates, c(alpha1_adj, alpha2_adj))
  
  list(
    estimate = original_est,
    bootstrap_estimates = bootstrap_estimates,
    ci_basic = ci,
    ci_bca = ci_bca,
    bias_correction = bias_correction,
    acceleration = acceleration,
    method = method,
    confidence_level = confidence_level
  )
}