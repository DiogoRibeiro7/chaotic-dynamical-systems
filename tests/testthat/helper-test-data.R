# Helper functions for generating test data
# This file provides utilities for creating consistent test datasets

#' Generate test data for extremal index estimation
#' @param n Number of observations
#' @param extremal_index Target extremal index (approximate)
#' @param seed Random seed for reproducibility
generate_test_extremal_data <- function(n = 1000, extremal_index = 0.5, seed = 123) {
  set.seed(seed)
  
  if (extremal_index == 1) {
    # Independent data (theta = 1)
    return(rnorm(n))
  } else {
    # Create clustered extremes by using logistic map
    # The chaotic nature creates dependence, reducing extremal index
    r_param <- 3.7 + 0.2 * (1 - extremal_index)  # Adjust chaos level
    simulate_logistic_map(n, r = r_param, x0 = 0.2)
  }
}

#' Generate bivariate test data with known dependence structure
#' @param n Number of observations
#' @param correlation Correlation between components
#' @param marginal_type Type of marginal distribution ("normal", "logistic")
generate_bivariate_test_data <- function(n = 1000, correlation = 0.5, marginal_type = "normal") {
  set.seed(123)
  
  if (marginal_type == "normal") {
    # Bivariate normal
    mu <- c(0, 0)
    Sigma <- matrix(c(1, correlation, correlation, 1), nrow = 2)
    mvn_data <- MASS::mvrnorm(n, mu, Sigma)
    return(data.frame(x = mvn_data[,1], y = mvn_data[,2]))
  } else if (marginal_type == "logistic") {
    # Use Hénon map for chaotic bivariate data
    tryCatch({
      simulate_henon_map(n, a = 1.4, b = 0.3)
    }, error = function(e) {
      # Fallback to simple bivariate data
      x <- rnorm(n)
      y <- correlation * x + sqrt(1 - correlation^2) * rnorm(n)
      data.frame(x = x, y = y)
    })
  }
}

#' Generate extreme value test scenarios
#' @param scenario Type of scenario ("no_extremes", "many_extremes", "clustered", "isolated")
#' @param n Number of observations
generate_extreme_scenarios <- function(scenario = "clustered", n = 1000) {
  set.seed(123)
  
  switch(scenario,
    "no_extremes" = rep(0.1, n),
    "many_extremes" = rep(0.9, n),
    "clustered" = {
      # Create data with clustered extremes
      base_data <- rep(0.1, n)
      # Add some clusters of extremes
      cluster_starts <- c(100, 300, 700)
      for (start in cluster_starts) {
        if (start + 10 <= n) {
          base_data[start:(start + 10)] <- 0.9
        }
      }
      base_data
    },
    "isolated" = {
      # Create data with isolated extremes
      base_data <- rep(0.1, n)
      extreme_positions <- seq(50, n - 50, by = 100)
      base_data[extreme_positions] <- 0.9
      base_data
    },
    "mixed" = {
      # Mixed pattern with both clustered and isolated extremes
      base_data <- rep(runif(n, 0, 0.3), n)
      # Add clusters
      cluster_starts <- c(100, 500)
      for (start in cluster_starts) {
        if (start + 5 <= n) {
          base_data[start:(start + 5)] <- runif(6, 0.8, 1.0)
        }
      }
      # Add isolated extremes
      isolated_positions <- c(200, 350, 750)
      isolated_positions <- isolated_positions[isolated_positions <= n]
      base_data[isolated_positions] <- runif(length(isolated_positions), 0.85, 0.95)
      base_data
    }
  )
}

#' Generate performance test datasets of various sizes
#' @param sizes Vector of dataset sizes to generate
#' @param type Type of data ("logistic", "henon", "mixed")
generate_performance_datasets <- function(sizes = c(1000, 5000, 10000), type = "logistic") {
  datasets <- list()
  
  for (i in seq_along(sizes)) {
    n <- sizes[i]
    set.seed(123 + i)  # Different seed for each size
    
    if (type == "logistic") {
      datasets[[paste0("logistic_", n)]] <- simulate_logistic_map(n, r = 3.8, x0 = 0.2)
    } else if (type == "henon") {
      tryCatch({
        datasets[[paste0("henon_", n)]] <- simulate_henon_map(n, a = 1.4, b = 0.3)
      }, error = function(e) {
        # Fallback if Hénon simulation not available
        datasets[[paste0("henon_", n)]] <- data.frame(
          x = simulate_logistic_map(n, r = 3.8, x0 = 0.2),
          y = simulate_logistic_map(n, r = 3.6, x0 = 0.5)
        )
      })
    } else if (type == "mixed") {
      datasets[[paste0("mixed_", n)]] <- list(
        logistic = simulate_logistic_map(n, r = 3.8, x0 = 0.2),
        normal = rnorm(n),
        uniform = runif(n)
      )
    }
  }
  
  datasets
}

#' Create test data with known statistical properties
#' @param distribution Type of distribution ("exponential", "pareto", "weibull")
#' @param n Number of observations
#' @param ... Additional parameters for the distribution
generate_known_distribution_data <- function(distribution = "exponential", n = 1000, ...) {
  set.seed(123)
  
  switch(distribution,
    "exponential" = rexp(n, rate = 1),
    "pareto" = {
      # Simple Pareto distribution
      alpha <- list(...)$alpha %||% 2
      x_min <- list(...)$x_min %||% 1
      u <- runif(n)
      x_min * (1 - u)^(-1/alpha)
    },
    "weibull" = {
      shape <- list(...)$shape %||% 2
      scale <- list(...)$scale %||% 1
      rweibull(n, shape = shape, scale = scale)
    },
    "uniform" = runif(n),
    "normal" = rnorm(n)
  )
}

#' Default operator for list access
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Validate test data properties
#' @param data Test data to validate
#' @param expected_length Expected length
#' @param expected_range Expected range (vector of length 2)
validate_test_data <- function(data, expected_length = NULL, expected_range = NULL) {
  if (!is.null(expected_length)) {
    checkmate::assert_true(length(data) == expected_length)
  }

  if (!is.null(expected_range)) {
    checkmate::assert_number(expected_range[1])
    checkmate::assert_number(expected_range[2])
    checkmate::assert_true(min(data, na.rm = TRUE) >= expected_range[1])
    checkmate::assert_true(max(data, na.rm = TRUE) <= expected_range[2])
  }

  # Check for invalid values
  checkmate::assert_true(!any(is.na(data)))
  checkmate::assert_true(!any(is.infinite(data)))
  
  TRUE
}