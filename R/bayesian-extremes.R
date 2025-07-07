#' Bayesian extreme value analysis
#'
#' Bayesian inference methods for extreme value parameters using MCMC.
#' These functions provide an alternative to classical MLE approaches.
#'
#' @name bayesian_extremes
NULL

#' Bayesian GEV parameter estimation
#'
#' Estimate GEV parameters using Bayesian MCMC methods.
#'
#' @param data Numeric vector of block maxima
#' @param n_iter Integer number of MCMC iterations
#' @param n_warmup Integer number of warmup iterations
#' @param prior_scale Numeric scale for weakly informative priors
#' @param method Character: "metropolis" or "stan" (if rstan available)
#'
#' @return List with posterior samples and summaries
#' @export
bayesian_gev <- function(data, n_iter = 2000, n_warmup = 1000, 
                         prior_scale = 10, method = "metropolis") {
  assertthat::assert_that(is.numeric(data), length(data) > 5)
  assertthat::assert_that(method %in% c("metropolis", "stan"))
  
  if (method == "stan" && requireNamespace("rstan", quietly = TRUE)) {
    return(bayesian_gev_stan(data, n_iter, n_warmup, prior_scale))
  }
  
  # Simple Metropolis-Hastings implementation
  n_data <- length(data)
  
  # Initialize parameters (location, scale, shape)
  params <- c(mean(data), sd(data), 0.1)
  param_names <- c("location", "scale", "shape")
  
  # Storage for samples
  samples <- matrix(NA, nrow = n_iter, ncol = 3)
  colnames(samples) <- param_names
  
  # Proposal covariance (tuned for typical GEV problems)
  prop_cov <- diag(c(0.1, 0.05, 0.02)) * var(data)
  
  n_accept <- 0
  
  for (i in 1:n_iter) {
    # Propose new parameters
    proposal <- MASS::mvrnorm(1, params, prop_cov)
    
    # Check constraints (scale > 0)
    if (proposal[2] <= 0) {
      samples[i, ] <- params
      next
    }
    
    # Log-likelihood
    ll_current <- gev_loglik(data, params[1], params[2], params[3])
    ll_proposal <- gev_loglik(data, proposal[1], proposal[2], proposal[3])
    
    # Log-prior (weakly informative)
    lp_current <- sum(dnorm(params, 0, prior_scale, log = TRUE))
    lp_proposal <- sum(dnorm(proposal, 0, prior_scale, log = TRUE))
    
    # Accept/reject
    log_ratio <- ll_proposal + lp_proposal - ll_current - lp_current
    
    if (is.finite(log_ratio) && log(runif(1)) < log_ratio) {
      params <- proposal
      if (i > n_warmup) n_accept <- n_accept + 1
    }
    
    samples[i, ] <- params
  }
  
  # Extract post-warmup samples
  post_warmup <- samples[(n_warmup + 1):n_iter, ]
  
  list(
    samples = post_warmup,
    summary = data.frame(
      parameter = param_names,
      mean = colMeans(post_warmup),
      sd = apply(post_warmup, 2, sd),
      q025 = apply(post_warmup, 2, quantile, 0.025),
      q975 = apply(post_warmup, 2, quantile, 0.975)
    ),
    acceptance_rate = n_accept / (n_iter - n_warmup),
    method = "metropolis"
  )
}

#' GEV log-likelihood function
#'
#' @param data Numeric vector of observations
#' @param location Numeric location parameter
#' @param scale Numeric scale parameter  
#' @param shape Numeric shape parameter
#'
#' @return Numeric log-likelihood value
gev_loglik <- function(data, location, scale, shape) {
  if (scale <= 0) return(-Inf)
  
  z <- (data - location) / scale
  
  if (abs(shape) < 1e-8) {
    # Gumbel case (shape = 0)
    ll <- -length(data) * log(scale) - sum(z) - sum(exp(-z))
  } else {
    # Fréchet (shape > 0) or Weibull (shape < 0)
    if (any(1 + shape * z <= 0)) return(-Inf)
    
    t <- 1 + shape * z
    ll <- -length(data) * log(scale) - sum(log(t) * (1/shape + 1)) - sum(t^(-1/shape))
  }
  
  if (!is.finite(ll)) return(-Inf)
  ll
}

#' Bayesian GPD parameter estimation
#'
#' Estimate GPD parameters using Bayesian MCMC methods.
#'
#' @param excesses Numeric vector of threshold excesses
#' @param n_iter Integer number of MCMC iterations
#' @param n_warmup Integer number of warmup iterations
#' @param prior_scale Numeric scale for weakly informative priors
#'
#' @return List with posterior samples and summaries
#' @export
bayesian_gpd <- function(excesses, n_iter = 2000, n_warmup = 1000, prior_scale = 10) {
  assertthat::assert_that(is.numeric(excesses), length(excesses) > 5)
  assertthat::assert_that(all(excesses >= 0))
  
  n_data <- length(excesses)
  
  # Initialize parameters (scale, shape)
  params <- c(mean(excesses), 0.1)
  param_names <- c("scale", "shape")
  
  # Storage for samples
  samples <- matrix(NA, nrow = n_iter, ncol = 2)
  colnames(samples) <- param_names
  
  # Proposal covariance
  prop_cov <- diag(c(0.05, 0.02)) * var(excesses)
  
  n_accept <- 0
  
  for (i in 1:n_iter) {
    # Propose new parameters
    proposal <- MASS::mvrnorm(1, params, prop_cov)
    
    # Check constraints (scale > 0)
    if (proposal[1] <= 0) {
      samples[i, ] <- params
      next
    }
    
    # Log-likelihood
    ll_current <- gpd_loglik(excesses, params[1], params[2])
    ll_proposal <- gpd_loglik(excesses, proposal[1], proposal[2])
    
    # Log-prior (weakly informative)
    lp_current <- dnorm(params[1], 0, prior_scale, log = TRUE) + 
                  dnorm(params[2], 0, 0.5, log = TRUE)  # Tighter prior on shape
    lp_proposal <- dnorm(proposal[1], 0, prior_scale, log = TRUE) + 
                   dnorm(proposal[2], 0, 0.5, log = TRUE)
    
    # Accept/reject
    log_ratio <- ll_proposal + lp_proposal - ll_current - lp_current
    
    if (is.finite(log_ratio) && log(runif(1)) < log_ratio) {
      params <- proposal
      if (i > n_warmup) n_accept <- n_accept + 1
    }
    
    samples[i, ] <- params
  }
  
  # Extract post-warmup samples
  post_warmup <- samples[(n_warmup + 1):n_iter, ]
  
  list(
    samples = post_warmup,
    summary = data.frame(
      parameter = param_names,
      mean = colMeans(post_warmup),
      sd = apply(post_warmup, 2, sd),
      q025 = apply(post_warmup, 2, quantile, 0.025),
      q975 = apply(post_warmup, 2, quantile, 0.975)
    ),
    acceptance_rate = n_accept / (n_iter - n_warmup),
    method = "metropolis"
  )
}

#' GPD log-likelihood function
#'
#' @param excesses Numeric vector of exceedances above threshold
#' @param scale Numeric scale parameter
#' @param shape Numeric shape parameter
#'
#' @return Numeric log-likelihood value
gpd_loglik <- function(excesses, scale, shape) {
  if (scale <= 0) return(-Inf)
  
  n <- length(excesses)
  
  if (abs(shape) < 1e-8) {
    # Exponential case (shape = 0)
    ll <- -n * log(scale) - sum(excesses) / scale
  } else {
    # General GPD case
    if (any(1 + shape * excesses / scale <= 0)) return(-Inf)
    
    t <- 1 + shape * excesses / scale
    ll <- -n * log(scale) - sum(log(t) * (1/shape + 1))
  }
  
  if (!is.finite(ll)) return(-Inf)
  ll
}

#' Bayesian return level estimation
#'
#' Estimate return levels with uncertainty quantification using posterior samples.
#'
#' @param posterior_samples Matrix of posterior samples (columns: parameters)
#' @param return_periods Numeric vector of return periods
#' @param model Character: "gev" or "gpd"
#' @param threshold Numeric threshold (for GPD only)
#' @param exceedance_rate Numeric annual exceedance rate (for GPD only)
#'
#' @return Data frame with return level estimates and credible intervals
#' @export
bayesian_return_levels <- function(posterior_samples, return_periods, model = "gev",
                                   threshold = NULL, exceedance_rate = NULL) {
  assertthat::assert_that(model %in% c("gev", "gpd"))
  assertthat::assert_that(is.matrix(posterior_samples))
  
  n_samples <- nrow(posterior_samples)
  n_periods <- length(return_periods)
  
  # Storage for return level samples
  rl_samples <- matrix(NA, nrow = n_samples, ncol = n_periods)
  
  if (model == "gev") {
    assertthat::assert_that(ncol(posterior_samples) == 3)
    
    for (i in 1:n_samples) {
      location <- posterior_samples[i, 1]
      scale <- posterior_samples[i, 2]
      shape <- posterior_samples[i, 3]
      
      for (j in 1:n_periods) {
        p <- 1 - 1/return_periods[j]
        
        if (abs(shape) < 1e-8) {
          # Gumbel case
          rl_samples[i, j] <- location - scale * log(-log(p))
        } else {
          # General GEV case
          rl_samples[i, j] <- location + scale * ((-log(p))^(-shape) - 1) / shape
        }
      }
    }
  } else {
    # GPD case
    assertthat::assert_that(ncol(posterior_samples) == 2)
    assertthat::assert_that(!is.null(threshold), !is.null(exceedance_rate))
    
    for (i in 1:n_samples) {
      scale <- posterior_samples[i, 1]
      shape <- posterior_samples[i, 2]
      
      for (j in 1:n_periods) {
        # Convert return period to exceedance probability
        p <- 1 - 1/(return_periods[j] * exceedance_rate)
        
        if (abs(shape) < 1e-8) {
          # Exponential case
          rl_samples[i, j] <- threshold - scale * log(1 - p)
        } else {
          # General GPD case
          rl_samples[i, j] <- threshold + scale * ((1 - p)^(-shape) - 1) / shape
        }
      }
    }
  }
  
  # Summarize return level samples
  results <- data.frame(
    return_period = return_periods,
    mean = colMeans(rl_samples),
    sd = apply(rl_samples, 2, sd),
    q025 = apply(rl_samples, 2, quantile, 0.025),
    q050 = apply(rl_samples, 2, quantile, 0.5),
    q975 = apply(rl_samples, 2, quantile, 0.975)
  )
  
  results
}

#' Load required package for MASS functions
#' @importFrom MASS mvrnorm
.onLoad <- function(libname, pkgname) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    packageStartupMessage("MASS package is recommended for Bayesian methods")
  }
}