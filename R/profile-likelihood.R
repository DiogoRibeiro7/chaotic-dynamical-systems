# ---------------------------------------------------------------------------
# Profile-likelihood inference for chaotic_model GEV/GPD fits.
#
# Why bother: Wald intervals on the shape parameter xi are notoriously
# asymmetric and miscalibrated -- the standard EVT correction is to invert
# the likelihood-ratio test, i.e. find {theta_i : 2*(ll_hat - ll_profile)
# <= qchisq(level, 1)}. We grid the target parameter around its MLE, refit
# the remaining parameters at each grid point with optim() (Nelder-Mead),
# and read the CI off the level set by linear interpolation.
#
# The implementation is independent of the fitting backend (evd, ismev,
# evir) so the same profile machinery works for any wrap_chaotic_model
# output, modulo the data/parameter extraction helpers below.
# ---------------------------------------------------------------------------

# Log-likelihoods ----------------------------------------------------------

.gev_loglik <- function(par, x) {
  mu    <- par[1L]
  sigma <- par[2L]
  xi    <- par[3L]
  if (!is.finite(sigma) || sigma <= 0) return(-Inf)
  z <- (x - mu) / sigma
  if (abs(xi) < 1e-8) {
    -length(x) * log(sigma) - sum(z) - sum(exp(-z))
  } else {
    s <- 1 + xi * z
    if (any(!is.finite(s)) || any(s <= 0)) return(-Inf)
    -length(x) * log(sigma) - (1 + 1 / xi) * sum(log(s)) - sum(s^(-1 / xi))
  }
}

.gpd_loglik <- function(par, y) {
  sigma <- par[1L]
  xi    <- par[2L]
  if (!is.finite(sigma) || sigma <= 0) return(-Inf)
  if (abs(xi) < 1e-8) {
    -length(y) * log(sigma) - sum(y / sigma)
  } else {
    s <- 1 + xi * y / sigma
    if (any(!is.finite(s)) || any(s <= 0)) return(-Inf)
    -length(y) * log(sigma) - (1 + 1 / xi) * sum(log(s))
  }
}

# Extraction helpers -------------------------------------------------------

.extract_fit_data <- function(fit) {
  model <- attr(fit, "chaotic_model")
  if (identical(model, "gev") || identical(model, "gev_rlargest")) {
    raw <- fit$data %||% fit$xdata
    if (!is.null(raw)) {
      if (is.matrix(raw)) {
        # r-largest fits store an n_blocks x r matrix; the GEV log-lik
        # extractor is for vector data, so flatten using the column-1
        # block maxima as a safe stand-in.
        return(list(data = as.numeric(raw[, 1L]), threshold = NULL))
      }
      return(list(data = as.numeric(raw), threshold = NULL))
    }
    stop("Could not extract block-maxima data from GEV fit")
  }
  if (identical(model, "gpd")) {
    threshold <- attr(fit, "chaotic_threshold")
    if (is.null(threshold)) stop("GPD fit is missing its threshold attribute")
    if (!is.null(fit$exceedances)) {
      return(list(data = as.numeric(fit$exceedances), threshold = threshold))
    }
    raw <- fit$data %||% fit$xdata
    if (is.null(raw)) stop("Could not extract input data from GPD fit")
    raw <- as.numeric(raw)
    excs <- raw[raw > threshold] - threshold
    return(list(data = excs, threshold = threshold))
  }
  stop("Profile likelihood is only implemented for GEV and GPD chaotic_model fits")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

.extract_fit_params <- function(fit) {
  model <- attr(fit, "chaotic_model")
  v <- .extract_param_vector(fit)
  if (identical(model, "gev")) {
    if (!is.null(names(v)) && all(c("loc", "scale", "shape") %in% names(v))) {
      return(c(mu    = unname(v[["loc"]]),
               sigma = unname(v[["scale"]]),
               xi    = unname(v[["shape"]])))
    }
    if (length(v) == 3L) {
      return(c(mu = v[1L], sigma = v[2L], xi = v[3L]))
    }
    stop("Could not extract GEV (mu, sigma, xi) from fit")
  }
  if (identical(model, "gpd")) {
    if (!is.null(names(v)) && all(c("scale", "shape") %in% names(v))) {
      return(c(sigma = unname(v[["scale"]]),
               xi    = unname(v[["shape"]])))
    }
    if (length(v) == 2L) {
      return(c(sigma = v[1L], xi = v[2L]))
    }
    stop("Could not extract GPD (sigma, xi) from fit")
  }
  stop("Unknown chaotic_model: ", model)
}

.extract_fit_se <- function(fit, n_params) {
  se <- fit$std.err %||% fit$se
  if (is.null(se) || length(se) < n_params) return(rep(NA_real_, n_params))
  as.numeric(se)[seq_len(n_params)]
}

# Profile-likelihood ------------------------------------------------------

.param_aliases <- list(
  gev = c(location = "mu", loc = "mu", mu = "mu",
          scale    = "sigma", sigma = "sigma",
          shape    = "xi", xi = "xi"),
  gpd = c(scale = "sigma", sigma = "sigma",
          shape = "xi",    xi = "xi")
)

#' Profile-likelihood inference for a GEV or GPD fit
#'
#' @description
#' Compute the profile log-likelihood curve for one parameter of a fitted
#' GEV or GPD model, plus its likelihood-ratio confidence interval. Wald
#' (standard-error) intervals on the shape parameter \eqn{\xi} are
#' notoriously asymmetric and miscalibrated; profile-likelihood intervals
#' are the standard fix.
#'
#' @details
#' At each fixed value of the target parameter we maximise the GEV (resp.
#' GPD) log-likelihood over the remaining parameters via [stats::optim()]
#' (Nelder-Mead) and record the profile log-likelihood. The CI is then
#' \deqn{\{\theta : 2(\hat{\ell} - \ell_{\text{profile}}(\theta)) \le
#' \chi^2_{1, \alpha}\}}
#' with the endpoints read off the grid by linear interpolation. The
#' implementation is independent of the fitting backend (`evd`, `ismev`,
#' `evir`) so any [fit_gev()] or [fit_gpd()] result works.
#'
#' @param fit A `chaotic_model` returned by [fit_gev()] or [fit_gpd()].
#' @param parameter Character. Which parameter to profile. For GEV:
#'   `"location"` / `"scale"` / `"shape"` (aliases `"mu"`, `"sigma"`,
#'   `"xi"`). For GPD: `"scale"` / `"shape"`.
#' @param level Confidence level for the CI. Defaults to 0.95.
#' @param n_points Integer. Number of grid points across the profile.
#'   Defaults to 41, which is dense enough for stable linear interpolation
#'   on the level set while staying cheap to compute.
#' @param span Numeric. Half-width of the grid in units of the parameter's
#'   standard error around the MLE. Defaults to 4 (a span of 4 standard
#'   errors typically brackets a 95% profile interval comfortably).
#'
#' @return An object of class `profile_likelihood`, a list with:
#'   \describe{
#'     \item{parameter, model, level}{The arguments echoed back.}
#'     \item{grid}{Numeric vector of values at which the parameter was
#'       fixed.}
#'     \item{log_lik}{Profile log-likelihood at each grid point.}
#'     \item{max_log_lik}{Log-likelihood at the unconstrained MLE.}
#'     \item{threshold_ll}{The level set used to invert the LRT:
#'       `max_log_lik - qchisq(level, 1) / 2`.}
#'     \item{mle}{MLE value of the profiled parameter.}
#'     \item{ci}{Length-2 named numeric vector `c(lower, upper)`. Either
#'       endpoint is `NA` when the profile curve does not bracket the
#'       level set within the grid span -- widen `span` and rerun if so.}
#'   }
#'
#' @references
#' Coles, S. (2001). *An Introduction to Statistical Modeling of Extreme
#' Values*. Springer, sections 2.6.5 and 3.3.3.
#'
#' @seealso [profile_ci()] for a tidy data-frame summary,
#'   [bootstrap_extremal_index()] for an alternative uncertainty
#'   quantification approach on the extremal index.
#'
#' @examples
#' set.seed(1)
#' x  <- evd::rgev(500, loc = 0, scale = 1, shape = 0.1)
#' fit <- fit_gev(x)
#' pl  <- profile_likelihood(fit, "shape")
#' pl
#' plot(pl)
#'
#' @export
profile_likelihood <- function(fit, parameter,
                               level    = 0.95,
                               n_points = 41L,
                               span     = 4) {
  checkmate::assert_class(fit, "chaotic_model")
  checkmate::assert_string(parameter)
  checkmate::assert_number(level, lower = 0.5, upper = 0.999)
  checkmate::assert_int(n_points, lower = 5L)
  checkmate::assert_number(span, lower = 0.1, finite = TRUE)

  model <- attr(fit, "chaotic_model")
  if (!model %in% c("gev", "gpd")) {
    stop("Profile likelihood is only implemented for GEV and GPD fits")
  }

  aliases <- .param_aliases[[model]]
  parameter <- tolower(parameter)
  if (!parameter %in% names(aliases)) {
    stop(sprintf("Unknown parameter %s for model %s. Use one of: %s",
                 sQuote(parameter), model,
                 paste(unique(names(aliases)), collapse = ", ")))
  }
  param_key <- unname(aliases[parameter])

  par_mle <- .extract_fit_params(fit)
  par_se  <- .extract_fit_se(fit, length(par_mle))
  data_info <- .extract_fit_data(fit)
  data <- data_info$data

  idx <- match(param_key, names(par_mle))
  if (is.na(idx)) stop("Unable to locate parameter ", param_key, " in fit")

  mle_value <- par_mle[[idx]]
  se        <- par_se[idx]
  if (!is.finite(se) || se <= 0) {
    # Fall back to a fraction of |mle_value| (or a small absolute width if
    # the MLE itself is near zero) when the fit failed to report SEs.
    se <- max(abs(mle_value), 0.1) * 0.1
  }
  grid <- mle_value + seq(-span * se, span * se, length.out = n_points)

  ll_full <- if (model == "gev") .gev_loglik else .gpd_loglik

  fit_at_fixed <- function(fixed_value) {
    objective <- function(other_par) {
      par <- numeric(length(par_mle))
      par[idx]  <- fixed_value
      par[-idx] <- other_par
      -ll_full(par, data)
    }
    start <- as.numeric(par_mle[-idx])
    # BFGS handles both 1D (the inner optimisation when profiling a GPD
    # parameter) and >=2D (every GEV profile) without warnings; Nelder-Mead
    # emits "one-dimensional optimization is unreliable" in the GPD case.
    out <- tryCatch(
      stats::optim(start, objective, method = "BFGS",
                   control = list(reltol = 1e-8, maxit = 500L)),
      error = function(e) NULL
    )
    if (is.null(out) || !is.finite(out$value)) return(NA_real_)
    -out$value
  }

  ll_max <- ll_full(as.numeric(par_mle), data)
  profile_ll  <- vapply(grid, fit_at_fixed, numeric(1L))
  threshold_ll <- ll_max - stats::qchisq(level, df = 1L) / 2
  ci <- .invert_profile(grid, profile_ll, threshold_ll, mle_value)

  structure(
    list(
      parameter     = parameter,
      parameter_key = param_key,
      model         = model,
      grid          = grid,
      log_lik       = profile_ll,
      threshold_ll  = threshold_ll,
      mle           = mle_value,
      max_log_lik   = ll_max,
      ci            = ci,
      level         = level
    ),
    class = "profile_likelihood"
  )
}

# Invert the LRT: linear-interpolate where profile_ll first crosses thr on
# each side of the MLE. Returns NA when the grid does not bracket the
# crossing -- caller should widen `span` in that case.
.invert_profile <- function(grid, ll, thr, mle) {
  ll[is.na(ll) | !is.finite(ll)] <- -Inf

  cross_between <- function(i) {
    # Linear interpolation between grid[i] and grid[i+1] for crossing thr.
    g1 <- grid[i];     g2 <- grid[i + 1L]
    l1 <- ll[i];       l2 <- ll[i + 1L]
    if (l2 == l1) return(g1)
    g1 + (thr - l1) / (l2 - l1) * (g2 - g1)
  }

  left_segments  <- which(grid[-length(grid)] < mle & grid[-1L] <= mle)
  right_segments <- which(grid[-length(grid)] >= mle & grid[-1L] > mle)

  # Lower endpoint: rightmost segment on the left half where ll crosses up.
  lower <- NA_real_
  for (i in rev(left_segments)) {
    if (ll[i] < thr && ll[i + 1L] >= thr) {
      lower <- cross_between(i)
      break
    }
  }
  # Upper endpoint: leftmost segment on the right half where ll crosses down.
  upper <- NA_real_
  for (i in right_segments) {
    if (ll[i] >= thr && ll[i + 1L] < thr) {
      upper <- cross_between(i)
      break
    }
  }
  c(lower = lower, upper = upper)
}

#' Print method for profile-likelihood objects
#'
#' @param x A `profile_likelihood` object.
#' @param ... Unused.
#' @return Invisibly returns `x`.
#' @export
print.profile_likelihood <- function(x, ...) {
  cat("<profile_likelihood>\n")
  cat("  Model:        ", x$model,     "\n", sep = "")
  cat("  Parameter:    ", x$parameter, " (", x$parameter_key, ")\n", sep = "")
  cat("  Level:        ", x$level,     "\n", sep = "")
  cat("  MLE:          ", format(x$mle, digits = 6), "\n", sep = "")
  ci_lo <- if (is.na(x$ci[["lower"]])) "NA (widen span)" else format(x$ci[["lower"]], digits = 6)
  ci_hi <- if (is.na(x$ci[["upper"]])) "NA (widen span)" else format(x$ci[["upper"]], digits = 6)
  cat("  ", round(x$level * 100), "% profile CI: [", ci_lo, ", ", ci_hi, "]\n", sep = "")
  invisible(x)
}

#' Plot a profile-likelihood curve
#'
#' Plots the profile log-likelihood as a function of the profiled parameter,
#' with horizontal and vertical reference lines showing the LRT cutoff and
#' the CI endpoints.
#'
#' @param x A `profile_likelihood` object.
#' @param ... Unused.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_line geom_hline geom_vline labs theme_minimal
#' @export
plot.profile_likelihood <- function(x, ...) {
  df <- data.frame(value = x$grid, log_lik = x$log_lik)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = value, y = log_lik)) +
    ggplot2::geom_line(colour = "steelblue", linewidth = 0.6) +
    ggplot2::geom_hline(yintercept = x$threshold_ll,
                        linetype = "dashed", colour = "firebrick") +
    ggplot2::geom_vline(xintercept = x$mle, linetype = "dotted") +
    ggplot2::labs(
      title = sprintf("Profile log-likelihood: %s (%s)", x$parameter, x$model),
      subtitle = sprintf("MLE = %.4g, %d%% CI = [%.4g, %.4g]",
                         x$mle, round(x$level * 100),
                         x$ci[["lower"]], x$ci[["upper"]]),
      x = x$parameter,
      y = "profile log-likelihood"
    ) +
    ggplot2::theme_minimal()
  if (!is.na(x$ci[["lower"]])) {
    p <- p + ggplot2::geom_vline(xintercept = x$ci[["lower"]],
                                 linetype = "dashed", colour = "firebrick")
  }
  if (!is.na(x$ci[["upper"]])) {
    p <- p + ggplot2::geom_vline(xintercept = x$ci[["upper"]],
                                 linetype = "dashed", colour = "firebrick")
  }
  p
}

#' Profile-likelihood interval for a GEV or GPD return level
#'
#' @description
#' Returns the profile-likelihood confidence interval for the m-period
#' return level `z_m`. This is the EVT analogue of a calibrated interval
#' on an extrapolated quantile: where Wald intervals on `z_m` are
#' notoriously skewed (especially for long return periods), the
#' profile-likelihood interval respects the curvature of the likelihood
#' surface.
#'
#' @details
#' The return level is the value exceeded on average once per `m` periods.
#' For GEV (block-based) with block size implicit in the fit, `m` counts
#' *blocks*. For GPD (POT-based) with `n_per_year` observations per year
#' and exceedance rate \eqn{\zeta_u} = P(X > u), `m` counts *years* and
#' the function uses `m * n_per_year * exceedance_rate` as the effective
#' rate parameter.
#'
#' Internally we reparametrise the GEV / GPD log-likelihood so that `z_m`
#' is a free parameter and the others are nuisance:
#'
#' - GEV: \eqn{\mu(z_m, \sigma, \xi) = z_m + \sigma/\xi \, (1 - y_m^{-\xi})}
#'   with \eqn{y_m = -\log(1 - 1/m)}, falling back to
#'   \eqn{\mu = z_m + \sigma \log y_m} for the Gumbel limit \eqn{\xi = 0}.
#' - GPD: \eqn{\sigma(z_m, \xi) = \xi (z_m - u) / ((m n_y \zeta_u)^\xi - 1)},
#'   with the \eqn{\xi = 0} limit handled separately.
#'
#' At each fixed `z_m` the remaining parameters are refit by BFGS, and the
#' CI is read off the LRT level set just like [profile_likelihood()].
#'
#' @param fit A `chaotic_model` returned by [fit_gev()], [fit_gev_rlargest()],
#'   or [fit_gpd()].
#' @param m Numeric. The return period (in blocks for GEV, in years for
#'   GPD when `n_per_year > 1`). Must be greater than 1.
#' @param level Confidence level. Defaults to 0.95.
#' @param n_per_year Numeric (\eqn{> 0}). Observations per year. Only used
#'   for GPD fits; ignored for GEV.
#' @param exceedance_rate Numeric in (0, 1). For GPD fits, the empirical
#'   probability that the underlying process exceeds the threshold. If
#'   `NULL` (default), the function infers it as `length(exceedances) /
#'   length(raw_data)` -- which requires that `fit$data` carries the full
#'   raw series, not just the excesses.
#' @param n_points Integer. Number of grid points across `z_m`.
#' @param span Numeric (\eqn{> 0}). Half-width of the grid expressed as a
#'   fraction of \eqn{|\hat{z}_m|}, with a small absolute fallback when
#'   \eqn{\hat{z}_m} is near zero. Widen if the returned CI endpoints are
#'   `NA`.
#'
#' @return A `profile_likelihood` object (so [print()] and [plot()] work
#'   unchanged), with `parameter = "return_level_<m>"`, `mle = z_m_hat`,
#'   and `ci = c(lower, upper)`.
#'
#' @examples
#' set.seed(1)
#' fit <- fit_gev(evd::rgev(500, 0, 1, 0.1))
#' profile_return_level(fit, m = 100)
#'
#' @export
profile_return_level <- function(fit, m, level = 0.95, n_per_year = 1,
                                 exceedance_rate = NULL,
                                 n_points = 41L, span = 0.5) {
  checkmate::assert_class(fit, "chaotic_model")
  checkmate::assert_number(m, lower = 1.001, finite = TRUE)
  checkmate::assert_number(level, lower = 0.5, upper = 0.999)
  checkmate::assert_number(n_per_year, lower = 1e-6, finite = TRUE)
  checkmate::assert_int(n_points, lower = 5L)
  checkmate::assert_number(span, lower = 1e-6, finite = TRUE)

  model <- attr(fit, "chaotic_model")
  if (!model %in% c("gev", "gev_rlargest", "gpd")) {
    stop("Return-level profile is only implemented for GEV/GPD fits")
  }

  par_mle <- .extract_fit_params(fit)
  data_info <- .extract_fit_data(fit)
  data <- data_info$data

  if (model %in% c("gev", "gev_rlargest")) {
    mu_hat    <- par_mle[["mu"]]
    sigma_hat <- par_mle[["sigma"]]
    xi_hat    <- par_mle[["xi"]]
    y_m <- -log(1 - 1 / m)

    z_m_hat <- if (abs(xi_hat) < 1e-8) {
      mu_hat - sigma_hat * log(y_m)
    } else {
      mu_hat - sigma_hat / xi_hat * (1 - y_m^(-xi_hat))
    }
    ll_max <- .gev_loglik(c(mu_hat, sigma_hat, xi_hat), data)

    fit_at_z <- function(z_m) {
      objective <- function(par) {
        sigma <- par[1L]
        xi    <- par[2L]
        if (!is.finite(sigma) || sigma <= 0) return(Inf)
        mu <- if (abs(xi) < 1e-8) {
          z_m + sigma * log(y_m)
        } else {
          z_m + sigma / xi * (1 - y_m^(-xi))
        }
        -.gev_loglik(c(mu, sigma, xi), data)
      }
      out <- tryCatch(
        stats::optim(c(sigma_hat, xi_hat), objective, method = "BFGS",
                     control = list(reltol = 1e-8, maxit = 500L)),
        error = function(e) NULL
      )
      if (is.null(out) || !is.finite(out$value)) return(NA_real_)
      -out$value
    }
  } else {
    # GPD branch
    sigma_hat <- par_mle[["sigma"]]
    xi_hat    <- par_mle[["xi"]]
    threshold <- data_info$threshold

    raw <- fit$data %||% fit$xdata
    if (is.null(exceedance_rate)) {
      if (is.null(raw)) {
        stop("exceedance_rate must be supplied (no raw data on the fit to infer it)")
      }
      raw_num <- as.numeric(raw)
      exceedance_rate <- mean(raw_num > threshold)
    }
    checkmate::assert_number(exceedance_rate, lower = 1e-10, upper = 1 - 1e-10)

    m_eff <- m * n_per_year * exceedance_rate

    z_m_hat <- if (abs(xi_hat) < 1e-8) {
      threshold + sigma_hat * log(m_eff)
    } else {
      threshold + sigma_hat / xi_hat * (m_eff^xi_hat - 1)
    }
    ll_max <- .gpd_loglik(c(sigma_hat, xi_hat), data)

    fit_at_z <- function(z_m) {
      objective <- function(par) {
        xi <- par[1L]
        sigma <- if (abs(xi) < 1e-8) {
          (z_m - threshold) / log(m_eff)
        } else {
          xi * (z_m - threshold) / (m_eff^xi - 1)
        }
        if (!is.finite(sigma) || sigma <= 0) return(Inf)
        -.gpd_loglik(c(sigma, xi), data)
      }
      out <- tryCatch(
        stats::optim(xi_hat, objective, method = "BFGS",
                     control = list(reltol = 1e-8, maxit = 500L)),
        error = function(e) NULL
      )
      if (is.null(out) || !is.finite(out$value)) return(NA_real_)
      -out$value
    }
  }

  half_width <- max(abs(z_m_hat) * span, span)
  grid <- seq(z_m_hat - half_width, z_m_hat + half_width, length.out = n_points)
  profile_ll <- vapply(grid, fit_at_z, numeric(1L))
  threshold_ll <- ll_max - stats::qchisq(level, df = 1L) / 2
  ci <- .invert_profile(grid, profile_ll, threshold_ll, z_m_hat)

  structure(
    list(
      parameter     = sprintf("return_level_%g", m),
      parameter_key = "z_m",
      model         = paste0(model, "_return_level"),
      grid          = grid,
      log_lik       = profile_ll,
      threshold_ll  = threshold_ll,
      mle           = z_m_hat,
      max_log_lik   = ll_max,
      ci            = ci,
      level         = level,
      m             = m
    ),
    class = "profile_likelihood"
  )
}

#' Profile-likelihood confidence intervals for a GEV or GPD fit
#'
#' Convenience wrapper that calls [profile_likelihood()] for each parameter
#' (or a chosen subset) and returns a tidy data frame.
#'
#' @param fit A `chaotic_model` returned by [fit_gev()] or [fit_gpd()].
#' @param parameter Character vector of parameter names. If `NULL`
#'   (default), all model parameters are profiled.
#' @param level Confidence level. Defaults to 0.95.
#' @param ... Passed to [profile_likelihood()] (e.g. `n_points`, `span`).
#'
#' @return A data frame with columns `parameter`, `estimate`, `lower`,
#'   `upper`. Either CI endpoint is `NA` if the profile curve did not
#'   bracket the LRT cutoff within the grid span.
#'
#' @examples
#' set.seed(1)
#' x  <- evd::rgev(500, loc = 0, scale = 1, shape = 0.1)
#' fit <- fit_gev(x)
#' profile_ci(fit)
#'
#' @export
profile_ci <- function(fit, parameter = NULL, level = 0.95, ...) {
  checkmate::assert_class(fit, "chaotic_model")
  model <- attr(fit, "chaotic_model")
  defaults <- list(gev = c("location", "scale", "shape"),
                   gpd = c("scale", "shape"))
  if (is.null(parameter)) parameter <- defaults[[model]]
  res <- lapply(parameter, function(p) {
    pl <- profile_likelihood(fit, p, level = level, ...)
    data.frame(parameter = p,
               estimate  = unname(pl$mle),
               lower     = unname(pl$ci[["lower"]]),
               upper     = unname(pl$ci[["upper"]]))
  })
  do.call(rbind, res)
}
