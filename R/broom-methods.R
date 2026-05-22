# ---------------------------------------------------------------------------
# broom-style tidiers for chaotic_model fits.
#
# tidy()    -> one row per parameter (term, estimate, std.error, statistic,
#              p.value when available, conf.low/conf.high if requested).
# glance()  -> one-row model summary (nobs, logLik, AIC, BIC, threshold,
#              model, method).
# augment() -> input data + fitted CDF / survival columns for diagnostic
#              plotting.
#
# These methods register against the generics in the `generics` package
# (the broom generics live there). They make chaotic_model fits drop
# straight into dplyr / ggplot2 pipelines.
# ---------------------------------------------------------------------------

.term_names <- function(model) {
  switch(model,
    gev          = c("location", "scale", "shape"),
    gev_rlargest = c("location", "scale", "shape"),
    ppp          = c("location", "scale", "shape"),
    gpd          = c("scale", "shape"),
    NA_character_
  )
}

.term_index <- function(model) {
  switch(model,
    gev          = c(loc = 1L, scale = 2L, shape = 3L),
    gev_rlargest = c(loc = 1L, scale = 2L, shape = 3L),
    gpd          = c(scale = 1L, shape = 2L)
  )
}

.fit_nobs <- function(fit) {
  for (slot in c("nobs", "n", "n.exceedances", "length")) {
    val <- fit[[slot]]
    if (is.numeric(val) && length(val) == 1L) return(as.integer(val))
  }
  data <- fit$data %||% fit$xdata
  if (!is.null(data)) return(length(as.numeric(data)))
  NA_integer_
}

.fit_loglik <- function(fit) {
  for (slot in c("loglik", "nllh", "deviance")) {
    val <- fit[[slot]]
    if (is.numeric(val) && length(val) == 1L) {
      # evd and ismev store negative log-likelihood -- both deviance and
      # nllh are negated; loglik is the actual value when present.
      return(if (slot == "loglik") val else -val)
    }
  }
  NA_real_
}

#' Tidy a chaoticds GEV or GPD fit
#'
#' Returns one row per fitted parameter with point estimates and (when the
#' fit reports them) Wald standard errors.
#'
#' @param x A `chaotic_model` returned by [fit_gev()] or [fit_gpd()].
#' @param conf.int Logical. If `TRUE`, attach Wald confidence intervals.
#'   Defaults to `FALSE`. For better-calibrated intervals on the shape
#'   parameter use [profile_ci()] instead.
#' @param conf.level Numeric in (0, 1). Confidence level when `conf.int`
#'   is `TRUE`. Defaults to 0.95.
#' @param ... Unused.
#'
#' @return A data frame with columns `term`, `estimate`, `std.error`, and
#'   (when `conf.int = TRUE`) `conf.low`, `conf.high`.
#'
#' @examples
#' set.seed(1)
#' fit <- fit_gev(evd::rgev(500, 0, 1, 0.1))
#' tidy(fit)
#'
#' @importFrom generics tidy
#' @exportS3Method generics::tidy chaotic_model
tidy.chaotic_model <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  model  <- attr(x, "chaotic_model")
  params <- .extract_param_vector(x)
  terms  <- .term_names(model)

  if (length(params) == 0L || all(is.na(terms))) {
    return(data.frame(
      term = character(0),
      estimate = numeric(0),
      std.error = numeric(0)
    ))
  }

  if (length(params) != length(terms)) {
    terms <- if (!is.null(names(params))) names(params) else
             paste0("p", seq_along(params))
  }

  se <- .extract_fit_se(x, length(params))

  out <- data.frame(
    term      = terms,
    estimate  = unname(as.numeric(params)),
    std.error = unname(as.numeric(se))
  )

  if (isTRUE(conf.int)) {
    checkmate::assert_number(conf.level, lower = 0, upper = 1)
    z <- stats::qnorm(1 - (1 - conf.level) / 2)
    out$conf.low  <- out$estimate - z * out$std.error
    out$conf.high <- out$estimate + z * out$std.error
  }

  out
}

#' One-row summary of a chaoticds GEV or GPD fit
#'
#' Returns the kind of single-row diagnostics summary broom's `glance()`
#' produces for ordinary `lm`/`glm` fits.
#'
#' @param x A `chaotic_model` returned by [fit_gev()] or [fit_gpd()].
#' @param ... Unused.
#'
#' @return A data frame with one row and columns `model`, `method`,
#'   `threshold` (NA for GEV), `nobs`, `logLik`, `AIC`, `BIC`.
#'
#' @examples
#' set.seed(1)
#' fit <- fit_gev(evd::rgev(500, 0, 1, 0.1))
#' glance(fit)
#'
#' @importFrom generics glance
#' @exportS3Method generics::glance chaotic_model
glance.chaotic_model <- function(x, ...) {
  model  <- attr(x, "chaotic_model")
  method <- attr(x, "chaotic_method")
  thr    <- attr(x, "chaotic_threshold") %||% NA_real_

  nobs   <- .fit_nobs(x)
  loglik <- .fit_loglik(x)
  k      <- length(.extract_param_vector(x))

  aic <- if (is.finite(loglik) && k > 0L)               2 * k - 2 * loglik else NA_real_
  bic <- if (is.finite(loglik) && k > 0L && !is.na(nobs)) log(nobs) * k - 2 * loglik else NA_real_

  data.frame(
    model     = model     %||% NA_character_,
    method    = method    %||% NA_character_,
    threshold = as.numeric(thr),
    nobs      = nobs,
    logLik    = loglik,
    AIC       = aic,
    BIC       = bic,
    stringsAsFactors = FALSE
  )
}

#' Augment a chaoticds GEV or GPD fit with fitted CDF / survival columns
#'
#' Returns the input data alongside model-implied CDF and survival
#' (exceedance-probability) values at each observation. Useful for
#' PP / QQ-style diagnostic plots.
#'
#' @param x A `chaotic_model` returned by [fit_gev()] or [fit_gpd()].
#' @param data Optional numeric vector. If `NULL` (default), the data
#'   stored in the fit are used. For GPD this should be the *raw* series
#'   (the function will subset to exceedances above the threshold).
#' @param ... Unused.
#'
#' @return A data frame with columns `index`, `value`, `cdf`, `survival`,
#'   ordered to match the input.
#'
#' @examples
#' set.seed(1)
#' fit <- fit_gev(evd::rgev(500, 0, 1, 0.1))
#' head(augment(fit))
#'
#' @importFrom generics augment
#' @exportS3Method generics::augment chaotic_model
augment.chaotic_model <- function(x, data = NULL, ...) {
  model  <- attr(x, "chaotic_model")
  params <- .extract_param_vector(x)
  if (length(params) < 2L) {
    stop("augment() needs at least two fitted parameters on the chaotic_model")
  }

  if (is.null(data)) {
    data <- .extract_fit_data(x)$data
  }
  checkmate::assert_numeric(data, finite = TRUE, min.len = 1L)

  if (model %in% c("gev", "gev_rlargest", "ppp")) {
    mu    <- params[[1L]]
    sigma <- params[[2L]]
    xi    <- params[[3L]]
    z <- (data - mu) / sigma
    if (abs(xi) < 1e-8) {
      cdf <- exp(-exp(-z))
    } else {
      s <- 1 + xi * z
      cdf <- ifelse(s > 0, exp(-s^(-1 / xi)), ifelse(xi > 0, 0, 1))
    }
  } else if (identical(model, "gpd")) {
    sigma <- params[[1L]]
    xi    <- params[[2L]]
    thr   <- attr(x, "chaotic_threshold") %||% 0
    y <- pmax(data - thr, 0)
    if (abs(xi) < 1e-8) {
      cdf <- 1 - exp(-y / sigma)
    } else {
      s <- 1 + xi * y / sigma
      cdf <- ifelse(s > 0, 1 - s^(-1 / xi), 1)
    }
  } else {
    stop("augment() is not implemented for model ", model)
  }

  data.frame(
    index    = seq_along(data),
    value    = as.numeric(data),
    cdf      = as.numeric(cdf),
    survival = 1 - as.numeric(cdf)
  )
}
