#' Wrap fitted model objects with chaoticds metadata
#'
#' Internal helper to standardize fitted model objects while preserving their
#' original structure and class behavior.
#'
#' @param fit Underlying fitted model object.
#' @param model Character scalar identifying model family (e.g., "gev", "gpd").
#' @param method Character scalar identifying fitting backend.
#' @param threshold Optional numeric threshold for POT/GPD models.
#' @return The same object with class `chaotic_model` prepended and metadata
#'   stored in attributes.
wrap_chaotic_model <- function(fit, model, method, threshold = NULL) {
  class(fit) <- c("chaotic_model", class(fit))
  attr(fit, "chaotic_model") <- model
  attr(fit, "chaotic_method") <- method
  attr(fit, "chaotic_threshold") <- if (is.null(threshold)) NULL else unname(as.numeric(threshold))
  fit
}

.extract_param_vector <- function(x) {
  candidates <- c("estimate", "par.ests", "mle", "par", "coef")
  for (nm in candidates) {
    val <- x[[nm]]
    if (is.numeric(val)) {
      return(val)
    }
  }
  numeric(0)
}

#' Print chaoticds model summary header
#'
#' @param x Fitted model object returned by [fit_gev()] or [fit_gpd()].
#' @param ... Unused.
#' @return Invisibly returns `x`.
#' @export
print.chaotic_model <- function(x, ...) {
  model <- attr(x, "chaotic_model")
  method <- attr(x, "chaotic_method")
  threshold <- attr(x, "chaotic_threshold")

  cat("<chaotic_model>\n")
  cat("  Model:  ", model, "\n", sep = "")
  cat("  Method: ", method, "\n", sep = "")
  if (!is.null(threshold)) {
    cat("  Threshold: ", format(threshold), "\n", sep = "")
  }

  params <- .extract_param_vector(x)
  if (length(params) > 0) {
    cat("  Parameters:\n")
    print(round(params, 6))
  }
  invisible(x)
}

#' Summarize chaoticds model fits
#'
#' @param object Fitted model object returned by [fit_gev()] or [fit_gpd()].
#' @param ... Unused.
#' @return A list with standardized summary fields.
#' @export
summary.chaotic_model <- function(object, ...) {
  out <- list(
    model = attr(object, "chaotic_model"),
    method = attr(object, "chaotic_method"),
    threshold = attr(object, "chaotic_threshold"),
    parameters = .extract_param_vector(object),
    raw = object
  )
  class(out) <- "summary.chaotic_model"
  out
}

#' Print method for [summary.chaotic_model()]
#'
#' @param x Output of [summary.chaotic_model()].
#' @param ... Unused.
#' @return Invisibly returns `x`.
#' @export
print.summary.chaotic_model <- function(x, ...) {
  cat("chaoticds model summary\n")
  cat("  Model:  ", x$model, "\n", sep = "")
  cat("  Method: ", x$method, "\n", sep = "")
  if (!is.null(x$threshold)) {
    cat("  Threshold: ", format(x$threshold), "\n", sep = "")
  }
  if (length(x$parameters) > 0) {
    cat("  Parameters:\n")
    print(round(x$parameters, 6))
  }
  invisible(x)
}

#' Plot chaoticds model fits
#'
#' Delegates to the underlying model plot method when available.
#'
#' @param x Fitted model object returned by [fit_gev()] or [fit_gpd()].
#' @param ... Additional graphical arguments passed through.
#' @return The result of the delegated plotting method.
#' @export
plot.chaotic_model <- function(x, ...) {
  plot_func <- getS3method("plot", class(x)[2], optional = TRUE)
  if (!is.null(plot_func)) {
    return(plot_func(x, ...))
  }
  # Fallback to generic dispatch on original class if method lookup above fails.
  NextMethod()
}
