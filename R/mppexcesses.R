#' Marked Point Processes of Exceedances for Dynamical Systems
#'
#' @description
#' Implements Rare Events Point Processes (REPP), Excesses Over Threshold (EOT),
#' Peaks Over Threshold (POT), and Area Over Threshold (AOT) marked point processes
#' as described in Moreira–Freitas–Magalhães (2018). Provides tools for threshold selection,
#' cluster identification, MPP construction, and fitting compound Poisson limits.
#'
#' @docType package
#' @name mppexcesses
#' @importFrom checkmate assertFunction assertNumeric assertIntegerish assertCount assertNumber
#' @importFrom stats ecdf quantile optim
#' @importFrom extRemes fevd
NULL

#' Simulate Orbit of a One-Dimensional Map
#'
#' @param map_fn Function(x) returning numeric scalar. The dynamical map f: X->[0,1].
#' @param init Numeric scalar in domain of f. Initial condition.
#' @param n_iter Integer > 0. Number of iterations.
#' @return Numeric vector of length n_iter+1 (includes init). Orbit values.
#' @examples
#' orb <- simulate_orbit(function(x) 4*x*(1-x), init=0.1, n_iter=1e5)
#' @export
simulate_orbit <- function(map_fn, init, n_iter) {
  checkmate::assertFunction(map_fn, args = "x")
  checkmate::assertNumber(init)
  checkmate::assertCount(n_iter)
  orbit <- numeric(n_iter + 1)
  orbit[1] <- init
  for (i in seq_len(n_iter)) {
    orbit[i+1] <- map_fn(orbit[i])
  }
  orbit
}

#' Select High Threshold via Quantile
#'
#' @param X Numeric vector. Time series.
#' @param prob Numeric in (0,1). Exceedance probability level.
#' @return Numeric scalar. Threshold u.
#' @export
select_threshold <- function(X, prob = 0.99) {
  checkmate::assertNumeric(X, any.missing = FALSE)
  checkmate::assertNumber(prob, lower=0, upper=1)
  stats::quantile(X, probs = prob, names = FALSE, type = 8)
}

#' Identify Exceedance Indices
#'
#' @param X Numeric vector. Time series.
#' @param u Numeric scalar. Threshold.
#' @return Integer vector of time indices where X > u.
#' @export
exceedance_indices <- function(X, u) {
  checkmate::assertNumeric(X)
  checkmate::assertNumber(u)
  which(X > u)
}

#' Cluster Exceedances by Run-Length
#'
#' @param idx Integer vector sorted. Exceedance time indices.
#' @param run_length Integer >=0. Maximum gap to belong to same cluster.
#' @return List of integer vectors: each cluster's indices.
#' @export
cluster_exceedances <- function(idx, run_length = 1) {
  checkmate::assertIntegerish(idx, lower=1, sorted = TRUE)
  checkmate::assertCount(run_length, null.ok=FALSE)
  if (length(idx) == 0L) return(list())
  clusters <- list(); current <- idx[1]
  for (i in idx[-1]) {
    if (i - tail(current,1) <= run_length) {
      current <- c(current, i)
    } else {
      clusters[[length(clusters)+1]] <- current
      current <- i
    }
  }
  clusters[[length(clusters)+1]] <- current
  clusters
}

#' Build Marked Point Process
#'
#' @param X Numeric vector. Time series.
#' @param u Numeric scalar. Threshold.
#' @param run_length Integer. Cluster run-length p.
#' @param type Character: one of "REPP","EOT","POT","AOT".
#' @return Data.frame with columns time (start of cluster) and mark.
#' @export
marked_point_process <- function(X, u, run_length = 0L, type = c("REPP","EOT","POT","AOT")) {
  type <- match.arg(type)
  idx <- exceedance_indices(X, u)
  clusters <- cluster_exceedances(idx, run_length)
  # For each cluster compute time and mark
  mpp <- do.call(rbind, lapply(clusters, function(cl) {
    cluster_vals <- X[cl]
    mark <- switch(type,
      REPP = length(cl),
      EOT  = sum(cluster_vals - u),
      POT  = max(cluster_vals - u),
      AOT  = sum(cluster_vals - u)
    )
    data.frame(time = cl[1], mark = mark)
  }))
  rownames(mpp) <- NULL; mpp
}

#' Fit Compound Poisson Process
#'
#' @param mpp Data.frame with time and mark columns.
#' @param period Length of observation window.
#' @return List with lambda estimate and mark distribution fit.
#' @export
fit_compound_poisson <- function(mpp, period) {
  checkmate::assertDataFrame(mpp)
  checkmate::assertNumber(period, lower = 1)
  # rate = number of clusters / period
  lambda <- nrow(mpp) / period
  # Fit Pareto/GPD to marks
  gpd_fit <- extRemes::fevd(mpp$mark, threshold = 0, type = "GP")
  list(rate = lambda, gpd = gpd_fit)
}

#' Example: logistic map MPP analysis
#'
#' @examples
#' # simulate
#' orb <- simulate_orbit(function(x) 4*x*(1-x), init=0.2, n_iter=1e5)
#' # threshold
#' u <- select_threshold(orb, 0.99)
#' # compute POT process with p=2
#' mpp <- marked_point_process(orb, u, run_length=2, type="POT")
#' # fit compound Poisson
#' fit <- fit_compound_poisson(mpp, period=length(orb))
#' summary(fit$gpd)
