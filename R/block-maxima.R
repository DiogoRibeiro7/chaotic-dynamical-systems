#' Extract Block Maxima from Time Series
#'
#' @description
#' Divides a time series into non-overlapping blocks of equal size and
#' extracts the maximum value from each block. This is a fundamental method
#' in extreme value theory for analyzing the distribution of extreme events.
#'
#' @details
#' ## Overview
#' The block maxima method is one of two classical approaches in extreme
#' value theory (EVT), along with peaks-over-threshold (POT). It transforms
#' a time series of length n into approximately n/block_size maxima, which
#' under appropriate conditions converge to a Generalized Extreme Value (GEV)
#' distribution.
#'
#' ## When to Use Block Maxima
#' Block maxima is appropriate when:
#' - You want to analyze extremes over fixed time periods (e.g., annual maxima)
#' - Your data naturally divides into meaningful blocks (seasons, years, cycles)
#' - You prefer working with fewer, more independent observations
#' - Theoretical conditions for GEV approximation are met
#'
#' ## Comparison to Peaks-Over-Threshold
#' **Advantages**:
#' - Simpler conceptually
#' - Automatically provides approximately independent observations
#' - Well-established theory and inference procedures
#'
#' **Disadvantages**:
#' - Less efficient: discards all non-maximal values in each block
#' - Requires choosing block size (bias-variance tradeoff)
#' - May not be suitable for short time series
#'
#' Generally, POT is preferred when you have long time series and want to
#' extract more information from the data.
#'
#' ## Choosing Block Size
#' Block size selection involves a tradeoff:
#' - **Too small**: Maxima may not be independent, GEV approximation poor
#' - **Too large**: Few blocks, high variance in estimates
#'
#' Rules of thumb:
#' - At least 20-50 blocks for reliable estimation
#' - Blocks should span a "natural" time scale of the system
#' - For chaotic systems: consider the correlation time or Lyapunov timescale
#'
#' @section Mathematical Background:
#' Let \eqn{X_1, X_2, \ldots, X_n} be a stationary sequence and divide it into
#' k blocks of size m (so n ≈ km). The block maxima are:
#' \deqn{M_i = \max\{X_{(i-1)m+1}, \ldots, X_{im}\}, \quad i = 1,\ldots,k}
#'
#' Under appropriate mixing and regularity conditions, as m → ∞, the
#' distribution of (normalized) \eqn{M_i} converges to the GEV distribution:
#' \deqn{G(z) = \exp\{-(1 + \xi z)^{-1/\xi}\}}
#' where \eqn{\xi} is the shape parameter.
#'
#' For dependent sequences (like chaotic systems), the extremal index \eqn{\theta}
#' adjusts the limiting distribution to account for clustering.
#'
#' @param x Numeric vector. The time series from which to extract block maxima.
#'   Should not contain NA or infinite values. Typical length: at least
#'   20 × block_size for reliable statistical analysis.
#'
#' @param block_size Integer. Size of each block. Must be positive and should
#'   be much smaller than length(x). Recommended:
#'   - Start with length(x) / 50 and adjust based on diagnostics
#'   - Ensure at least 20 blocks: block_size ≤ length(x) / 20
#'   - For chaotic systems with known mixing time, use multiples of mixing time
#'
#' @return
#' Numeric vector of length floor(length(x) / block_size) containing the
#' maximum value from each block. The vector has length k where k is the
#' number of complete blocks. Any remaining observations (if length(x) is
#' not divisible by block_size) are discarded.
#'
#' The returned maxima can be used with \code{\link{fit_gev}} to estimate
#' GEV parameters and quantify extreme value behavior.
#'
#' @references
#' Coles, S. (2001). *An Introduction to Statistical Modeling of Extreme Values*.
#' Springer. Chapter 3. \doi{10.1007/978-1-4471-3675-0}
#'
#' Leadbetter, M. R., Lindgren, G., & Rootzén, H. (1983). *Extremes and
#' Related Properties of Random Sequences and Processes*. Springer.
#'
#' @seealso
#' \code{\link{fit_gev}} to fit the GEV distribution to block maxima,
#' \code{\link{exceedances}} for the alternative POT approach,
#' \code{\link{threshold_diagnostics}} for diagnostic plots.
#'
#' See \code{vignette("block-maxima-vs-pot-henon")} for a detailed comparison
#' of block maxima and POT methods.
#'
#' @family extreme value functions
#'
#' @examples
#' # Simulate chaotic time series
#' series <- simulate_logistic_map(n = 2000, r = 3.8, x0 = 0.2)
#'
#' # Extract block maxima with block size 50
#' bm <- block_maxima(series, block_size = 50)
#' length(bm)  # Should be 40 blocks
#'
#' # Visualize the block maxima
#' plot(bm, type = "h", lwd = 2, col = "darkred",
#'      main = "Block Maxima (block size = 50)",
#'      xlab = "Block", ylab = "Maximum Value")
#'
#' # Compare to original series
#' par(mfrow = c(2, 1))
#' plot(series, type = "l", col = "gray", main = "Original Time Series",
#'      xlab = "Iteration", ylab = "x")
#' plot(bm, type = "h", lwd = 2, col = "darkred",
#'      main = "Block Maxima", xlab = "Block", ylab = "Max")
#' par(mfrow = c(1, 1))
#'
#' # Distribution of block maxima
#' hist(bm, breaks = 15, probability = TRUE, col = "lightblue",
#'      border = "white", main = "Distribution of Block Maxima",
#'      xlab = "Block Maximum")
#' lines(density(bm), col = "darkblue", lwd = 2)
#'
#' # Effect of block size on number of maxima
#' for (bs in c(25, 50, 100, 200)) {
#'   bm_temp <- block_maxima(series, bs)
#'   cat("Block size", bs, ": ", length(bm_temp), "maxima\n")
#' }
#'
#' \donttest{
#' # Fit GEV distribution to block maxima
#' if (requireNamespace("evd", quietly = TRUE)) {
#'   gev_fit <- fit_gev(bm)
#'   print(gev_fit)
#' }
#' }
#'
#' @export
block_maxima <- function(x, block_size) {
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_int(block_size, lower = 1)
  if (length(x) < block_size) {
    stop("length(x) must be >= block_size")
  }
  n_blocks <- floor(length(x) / block_size)
  idx <- seq_len(n_blocks * block_size)
  mat <- matrix(x[idx], nrow = block_size, ncol = n_blocks)
  apply(mat, 2L, max)
}

#' Fit Generalized Extreme Value Distribution to Block Maxima
#'
#' @description
#' Fits a Generalized Extreme Value (GEV) distribution to block maxima data
#' using maximum likelihood estimation. The GEV distribution is the limiting
#' distribution for block maxima under appropriate conditions.
#'
#' @details
#' ## Overview
#' The GEV distribution combines three classical extreme value distributions
#' (Gumbel, Fréchet, and Weibull) into a single family. It is parameterized
#' by location (μ), scale (σ > 0), and shape (ξ) parameters.
#'
#' This function provides a convenient wrapper that uses `evd::fgev()` if
#' the evd package is available, otherwise falls back to `ismev::gev.fit()`.
#' Both implementations use maximum likelihood estimation.
#'
#' ## GEV Distribution
#' The cumulative distribution function is:
#' \deqn{G(z) = \exp\{-(1 + \xi z)^{-1/\xi}\}}
#' where z = (x - μ)/σ and the support depends on ξ.
#'
#' ## Shape Parameter Interpretation
#' The shape parameter ξ controls tail behavior:
#' - **ξ > 0**: Fréchet type (heavy tail, power-law decay)
#' - **ξ = 0**: Gumbel type (light tail, exponential decay)
#' - **ξ < 0**: Weibull type (bounded tail, finite upper endpoint)
#'
#' For chaotic systems, ξ is often close to zero or slightly negative,
#' depending on the system's dynamics and the observable being studied.
#'
#' ## Model Diagnostics
#' After fitting, always check:
#' - Probability plots (Q-Q, P-P)
#' - Return level plots
#' - Confidence intervals for parameters
#' - Likelihood-based diagnostics
#'
#' @section Mathematical Background:
#' Fisher-Tippett theorem states that if normalized block maxima converge
#' to a non-degenerate distribution, it must be the GEV. For a sequence
#' \eqn{M_n = \max(X_1, \ldots, X_n)}, there exist sequences \eqn{a_n > 0}
#' and \eqn{b_n} such that:
#' \deqn{P((M_n - b_n)/a_n \le z) \to G(z)}
#' as n → ∞, where G is the GEV distribution.
#'
#' @param block_maxima Numeric vector of block maxima, typically obtained
#'   from \code{\link{block_maxima}}. Should contain at least 20-30 values
#'   for reliable estimation, though more (50+) is preferable. Must not
#'   contain NA or infinite values.
#'
#' @return
#' A fitted GEV model object. The exact class depends on which package
#' is used:
#' - If evd is available: object of class "uvevd" from `evd::fgev()`
#' - If only ismev is available: list from `ismev::gev.fit()`
#'
#' Both objects contain:
#' - **Estimated parameters**: location (μ), scale (σ), shape (ξ)
#' - **Standard errors**: asymptotic standard errors for parameters
#' - **Log-likelihood**: maximized log-likelihood value
#' - **Convergence information**: optimization convergence status
#'
#' Use `summary()` to display parameter estimates and standard errors.
#' Use diagnostic plots from the respective package for model validation.
#'
#' @references
#' Jenkinson, A. F. (1955). The frequency distribution of the annual maximum
#' (or minimum) values of meteorological elements. *Quarterly Journal of the
#' Royal Meteorological Society*, 81(348), 158-171. \doi{10.1002/qj.49708134804}
#'
#' Coles, S. (2001). *An Introduction to Statistical Modeling of Extreme
#' Values*. Springer. Chapter 3. \doi{10.1007/978-1-4471-3675-0}
#'
#' Prescott, P., & Walden, A. T. (1980). Maximum likelihood estimation of
#' the parameters of the generalized extreme-value distribution. *Biometrika*,
#' 67(3), 723-724.
#'
#' @seealso
#' \code{\link{block_maxima}} for extracting block maxima from time series,
#' \code{\link{fit_gpd}} for the alternative POT approach with GPD distribution,
#' \code{\link{threshold_diagnostics}} for threshold selection in POT.
#'
#' For diagnostic plots, see the documentation of `evd::fgev()` or
#' `ismev::gev.fit()` depending on which package you have installed.
#'
#' @family extreme value functions
#'
#' @examples
#' # Simulate chaotic time series and extract block maxima
#' series <- simulate_logistic_map(n = 5000, r = 3.8, x0 = 0.2)
#' bm <- block_maxima(series, block_size = 100)
#'
#' \donttest{
#' # Fit GEV distribution
#' if (requireNamespace("evd", quietly = TRUE)) {
#'   gev_fit <- fit_gev(bm)
#'
#'   # Display parameter estimates
#'   print(gev_fit)
#'   summary(gev_fit)
#'
#'   # Extract parameters
#'   params <- gev_fit$estimate  # For evd::fgev
#'   cat("Location:", params["loc"], "\n")
#'   cat("Scale:", params["scale"], "\n")
#'   cat("Shape:", params["shape"], "\n")
#'
#'   # Interpretation of shape parameter
#'   if (params["shape"] > 0.1) {
#'     cat("Heavy-tailed distribution (Fréchet)\n")
#'   } else if (params["shape"] < -0.1) {
#'     cat("Bounded distribution (Weibull)\n")
#'   } else {
#'     cat("Exponential-tailed distribution (Gumbel)\n")
#'   }
#' }
#' }
#'
#' # Compare GEV fit to empirical distribution
#' bm2 <- block_maxima(simulate_logistic_map(10000, 3.8, 0.2), 100)
#' hist(bm2, breaks = 20, probability = TRUE, col = "lightblue",
#'      border = "white", main = "Block Maxima Distribution",
#'      xlab = "Maximum Value")
#'
#' \donttest{
#' # Overlay fitted GEV density
#' if (requireNamespace("evd", quietly = TRUE)) {
#'   fit2 <- fit_gev(bm2)
#'   x_seq <- seq(min(bm2), max(bm2), length.out = 200)
#'   lines(x_seq, evd::dgev(x_seq, loc = fit2$estimate["loc"],
#'                          scale = fit2$estimate["scale"],
#'                          shape = fit2$estimate["shape"]),
#'         col = "darkred", lwd = 2)
#'   legend("topright", legend = "Fitted GEV", col = "darkred",
#'          lwd = 2, bty = "n")
#' }
#' }
#'
#' @export
fit_gev <- function(block_maxima) {
  checkmate::assert_numeric(block_maxima, any.missing = FALSE, min.len = 2)
  if (requireNamespace("evd", quietly = TRUE)) {
    evd::fgev(block_maxima)
  } else if (requireNamespace("ismev", quietly = TRUE)) {
    ismev::gev.fit(block_maxima, show = FALSE)
  } else {
    stop("Package 'evd' or 'ismev' required for GEV fitting")
  }
}
