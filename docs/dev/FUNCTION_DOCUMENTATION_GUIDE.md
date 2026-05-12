# Function Documentation Enhancement Guide

**Purpose**: Transform function documentation from basic to publication-quality, making the package accessible to beginners while maintaining technical rigor.

---

## Table of Contents

1. [Documentation Structure](#documentation-structure)
2. [Roxygen2 Tags Reference](#roxygen2-tags-reference)
3. [Function Type Templates](#function-type-templates)
4. [Writing Guidelines](#writing-guidelines)
5. [Priority Functions to Enhance](#priority-functions-to-enhance)
6. [Before/After Examples](#beforeafter-examples)

---

## Documentation Structure

### Complete Template

```r
#' Function Title: Clear, Concise Action (One Line)
#'
#' @description
#' One or two sentences describing what this function does.
#' Focus on the WHAT, not the HOW.
#'
#' @details
#' ## Overview
#' Detailed explanation of how the function works, when to use it,
#' and any important considerations.
#'
#' ## Algorithm
#' Brief description of the underlying algorithm or method.
#'
#' ## When to Use
#' Guidance on when this function is appropriate vs alternatives.
#'
#' @section Mathematical Background:
#' Mathematical formulas, theory, or statistical concepts.
#' Use LaTeX notation in \eqn{} or \deqn{}.
#'
#' Example:
#' The logistic map is defined as:
#' \deqn{x_{n+1} = r \cdot x_n \cdot (1 - x_n)}
#'
#' @section Performance:
#' (Optional) Performance characteristics, computational complexity,
#' memory usage, or when to use C++ implementations.
#'
#' @param param_name Description of parameter.
#'   Should include:
#'   - **Type**: numeric, character, logical, etc.
#'   - **Constraints**: valid range, required properties
#'   - **Typical values**: what users commonly use
#'   - **Default behavior**: if applicable
#'
#' @return
#' Clear description of return value.
#'   - **Type**: vector, list, S3 object
#'   - **Structure**: what elements/components it contains
#'   - **Interpretation**: how to use the result
#'
#'   For lists, use itemized format:
#'   A list with components:
#'   \item{component1}{Description}
#'   \item{component2}{Description}
#'
#'   For S3 objects, describe the class and methods available.
#'
#' @references
#' Author, A. (Year). Title of paper. *Journal Name*, Volume(Issue), pages.
#' DOI: \doi{10.xxxx/xxxxx}
#'
#' Author, B. (Year). *Book Title*. Publisher.
#' \url{https://...}
#'
#' @seealso
#' Related functions:
#' \code{\link{related_function1}} for related functionality,
#' \code{\link{related_function2}} for alternative approach.
#'
#' @family function_category
#'
#' @examples
#' # Basic usage
#' result <- my_function(x = 1:10)
#'
#' # Typical use case with interpretation
#' series <- simulate_logistic_map(n = 500, r = 3.8, x0 = 0.2)
#' # This generates a chaotic time series
#'
#' # Advanced example
#' # ... more complex scenario ...
#'
#' # Edge case handling
#' # ... show how function handles edge cases ...
#'
#' \donttest{
#' # Long-running example (takes > 5 seconds)
#' large_result <- my_function(n = 100000)
#' }
#'
#' \dontrun{
#' # Example requiring user interaction or external resources
#' interactive_example()
#' }
#'
#' @export
```

---

## Roxygen2 Tags Reference

### @description
- **Length**: 1-3 sentences
- **Purpose**: High-level summary of function's purpose
- **Tone**: Clear, accessible to beginners
- **Example**:
  ```r
  #' @description
  #' Simulates a time series from the logistic map, a classic example of
  #' deterministic chaos. The logistic map exhibits complex dynamics including
  #' fixed points, periodic orbits, and chaotic behavior depending on parameter r.
  ```

### @details
- **Length**: Multiple paragraphs
- **Structure**: Use markdown headers (##) for subsections
- **Content**:
  - How the function works
  - When to use it vs alternatives
  - Important caveats or limitations
  - Common use cases
- **Example**:
  ```r
  #' @details
  #' ## Overview
  #' The block maxima method divides a time series into non-overlapping blocks
  #' and extracts the maximum value from each block. This is one of two primary
  #' approaches in extreme value analysis, along with peaks-over-threshold (POT).
  #'
  #' ## When to Use Block Maxima
  #' - You want to analyze extremes over fixed time periods
  #' - Your data naturally divides into blocks (e.g., annual maxima)
  #' - You prefer working with fewer, independent observations
  #'
  #' ## Comparison to POT
  #' Block maxima is simpler but may be less efficient than POT because it
  #' discards all non-maximal values within each block. See
  #' \code{\link{exceedances}} for the POT approach.
  ```

### @section Custom Section
- **Use for**: Mathematical background, performance notes, implementation details
- **Example**:
  ```r
  #' @section Mathematical Background:
  #' The extremal index \eqn{\theta \in (0,1]} measures the clustering of extreme
  #' events in a stationary sequence. For independent data, \eqn{\theta = 1}.
  #' For dependent data with clustering, \eqn{\theta < 1}, with smaller values
  #' indicating stronger clustering.
  #'
  #' The runs estimator (Leadbetter et al., 1983) defines clusters based on
  #' runs of consecutive exceedances:
  #' \deqn{\hat{\theta} = \frac{NC}{N}}
  #' where NC is the number of clusters and N is the number of exceedances.
  ```

### @param
- **Format**: `@param name Description`
- **Content**: Type, constraints, typical values, units
- **Style**: Use imperative mood ("Number of iterations to simulate")
- **Good examples**:
  ```r
  #' @param n Integer. Number of iterations to simulate. Must be positive.
  #'   Typical values: 500-10000. Larger n provides better statistical
  #'   properties but takes longer to compute.
  #'
  #' @param r Numeric. The logistic map parameter controlling dynamics.
  #'   Valid range: [0, 4]. Key values:
  #'   - r < 3: Convergence to fixed point
  #'   - 3 < r < 3.57: Period doubling
  #'   - r > 3.57: Chaotic regime
  #'   Default: 3.8 (fully chaotic).
  #'
  #' @param threshold Numeric scalar. Values above this threshold are
  #'   considered exceedances. Should be a high quantile of the data,
  #'   typically between 0.90 and 0.99. Use \code{\link{threshold_diagnostics}}
  #'   to help choose an appropriate value.
  ```

### @return
- **Structure**: Type, then detailed description
- **For vectors**: Include length, range, interpretation
- **For lists**: Itemize all components
- **For S3 objects**: Mention class and available methods
- **Examples**:
  ```r
  #' @return
  #' Numeric vector of length n containing the simulated time series.
  #' Values are bounded in (0, 1) due to the logistic map dynamics.
  #' The first value equals x0.

  #' @return
  #' A list with components:
  #' \item{estimate}{Numeric. The extremal index estimate, between 0 and 1}
  #' \item{threshold}{Numeric. The threshold value used}
  #' \item{n_exceedances}{Integer. Number of values exceeding threshold}
  #' \item{n_clusters}{Integer. Number of distinct clusters}
  #' \item{run_length}{Integer. The run length parameter used}

  #' @return
  #' An object of class "extremal_index" with print and summary methods.
  #' The object is a list containing the estimate, threshold, and cluster
  #' statistics. Use \code{print()} for a brief summary or \code{summary()}
  #' for detailed statistics.
  ```

### @examples
- **Structure**:
  1. Basic example (simple, runs quickly)
  2. Typical use case with interpretation
  3. Advanced example (optional)
  4. Edge cases (optional)
  5. Long-running examples in `\donttest{}`
- **Style**:
  - Include comments explaining what each example demonstrates
  - Show the output or interpretation
  - Use realistic parameter values
  - Make examples self-contained
- **Example**:
  ```r
  #' @examples
  #' # Basic usage: Generate 500 iterations
  #' series <- simulate_logistic_map(n = 500, r = 3.8, x0 = 0.2)
  #' head(series)
  #'
  #' # Visualize the chaotic dynamics
  #' plot(series, type = "l", main = "Logistic Map (r = 3.8)",
  #'      xlab = "Iteration", ylab = "x")
  #'
  #' # Compare different parameter values
  #' par(mfrow = c(2, 2))
  #' plot(simulate_logistic_map(200, r = 2.5), type = "l",
  #'      main = "r = 2.5 (Fixed Point)")
  #' plot(simulate_logistic_map(200, r = 3.2), type = "l",
  #'      main = "r = 3.2 (Periodic)")
  #' plot(simulate_logistic_map(200, r = 3.8), type = "l",
  #'      main = "r = 3.8 (Chaotic)")
  #' plot(simulate_logistic_map(200, r = 4.0), type = "l",
  #'      main = "r = 4.0 (Fully Chaotic)")
  #' par(mfrow = c(1, 1))
  #'
  #' \donttest{
  #' # Generate very long series for statistical analysis
  #' # This takes ~10 seconds
  #' long_series <- simulate_logistic_map(n = 1000000, r = 3.8, x0 = 0.2)
  #' }
  ```

### @family
- **Purpose**: Group related functions
- **Usage**: `@family category_name`
- **Categories for chaoticds**:
  - simulation functions
  - extreme value functions
  - extremal index functions
  - diagnostic functions
  - threshold selection functions
  - bootstrap functions
  - mixing functions
  - recurrence functions
  - utility functions

### @seealso
- **Purpose**: Link to related functions, vignettes, or help topics
- **Format**: Use `\code{\link{function_name}}` for functions
- **Example**:
  ```r
  #' @seealso
  #' \code{\link{extremal_index_intervals}} for the intervals method estimator,
  #' \code{\link{bootstrap_extremal_index}} for confidence intervals,
  #' \code{\link{cluster_sizes}} for analyzing cluster structure.
  #'
  #' For threshold selection guidance, see \code{\link{threshold_diagnostics}}
  #' and \code{vignette("threshold-selection")}.
  ```

### @references
- **Format**: Standard academic citation
- **Include**: DOI using `\doi{}`, URLs using `\url{}`
- **Example**:
  ```r
  #' @references
  #' Leadbetter, M. R. (1983). Extremes and local dependence in stationary
  #' sequences. *Zeitschrift für Wahrscheinlichkeitstheorie und verwandte
  #' Gebiete*, 65(2), 291-306. \doi{10.1007/BF00532484}
  #'
  #' Coles, S. (2001). *An Introduction to Statistical Modeling of Extreme
  #' Values*. Springer. \doi{10.1007/978-1-4471-3675-0}
  #'
  #' Freitas, A. C. M., Freitas, J. M., & Todd, M. (2010). Hitting time
  #' statistics and extreme value theory. *Probability Theory and Related
  #' Fields*, 147(3-4), 675-710. \doi{10.1007/s00440-009-0221-y}
  ```

---

## Function Type Templates

### Template 1: Simulation Functions

```r
#' Simulate [System Name] Dynamics
#'
#' @description
#' Generates a time series from the [system name], a [type of system]
#' that exhibits [key behavior].
#'
#' @details
#' ## Overview
#' The [system name] is defined by the iteration:
#' [equation]
#'
#' This system is important in [application domains] and demonstrates
#' [key properties].
#'
#' ## Parameter Effects
#' The parameter [param] controls [behavior]:
#' - [range1]: [behavior1]
#' - [range2]: [behavior2]
#' - [range3]: [behavior3]
#'
#' @section Mathematical Background:
#' [Mathematical details, derivations, properties]
#'
#' @param n Integer. Number of iterations. Must be positive.
#' @param [params] [Detailed parameter descriptions with valid ranges]
#' @param x0 Numeric. Initial condition. Must be in [valid range].
#'
#' @return
#' Numeric vector of length n containing the simulated trajectory.
#' Values are in [range]. The first value equals x0.
#'
#' @references
#' [Key papers on this system]
#'
#' @seealso
#' \code{\link{other_system}} for similar dynamics,
#' \code{\link{bifurcation_function}} to explore parameter space.
#'
#' @family simulation functions
#'
#' @examples
#' # [Examples as shown above]
#'
#' @export
```

### Template 2: Extreme Value Functions

```r
#' [Method Name] for Extreme Value Analysis
#'
#' @description
#' [What this method does - block maxima, POT, etc.]
#'
#' @details
#' ## Overview
#' [How the method works]
#'
#' ## When to Use
#' This method is appropriate when:
#' - [condition 1]
#' - [condition 2]
#'
#' ## Comparison to Alternatives
#' [Compare to other EVT methods]
#'
#' @section Mathematical Background:
#' [EVT theory relevant to this method]
#'
#' @param x Numeric vector. The time series to analyze. Should not
#'   contain NA or infinite values.
#' @param [method-specific params]
#'
#' @return
#' [Return value structure]
#'
#' @references
#' Coles, S. (2001). *An Introduction to Statistical Modeling of Extreme
#' Values*. Springer.
#'
#' @seealso
#' \code{\link{fit_gev}} or \code{\link{fit_gpd}} for fitting distributions,
#' \code{\link{alternative_method}} for alternative approach.
#'
#' @family extreme value functions
#'
#' @examples
#' # [Examples]
#'
#' @export
```

### Template 3: Estimation Functions

```r
#' Estimate [Parameter Name] Using [Method]
#'
#' @description
#' Estimates the [parameter] from time series data using the [method] method.
#'
#' @details
#' ## Overview
#' The [parameter] quantifies [what it measures]. This function implements
#' the [method] estimator proposed by [Author (Year)].
#'
#' ## Algorithm
#' The estimator is calculated as:
#' [brief algorithm description]
#'
#' ## Interpretation
#' - [Value range]: [interpretation]
#' - Typical values for [context]: [typical range]
#'
#' @section Mathematical Background:
#' [Theory and formulas]
#'
#' @param x Numeric vector. Time series data.
#' @param threshold Numeric. Exceedance threshold. Should be a high
#'   quantile (typically 0.90-0.99). Use \code{\link{threshold_diagnostics}}
#'   for guidance.
#' @param [other params]
#'
#' @return
#' [Return structure with interpretation guidance]
#'
#' @references
#' [Key references for this estimator]
#'
#' @seealso
#' \code{\link{alternative_estimator}} for different method,
#' \code{\link{bootstrap_function}} for confidence intervals,
#' \code{\link{diagnostic_function}} for validation.
#'
#' @family [category] functions
#'
#' @examples
#' # [Examples with interpretation]
#'
#' @export
```

### Template 4: Diagnostic/Utility Functions

```r
#' [Diagnostic/Utility Name]
#'
#' @description
#' [What diagnostic information this provides OR what utility task it performs]
#'
#' @details
#' ## Overview
#' [How it works and when to use it]
#'
#' ## Interpretation
#' [How to interpret the results]
#'
#' @param [params with clear descriptions]
#'
#' @return
#' [Return structure]
#'
#' @seealso
#' \code{\link{related_diagnostic}} for related diagnostic,
#' \code{\link{main_function}} for the main analysis function.
#'
#' @family diagnostic functions
#'
#' @examples
#' # [Examples]
#'
#' @export
```

---

## Writing Guidelines

### General Principles

1. **Audience**: Write for both beginners and experts
   - Beginners: Clear explanations, no assumed knowledge
   - Experts: Technical details, references, mathematical rigor

2. **Tone**: Professional but accessible
   - ✅ "The extremal index measures clustering of extreme events"
   - ❌ "θ is defined as the reciprocal of the mean cluster size under certain regularity conditions"

3. **Length**: Be comprehensive but concise
   - @description: 1-3 sentences
   - @details: 1-4 paragraphs
   - @param: 1-3 sentences per parameter
   - @examples: 3-5 examples of increasing complexity

4. **Consistency**:
   - Use consistent parameter names across related functions
   - Use consistent terminology (e.g., always "extremal index" not "θ" or "theta")
   - Follow the same structure for similar functions

### Technical Writing Tips

**Parameter Descriptions**:
- Start with type (Integer, Numeric, Character)
- Include constraints (positive, between 0 and 1, etc.)
- Provide typical or recommended values
- Explain the parameter's effect on results

**Examples**:
- Make self-contained (can be run independently)
- Progress from simple to complex
- Include comments explaining what each example demonstrates
- Show realistic use cases, not toy examples
- Include visualization when appropriate

**Mathematical Notation**:
- Use `\eqn{}` for inline math: \eqn{x^2}
- Use `\deqn{}` for display math: \deqn{x_{n+1} = f(x_n)}
- Define all symbols used
- Provide intuition before formulas

**Cross-References**:
- Link to related functions with `\code{\link{function_name}}`
- Reference vignettes with `vignette("vignette-name")`
- Create "See Also" networks so users can discover related functionality

---

## Priority Functions to Enhance

### 1. simulate_logistic_map()
**Why**: Entry point for most users, demonstrates package capabilities
**Focus**:
- Explain what chaos means
- Guide on parameter selection
- Visualizations in examples
- Link to bifurcation diagram function

### 2. block_maxima()
**Why**: Core EVT function, introduces fundamental concept
**Focus**:
- When to use vs POT
- How to choose block size
- Interpretation of results
- Link to fit_gev

### 3. extremal_index_runs()
**Why**: Main contribution of package, unique to chaotic systems
**Focus**:
- What extremal index means in plain English
- Interpretation of θ values
- How clustering relates to chaos
- Comparison to intervals method

### 4. fit_gev()
**Why**: Statistical modeling of extremes
**Focus**:
- GEV distribution overview
- Shape parameter interpretation
- Diagnostic plots
- When estimates are reliable

### 5. run_demo()
**Why**: Complete workflow demonstration
**Focus**:
- What analyses it runs
- How to interpret output
- Customization options
- When to use vs manual analysis

---

## Before/After Examples

### Example 1: simulate_logistic_map()

**BEFORE** (basic documentation):
```r
#' Simulate Logistic Map
#'
#' @param n number of iterations
#' @param r parameter r
#' @param x0 initial value
#'
#' @return numeric vector
#'
#' @export
simulate_logistic_map <- function(n, r, x0 = 0.2) {
  # ... implementation ...
}
```

**AFTER** (enhanced documentation):
```r
#' Simulate Logistic Map Dynamics
#'
#' @description
#' Generates a time series from the logistic map, a classic one-dimensional
#' chaotic dynamical system. The logistic map exhibits complex behavior
#' including fixed points, periodic orbits, and deterministic chaos depending
#' on the parameter r.
#'
#' @details
#' ## Overview
#' The logistic map is defined by the iteration:
#' \deqn{x_{n+1} = r \cdot x_n \cdot (1 - x_n)}
#'
#' Despite its simplicity, this map demonstrates the route to chaos through
#' period-doubling bifurcations and is fundamental to understanding chaotic
#' dynamics.
#'
#' ## Parameter Regions
#' The parameter r controls the system's behavior:
#' - **r < 1**: Extinction (x → 0)
#' - **1 < r < 3**: Convergence to fixed point
#' - **3 < r < 1 + √6 ≈ 3.45**: Oscillation between two values
#' - **3.45 < r < 3.57**: Period-doubling cascade
#' - **r > 3.57**: Chaotic regime (with periodic windows)
#' - **r = 4**: Fully chaotic (every orbit is dense)
#'
#' For studying extreme value theory, r = 3.8 or r = 4.0 are common choices
#' as they produce robust chaotic dynamics.
#'
#' @section Mathematical Background:
#' The logistic map was introduced by Robert May (1976) as a model for
#' population dynamics. The equation represents population growth with
#' reproduction (r·x) and competition/limiting resources (1-x).
#'
#' For r > 3.57, the system exhibits sensitive dependence on initial
#' conditions, a hallmark of chaos. The largest Lyapunov exponent is
#' positive in the chaotic regime, confirming exponential divergence of
#' nearby trajectories.
#'
#' @param n Integer. Number of iterations to simulate. Must be positive.
#'   Typical values range from 500 (quick exploration) to 10000 (statistical
#'   analysis). Larger n provides better estimates of long-term statistics
#'   but takes longer to compute.
#'
#' @param r Numeric. The logistic map parameter. Valid range is [0, 4],
#'   though values outside [2.5, 4] are rarely used. Recommended values:
#'   - r = 3.8 for robust chaotic dynamics
#'   - r = 4.0 for fully chaotic behavior with known invariant density
#'   - r = 3.2 for periodic behavior (educational comparison)
#'
#' @param x0 Numeric. Initial condition. Must be in the open interval (0, 1).
#'   The dynamics are typically not sensitive to x0 for chaotic parameter
#'   values (after a short transient), but periodic regimes may have multiple
#'   attractors. Default is 0.2.
#'
#' @return
#' Numeric vector of length n containing the simulated time series.
#' All values are in (0, 1) due to the map's dynamics. The first element
#' equals x0. For r = 4, the invariant density is β(0.5, 0.5), resulting
#' in a characteristic U-shaped histogram.
#'
#' @references
#' May, R. M. (1976). Simple mathematical models with very complicated
#' dynamics. *Nature*, 261(5560), 459-467. \doi{10.1038/261459a0}
#'
#' Strogatz, S. H. (2015). *Nonlinear Dynamics and Chaos: With Applications
#' to Physics, Biology, Chemistry, and Engineering* (2nd ed.). Westview Press.
#'
#' @seealso
#' \code{\link{logistic_bifurcation}} to explore the bifurcation diagram across
#' parameter values, \code{\link{simulate_henon_map}} for a 2D chaotic system,
#' \code{\link{estimate_lyapunov_exponent}} to quantify chaotic behavior.
#'
#' For using this in extreme value analysis, see
#' \code{vignette("estimating-theta-logistic")}.
#'
#' @family simulation functions
#'
#' @examples
#' # Basic usage: Generate 500 iterations in chaotic regime
#' series <- simulate_logistic_map(n = 500, r = 3.8, x0 = 0.2)
#' head(series)
#'
#' # Visualize the chaotic time series
#' plot(series, type = "l", col = "steelblue",
#'      main = "Logistic Map Time Series (r = 3.8)",
#'      xlab = "Iteration", ylab = "x")
#'
#' # Compare different dynamical regimes
#' par(mfrow = c(2, 2))
#' plot(simulate_logistic_map(200, r = 2.5), type = "l",
#'      main = "r = 2.5 (Fixed Point)", ylab = "x", col = "darkgreen")
#' plot(simulate_logistic_map(200, r = 3.2), type = "l",
#'      main = "r = 3.2 (Period 2)", ylab = "x", col = "orange")
#' plot(simulate_logistic_map(200, r = 3.8), type = "l",
#'      main = "r = 3.8 (Chaos)", ylab = "x", col = "red")
#' plot(simulate_logistic_map(200, r = 4.0), type = "l",
#'      main = "r = 4.0 (Full Chaos)", ylab = "x", col = "purple")
#' par(mfrow = c(1, 1))
#'
#' # Examine the invariant distribution for r = 4
#' long_series <- simulate_logistic_map(n = 10000, r = 4.0, x0 = 0.2)
#' hist(long_series, breaks = 50, probability = TRUE,
#'      main = "Invariant Density (r = 4.0)",
#'      xlab = "x", col = "lightblue", border = "white")
#' # Note the U-shape characteristic of beta(0.5, 0.5)
#'
#' # Sensitivity to initial conditions (chaos demonstration)
#' x1 <- simulate_logistic_map(100, r = 4.0, x0 = 0.2)
#' x2 <- simulate_logistic_map(100, r = 4.0, x0 = 0.200001)  # tiny difference
#' plot(abs(x1 - x2), type = "l", log = "y",
#'      main = "Sensitive Dependence on Initial Conditions",
#'      xlab = "Iteration", ylab = "|x1 - x2| (log scale)")
#' # Trajectories diverge exponentially despite nearly identical start
#'
#' \donttest{
#' # Generate very long series for statistical analysis
#' # This takes ~5-10 seconds
#' statistical_sample <- simulate_logistic_map(n = 1000000, r = 3.8)
#'
#' # Estimate extremal index
#' threshold <- quantile(statistical_sample, 0.95)
#' theta <- extremal_index_runs(statistical_sample, threshold, run_length = 2)
#' print(theta)
#' }
#'
#' @export
```

**Key Improvements**:
1. Comprehensive description of what the function does
2. Detailed parameter regions with interpretation
3. Mathematical background section
4. Enhanced parameter documentation with guidance
5. Clear return value description
6. Academic references
7. Rich cross-references to related functions
8. Multiple examples showing different use cases
9. Interpretation of results

---

### Example 2: block_maxima()

**BEFORE**:
```r
#' Extract Block Maxima
#'
#' @param x numeric vector
#' @param block_size size of blocks
#'
#' @return vector of maxima
#'
#' @export
```

**AFTER**:
```r
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
#'   cat("Block size", bs, ": ", length(bm_temp), "maxima\\n")
#' }
#'
#' \donttest{
#' # Fit GEV distribution to block maxima
#' if (requireNamespace("evd", quietly = TRUE)) {
#'   gev_fit <- fit_gev(bm)
#'   print(gev_fit)
#'
#'   # Diagnostic plots
#'   par(mfrow = c(2, 2))
#'   # Add GEV diagnostic plots here
#'   par(mfrow = c(1, 1))
#' }
#' }
#'
#' @export
```

**Key Improvements**:
1. Explains EVT context and theory
2. Compares to POT method (helps users choose)
3. Detailed guidance on block size selection
4. Mathematical background section
5. Practical interpretation of results
6. Rich examples showing usage and diagnostics
7. Clear return value description
8. Cross-references to related functions and vignettes

---

## Implementation Checklist

For each of the 5 priority functions:

- [ ] Review current documentation
- [ ] Add comprehensive @description (1-3 sentences)
- [ ] Expand @details with multiple subsections
- [ ] Add @section Mathematical Background (if applicable)
- [ ] Enhance all @param descriptions (type, constraints, guidance)
- [ ] Expand @return with structure and interpretation
- [ ] Add @references with DOIs
- [ ] Add @seealso with at least 3 links
- [ ] Add @family tag
- [ ] Expand @examples:
  - [ ] Basic usage
  - [ ] Visualization
  - [ ] Comparison/sensitivity analysis
  - [ ] Integration with other functions
  - [ ] Long-running in \donttest{}
- [ ] Run `devtools::document()` to regenerate
- [ ] Test all examples with `devtools::run_examples()`
- [ ] Review rendered documentation with `?function_name`

---

## Testing Enhanced Documentation

After enhancing documentation:

```r
# 1. Regenerate documentation
devtools::document()

# 2. Check that all examples run
devtools::run_examples()

# 3. Review help pages
?simulate_logistic_map
?block_maxima
?extremal_index_runs
?fit_gev
?run_demo

# 4. Build PDF manual to check formatting
devtools::build_manual()

# 5. Check for documentation warnings
devtools::check(document = FALSE, args = "--no-tests")

# 6. Build pkgdown site to see final result
pkgdown::build_site()
# Open docs/index.html to review
```

---

## Summary

This guide provides templates and examples for transforming basic function documentation into comprehensive, user-friendly documentation that:

1. **Helps beginners** understand what functions do and how to use them
2. **Provides depth** for advanced users with mathematical background and references
3. **Guides decisions** on parameter selection and method choice
4. **Demonstrates usage** with realistic, interpretable examples
5. **Connects concepts** through cross-references and families
6. **Maintains quality** through consistent structure and style

Following these templates will make `chaoticds` accessible to a wide audience while maintaining scientific rigor.
