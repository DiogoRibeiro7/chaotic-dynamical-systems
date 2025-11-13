#include <Rcpp.h>
#include <algorithm>
#include <cmath>
using namespace Rcpp;

//' Fast Hénon map simulation (C++ implementation)
//'
//' Efficient C++ implementation of the Hénon map for improved performance.
//'
//' @param n Number of iterations
//' @param a Parameter a
//' @param b Parameter b
//' @param x0 Initial x value
//' @param y0 Initial y value
//' @return DataFrame with x and y columns
//' @export
// [[Rcpp::export]]
DataFrame simulate_henon_map_cpp(int n, double a = 1.4, double b = 0.3,
                                  double x0 = 0.0, double y0 = 0.0) {
  NumericVector x(n);
  NumericVector y(n);

  x[0] = x0;
  y[0] = y0;

  for (int i = 1; i < n; i++) {
    x[i] = 1.0 - a * x[i-1] * x[i-1] + y[i-1];
    y[i] = b * x[i-1];
  }

  return DataFrame::create(
    Named("x") = x,
    Named("y") = y
  );
}

//' Fast tent map simulation (C++ implementation)
//'
//' Efficient C++ implementation of the tent map.
//'
//' @param n Number of iterations
//' @param r Parameter r
//' @param x0 Initial value
//' @return Numeric vector
//' @export
// [[Rcpp::export]]
NumericVector simulate_tent_map_cpp(int n, double r, double x0) {
  NumericVector x(n);
  x[0] = x0;

  for (int i = 1; i < n; i++) {
    if (x[i-1] < 0.5) {
      x[i] = r * x[i-1];
    } else {
      x[i] = r * (1.0 - x[i-1]);
    }
  }

  return x;
}

//' Fast ACF computation (C++ implementation)
//'
//' Efficient computation of autocorrelation function.
//'
//' @param x Numeric vector
//' @param max_lag Maximum lag
//' @return NumericVector of ACF values
//' @export
// [[Rcpp::export]]
NumericVector acf_cpp(NumericVector x, int max_lag) {
  int n = x.size();
  NumericVector acf_vals(max_lag + 1);

  // Compute mean
  double mean = 0.0;
  for (int i = 0; i < n; i++) {
    mean += x[i];
  }
  mean /= n;

  // Compute variance (lag 0)
  double var = 0.0;
  for (int i = 0; i < n; i++) {
    double diff = x[i] - mean;
    var += diff * diff;
  }

  // Compute ACF for each lag
  for (int lag = 0; lag <= max_lag; lag++) {
    double cov = 0.0;
    for (int i = 0; i < n - lag; i++) {
      cov += (x[i] - mean) * (x[i + lag] - mean);
    }
    acf_vals[lag] = cov / var;
  }

  return acf_vals;
}

//' Fast exceedances extraction (C++ implementation)
//'
//' Extract values that exceed threshold.
//'
//' @param x Numeric vector
//' @param threshold Threshold value
//' @return NumericVector of exceedance values
//' @export
// [[Rcpp::export]]
NumericVector exceedances_cpp(NumericVector x, double threshold) {
  std::vector<double> exc;

  for (int i = 0; i < x.size(); i++) {
    if (x[i] > threshold) {
      exc.push_back(x[i] - threshold);
    }
  }

  return wrap(exc);
}

//' Fast inter-exceedance times (C++ implementation)
//'
//' Compute times between consecutive exceedances.
//'
//' @param exceedance_indices Integer vector of exceedance indices
//' @return IntegerVector of inter-exceedance times
//' @export
// [[Rcpp::export]]
IntegerVector inter_exceedance_times_cpp(IntegerVector exceedance_indices) {
  int n = exceedance_indices.size();
  if (n <= 1) {
    return IntegerVector(0);
  }

  IntegerVector times(n - 1);
  for (int i = 1; i < n; i++) {
    times[i-1] = exceedance_indices[i] - exceedance_indices[i-1];
  }

  return times;
}

//' Fast empirical CDF (C++ implementation)
//'
//' Compute empirical cumulative distribution function.
//'
//' @param x Numeric vector (data)
//' @param eval_points Numeric vector (points to evaluate CDF)
//' @return NumericVector of CDF values
//' @export
// [[Rcpp::export]]
NumericVector ecdf_cpp(NumericVector x, NumericVector eval_points) {
  int n = x.size();
  int m = eval_points.size();
  NumericVector cdf(m);

  // Sort x for efficient searching
  NumericVector sorted_x = clone(x);
  std::sort(sorted_x.begin(), sorted_x.end());

  for (int i = 0; i < m; i++) {
    // Count how many values are <= eval_points[i]
    int count = 0;
    for (int j = 0; j < n; j++) {
      if (sorted_x[j] <= eval_points[i]) {
        count++;
      } else {
        break;  // Since sorted, can stop here
      }
    }
    cdf[i] = (double)count / n;
  }

  return cdf;
}

//' Fast return level estimation (C++ implementation)
//'
//' Estimate return levels from block maxima.
//'
//' @param block_maxima Numeric vector of block maxima
//' @param return_period Integer return period
//' @return Numeric scalar return level estimate
//' @export
// [[Rcpp::export]]
double return_level_empirical_cpp(NumericVector block_maxima, int return_period) {
  NumericVector sorted_bm = clone(block_maxima);
  std::sort(sorted_bm.begin(), sorted_bm.end());

  int n = sorted_bm.size();
  double prob = 1.0 - 1.0 / return_period;
  int index = (int)floor(prob * (n - 1));

  if (index >= n) index = n - 1;
  if (index < 0) index = 0;

  return sorted_bm[index];
}

//' Fast bootstrap sample generation (C++ implementation)
//'
//' Generate bootstrap samples efficiently.
//'
//' @param x Numeric vector to resample
//' @param B Number of bootstrap samples
//' @return NumericMatrix with B columns, each a bootstrap sample
//' @export
// [[Rcpp::export]]
NumericMatrix bootstrap_samples_cpp(NumericVector x, int B) {
  int n = x.size();
  NumericMatrix samples(n, B);

  // Use R's random number generator
  for (int b = 0; b < B; b++) {
    // Sample with replacement
    IntegerVector indices = sample(n, n, true) - 1;  // -1 for 0-based indexing
    for (int i = 0; i < n; i++) {
      samples(i, b) = x[indices[i]];
    }
  }

  return samples;
}

//' Fast moving average (C++ implementation)
//'
//' Compute moving average for time series.
//'
//' @param x Numeric vector
//' @param window_size Integer window size
//' @return NumericVector of moving averages
//' @export
// [[Rcpp::export]]
NumericVector moving_average_cpp(NumericVector x, int window_size) {
  int n = x.size();
  if (window_size > n) {
    stop("Window size cannot be larger than data size");
  }

  NumericVector ma(n - window_size + 1);

  // Compute first window
  double sum = 0.0;
  for (int i = 0; i < window_size; i++) {
    sum += x[i];
  }
  ma[0] = sum / window_size;

  // Slide window
  for (int i = window_size; i < n; i++) {
    sum = sum - x[i - window_size] + x[i];
    ma[i - window_size + 1] = sum / window_size;
  }

  return ma;
}

//' Fast threshold stability diagnostic (C++ implementation)
//'
//' Compute mean excess over threshold for threshold selection.
//'
//' @param x Numeric vector
//' @param thresholds Numeric vector of candidate thresholds
//' @return NumericVector of mean excess values
//' @export
// [[Rcpp::export]]
NumericVector mean_excess_cpp(NumericVector x, NumericVector thresholds) {
  int n_thresh = thresholds.size();
  NumericVector mean_excess(n_thresh);

  for (int t = 0; t < n_thresh; t++) {
    double threshold = thresholds[t];
    std::vector<double> excesses;

    for (int i = 0; i < x.size(); i++) {
      if (x[i] > threshold) {
        excesses.push_back(x[i] - threshold);
      }
    }

    if (excesses.size() > 0) {
      double sum = 0.0;
      for (size_t i = 0; i < excesses.size(); i++) {
        sum += excesses[i];
      }
      mean_excess[t] = sum / excesses.size();
    } else {
      mean_excess[t] = NA_REAL;
    }
  }

  return mean_excess;
}

//' Fast extremal index intervals estimator (C++ implementation)
//'
//' Ferro-Segers intervals estimator for extremal index.
//'
//' @param x Numeric vector
//' @param threshold Numeric threshold
//' @return Numeric extremal index estimate
//' @export
// [[Rcpp::export]]
double extremal_index_intervals_cpp(NumericVector x, double threshold) {
  // Find exceedances
  std::vector<int> exc_indices;
  for (int i = 0; i < x.size(); i++) {
    if (x[i] > threshold) {
      exc_indices.push_back(i);
    }
  }

  int N = exc_indices.size();
  if (N < 2) {
    return NA_REAL;
  }

  // Compute inter-exceedance times
  std::vector<int> S(N - 1);
  for (int i = 1; i < N; i++) {
    S[i-1] = exc_indices[i] - exc_indices[i-1];
  }

  // Ferro-Segers estimator
  double sum_S = 0.0;
  double sum_S_minus_1 = 0.0;

  for (int i = 0; i < N - 1; i++) {
    sum_S += S[i];
    if (S[i] > 1) {
      sum_S_minus_1 += S[i] - 1;
    }
  }

  if (sum_S_minus_1 == 0.0) {
    return 1.0;  // No clustering
  }

  double theta = 2.0 * sum_S * sum_S / ((N - 1) * sum_S_minus_1 + sum_S * sum_S);

  // Bound between 0 and 1
  if (theta > 1.0) theta = 1.0;
  if (theta < 0.0) theta = 0.0;

  return theta;
}

//' Fast logistic bifurcation diagram data (C++ implementation)
//'
//' Generate bifurcation diagram data efficiently.
//'
//' @param r_values Numeric vector of r parameters
//' @param n_iter Number of iterations per r value
//' @param discard Number of initial iterations to discard
//' @param x0 Initial value
//' @return DataFrame with r and x columns
//' @export
// [[Rcpp::export]]
DataFrame logistic_bifurcation_cpp(NumericVector r_values,
                                    int n_iter = 200,
                                    int discard = 100,
                                    double x0 = 0.2) {
  int n_r = r_values.size();
  int keep = n_iter - discard;
  int total_points = n_r * keep;

  NumericVector r_out(total_points);
  NumericVector x_out(total_points);

  int idx = 0;
  for (int r_idx = 0; r_idx < n_r; r_idx++) {
    double r = r_values[r_idx];
    double x = x0;

    // Transient phase
    for (int i = 0; i < discard; i++) {
      x = r * x * (1.0 - x);
    }

    // Keep phase
    for (int i = 0; i < keep; i++) {
      x = r * x * (1.0 - x);
      r_out[idx] = r;
      x_out[idx] = x;
      idx++;
    }
  }

  return DataFrame::create(
    Named("r") = r_out,
    Named("x") = x_out
  );
}
