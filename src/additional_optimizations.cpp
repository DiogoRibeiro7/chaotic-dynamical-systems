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
//' @param noise_sd Additive Gaussian noise SD per iteration (default 0)
//' @return DataFrame with x and y columns
//' @export
// [[Rcpp::export]]
DataFrame simulate_henon_map_cpp(int n, double a = 1.4, double b = 0.3,
                                  double x0 = 0.0, double y0 = 0.0,
                                  double noise_sd = 0.0) {
  NumericVector nx = (noise_sd > 0.0)
    ? Rcpp::rnorm(n - 1, 0.0, noise_sd) : NumericVector(n - 1);
  NumericVector ny = (noise_sd > 0.0)
    ? Rcpp::rnorm(n - 1, 0.0, noise_sd) : NumericVector(n - 1);
  NumericVector x(n);
  NumericVector y(n);
  x[0] = x0;
  y[0] = y0;
  for (int i = 1; i < n; i++) {
    x[i] = 1.0 - a * x[i-1] * x[i-1] + y[i-1] + nx[i-1];
    y[i] =                    b * x[i-1]      + ny[i-1];
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
//' @param noise_sd Additive Gaussian noise SD per iteration (default 0)
//' @return Numeric vector
//' @export
// [[Rcpp::export]]
NumericVector simulate_tent_map_cpp(int n, double r, double x0,
                                     double noise_sd = 0.0) {
  NumericVector noise = (noise_sd > 0.0)
    ? Rcpp::rnorm(n - 1, 0.0, noise_sd) : NumericVector(n - 1);
  NumericVector x(n);
  x[0] = x0;
  for (int i = 1; i < n; i++) {
    double base = (x[i-1] < 0.5) ? r * x[i-1] : r * (1.0 - x[i-1]);
    x[i] = base + noise[i-1];
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

//' Fast Lozi map simulation (C++ implementation)
//'
//' Efficient C++ implementation of the two-dimensional Lozi map.
//'
//' @param n Number of iterations
//' @param a Parameter a
//' @param b Parameter b
//' @param x0 Initial x value
//' @param y0 Initial y value
//' @return DataFrame with x and y columns
//' @export
// [[Rcpp::export]]
DataFrame simulate_lozi_map_cpp(int n, double a = 1.7, double b = 0.5,
                                 double x0 = 0.0, double y0 = 0.0,
                                 double noise_sd = 0.0) {
  NumericVector nx = (noise_sd > 0.0)
    ? Rcpp::rnorm(n - 1, 0.0, noise_sd) : NumericVector(n - 1);
  NumericVector ny = (noise_sd > 0.0)
    ? Rcpp::rnorm(n - 1, 0.0, noise_sd) : NumericVector(n - 1);
  NumericVector x(n);
  NumericVector y(n);
  x[0] = x0;
  y[0] = y0;
  for (int i = 1; i < n; i++) {
    x[i] = 1.0 - a * std::fabs(x[i-1]) + b * y[i-1] + nx[i-1];
    y[i] = x[i-1]                                    + ny[i-1];
  }
  return DataFrame::create(
    Named("x") = x,
    Named("y") = y
  );
}

//' Fast Chirikov standard map simulation (C++ implementation)
//'
//' Efficient C++ implementation of the area-preserving Chirikov-Taylor
//' map on the torus [0, 2*pi)^2.
//'
//' @param n Number of iterations
//' @param K Kick parameter (default 1.2)
//' @param p0 Initial momentum
//' @param theta0 Initial angle
//' @return DataFrame with columns p and theta
//' @export
// [[Rcpp::export]]
DataFrame simulate_standard_map_cpp(int n, double K = 1.2,
                                     double p0 = 1.0, double theta0 = 1.0,
                                     double noise_sd = 0.0) {
  const double two_pi = 2.0 * M_PI;
  auto wrap = [&](double v) {
    double w = std::fmod(v, two_pi);
    if (w < 0.0) w += two_pi;
    return w;
  };

  NumericVector np = (noise_sd > 0.0)
    ? Rcpp::rnorm(n - 1, 0.0, noise_sd) : NumericVector(n - 1);
  NumericVector nt = (noise_sd > 0.0)
    ? Rcpp::rnorm(n - 1, 0.0, noise_sd) : NumericVector(n - 1);
  NumericVector p(n);
  NumericVector theta(n);
  p[0]     = wrap(p0);
  theta[0] = wrap(theta0);

  for (int i = 1; i < n; i++) {
    double p_new     = wrap(p[i-1] + K * std::sin(theta[i-1]) + np[i-1]);
    double theta_new = wrap(theta[i-1] + p_new                 + nt[i-1]);
    p[i]     = p_new;
    theta[i] = theta_new;
  }

  return DataFrame::create(
    Named("p")     = p,
    Named("theta") = theta
  );
}

//' Fast Ikeda map simulation (C++ implementation)
//'
//' Efficient C++ implementation of the two-dimensional Ikeda map.
//'
//' @param n Number of iterations
//' @param u Dissipation parameter (default 0.9)
//' @param x0 Initial x
//' @param y0 Initial y
//' @return DataFrame with columns x and y
//' @export
// [[Rcpp::export]]
DataFrame simulate_ikeda_map_cpp(int n, double u = 0.9,
                                  double x0 = 0.0, double y0 = 0.0,
                                  double noise_sd = 0.0) {
  NumericVector nx = (noise_sd > 0.0)
    ? Rcpp::rnorm(n - 1, 0.0, noise_sd) : NumericVector(n - 1);
  NumericVector ny = (noise_sd > 0.0)
    ? Rcpp::rnorm(n - 1, 0.0, noise_sd) : NumericVector(n - 1);
  NumericVector x(n);
  NumericVector y(n);
  x[0] = x0;
  y[0] = y0;

  for (int i = 1; i < n; i++) {
    double t_n = 0.4 - 6.0 / (1.0 + x[i-1] * x[i-1] + y[i-1] * y[i-1]);
    double ct  = std::cos(t_n);
    double st  = std::sin(t_n);
    x[i] = 1.0 + u * (x[i-1] * ct - y[i-1] * st) + nx[i-1];
    y[i] =       u * (x[i-1] * st + y[i-1] * ct) + ny[i-1];
  }

  return DataFrame::create(
    Named("x") = x,
    Named("y") = y
  );
}

//' Fast Arnold cat map simulation (C++ implementation)
//'
//' Efficient C++ implementation of the Arnold cat map on the unit torus.
//'
//' @param n Number of iterations
//' @param x0 Initial x value
//' @param y0 Initial y value
//' @return DataFrame with x and y columns
//' @export
// [[Rcpp::export]]
DataFrame simulate_cat_map_cpp(int n, double x0 = 0.1, double y0 = 0.1,
                                double noise_sd = 0.0) {
  NumericVector nx = (noise_sd > 0.0)
    ? Rcpp::rnorm(n - 1, 0.0, noise_sd) : NumericVector(n - 1);
  NumericVector ny = (noise_sd > 0.0)
    ? Rcpp::rnorm(n - 1, 0.0, noise_sd) : NumericVector(n - 1);
  NumericVector x(n);
  NumericVector y(n);

  auto wrap_unit = [](double v) {
    double w = std::fmod(v, 1.0);
    if (w < 0.0) w += 1.0;
    return w;
  };

  x[0] = wrap_unit(x0);
  y[0] = wrap_unit(y0);

  for (int i = 1; i < n; i++) {
    double x_new = wrap_unit(x[i-1] +       y[i-1] + nx[i-1]);
    double y_new = wrap_unit(x[i-1] + 2.0 * y[i-1] + ny[i-1]);
    x[i] = x_new;
    y[i] = y_new;
  }

  return DataFrame::create(
    Named("x") = x,
    Named("y") = y
  );
}

// ---------------------------------------------------------------------------
// Continuous-time chaotic systems
//
// Each simulator below uses a fixed-step classical Runge-Kutta (RK4) scheme
// with the same arithmetic ordering as R/simulate-continuous.R, so the
// outputs agree bit-for-bit modulo IEEE rounding propagation (parity tests
// use tolerance 1e-8). The integrator is inlined into each function to match
// the flat style of the existing _cpp simulators.
// ---------------------------------------------------------------------------

//' Fast Lorenz system simulation (C++ implementation)
//'
//' RK4 integration of the classical Lorenz system, mirroring [simulate_lorenz()].
//'
//' @param t_max Total integration time after any transient
//' @param dt Integration step size
//' @param x0,y0,z0 Initial conditions
//' @param sigma,rho,beta Lorenz parameters
//' @param transient Integration time discarded from the start of the trajectory
//' @return DataFrame with columns t, x, y, z
//' @export
// [[Rcpp::export]]
DataFrame simulate_lorenz_cpp(double t_max = 50.0, double dt = 0.01,
                              double x0 = 1.0, double y0 = 1.0, double z0 = 1.05,
                              double sigma = 10.0, double rho = 28.0,
                              double beta = 8.0 / 3.0,
                              double transient = 0.0) {
  if (t_max <= 0.0) stop("t_max must be strictly positive");
  if (dt <= 0.0)    stop("dt must be strictly positive");
  if (transient < 0.0) stop("transient must be non-negative");

  double total = transient + t_max;
  int n_steps = (int)std::round(total / dt);
  if (n_steps < 1) stop("dt is too large relative to t_max");

  int n_total = n_steps + 1;
  std::vector<double> tx(n_total), xx(n_total), yy(n_total), zz(n_total);
  xx[0] = x0; yy[0] = y0; zz[0] = z0;
  tx[0] = 0.0;

  for (int i = 0; i < n_steps; i++) {
    double xi = xx[i], yi = yy[i], zi = zz[i];

    double k1x = sigma * (yi - xi);
    double k1y = xi * (rho - zi) - yi;
    double k1z = xi * yi - beta * zi;

    double x2 = xi + dt / 2.0 * k1x;
    double y2 = yi + dt / 2.0 * k1y;
    double z2 = zi + dt / 2.0 * k1z;
    double k2x = sigma * (y2 - x2);
    double k2y = x2 * (rho - z2) - y2;
    double k2z = x2 * y2 - beta * z2;

    double x3 = xi + dt / 2.0 * k2x;
    double y3 = yi + dt / 2.0 * k2y;
    double z3 = zi + dt / 2.0 * k2z;
    double k3x = sigma * (y3 - x3);
    double k3y = x3 * (rho - z3) - y3;
    double k3z = x3 * y3 - beta * z3;

    double x4 = xi + dt * k3x;
    double y4 = yi + dt * k3y;
    double z4 = zi + dt * k3z;
    double k4x = sigma * (y4 - x4);
    double k4y = x4 * (rho - z4) - y4;
    double k4z = x4 * y4 - beta * z4;

    xx[i+1] = xi + dt / 6.0 * (k1x + 2.0 * k2x + 2.0 * k3x + k4x);
    yy[i+1] = yi + dt / 6.0 * (k1y + 2.0 * k2y + 2.0 * k3y + k4y);
    zz[i+1] = zi + dt / 6.0 * (k1z + 2.0 * k2z + 2.0 * k3z + k4z);
    tx[i+1] = tx[i] + dt;
  }

  // Drop the transient and re-zero the time column to match the R reference.
  std::vector<double> t_out, x_out, y_out, z_out;
  t_out.reserve(n_total);
  for (int i = 0; i < n_total; i++) {
    if (tx[i] >= transient) {
      t_out.push_back(tx[i] - transient);
      x_out.push_back(xx[i]);
      y_out.push_back(yy[i]);
      z_out.push_back(zz[i]);
    }
  }

  return DataFrame::create(
    Named("t") = wrap(t_out),
    Named("x") = wrap(x_out),
    Named("y") = wrap(y_out),
    Named("z") = wrap(z_out)
  );
}

//' Fast Rossler system simulation (C++ implementation)
//'
//' RK4 integration of the Rossler system, mirroring [simulate_rossler()].
//'
//' @param t_max Total integration time after any transient
//' @param dt Integration step size
//' @param x0,y0,z0 Initial conditions
//' @param a,b,c Rossler parameters
//' @param transient Integration time discarded from the start of the trajectory
//' @return DataFrame with columns t, x, y, z
//' @export
// [[Rcpp::export]]
DataFrame simulate_rossler_cpp(double t_max = 200.0, double dt = 0.05,
                               double x0 = 0.0, double y0 = 1.0, double z0 = 0.0,
                               double a = 0.2, double b = 0.2, double c = 5.7,
                               double transient = 0.0) {
  if (t_max <= 0.0) stop("t_max must be strictly positive");
  if (dt <= 0.0)    stop("dt must be strictly positive");
  if (transient < 0.0) stop("transient must be non-negative");

  double total = transient + t_max;
  int n_steps = (int)std::round(total / dt);
  if (n_steps < 1) stop("dt is too large relative to t_max");

  int n_total = n_steps + 1;
  std::vector<double> tx(n_total), xx(n_total), yy(n_total), zz(n_total);
  xx[0] = x0; yy[0] = y0; zz[0] = z0;
  tx[0] = 0.0;

  for (int i = 0; i < n_steps; i++) {
    double xi = xx[i], yi = yy[i], zi = zz[i];

    double k1x = -(yi + zi);
    double k1y = xi + a * yi;
    double k1z = b + zi * (xi - c);

    double x2 = xi + dt / 2.0 * k1x;
    double y2 = yi + dt / 2.0 * k1y;
    double z2 = zi + dt / 2.0 * k1z;
    double k2x = -(y2 + z2);
    double k2y = x2 + a * y2;
    double k2z = b + z2 * (x2 - c);

    double x3 = xi + dt / 2.0 * k2x;
    double y3 = yi + dt / 2.0 * k2y;
    double z3 = zi + dt / 2.0 * k2z;
    double k3x = -(y3 + z3);
    double k3y = x3 + a * y3;
    double k3z = b + z3 * (x3 - c);

    double x4 = xi + dt * k3x;
    double y4 = yi + dt * k3y;
    double z4 = zi + dt * k3z;
    double k4x = -(y4 + z4);
    double k4y = x4 + a * y4;
    double k4z = b + z4 * (x4 - c);

    xx[i+1] = xi + dt / 6.0 * (k1x + 2.0 * k2x + 2.0 * k3x + k4x);
    yy[i+1] = yi + dt / 6.0 * (k1y + 2.0 * k2y + 2.0 * k3y + k4y);
    zz[i+1] = zi + dt / 6.0 * (k1z + 2.0 * k2z + 2.0 * k3z + k4z);
    tx[i+1] = tx[i] + dt;
  }

  std::vector<double> t_out, x_out, y_out, z_out;
  t_out.reserve(n_total);
  for (int i = 0; i < n_total; i++) {
    if (tx[i] >= transient) {
      t_out.push_back(tx[i] - transient);
      x_out.push_back(xx[i]);
      y_out.push_back(yy[i]);
      z_out.push_back(zz[i]);
    }
  }

  return DataFrame::create(
    Named("t") = wrap(t_out),
    Named("x") = wrap(x_out),
    Named("y") = wrap(y_out),
    Named("z") = wrap(z_out)
  );
}

//' Fast forced Duffing oscillator simulation (C++ implementation)
//'
//' RK4 integration of the forced Duffing oscillator, mirroring
//' [simulate_duffing()]. Unlike Lorenz and Rossler the forcing introduces
//' explicit time dependence, so the integrator threads the internal clock
//' through the derivative evaluation.
//'
//' @param t_max Total integration time after any transient
//' @param dt Integration step size
//' @param x0,v0 Initial position and velocity
//' @param alpha,beta,delta,gamma,omega Duffing parameters
//' @param transient Integration time discarded from the start of the trajectory
//' @return DataFrame with columns t, x, v
//' @export
// [[Rcpp::export]]
DataFrame simulate_duffing_cpp(double t_max = 100.0, double dt = 0.05,
                               double x0 = 1.0, double v0 = 0.0,
                               double alpha = -1.0, double beta = 1.0,
                               double delta = 0.2, double gamma = 0.3,
                               double omega = 1.0, double transient = 0.0) {
  if (t_max <= 0.0) stop("t_max must be strictly positive");
  if (dt <= 0.0)    stop("dt must be strictly positive");
  if (transient < 0.0) stop("transient must be non-negative");

  double total = transient + t_max;
  int n_steps = (int)std::round(total / dt);
  if (n_steps < 1) stop("dt is too large relative to t_max");

  int n_total = n_steps + 1;
  std::vector<double> tx(n_total), xx(n_total), vv(n_total);
  xx[0] = x0; vv[0] = v0;
  tx[0] = 0.0;

  auto force = [&](double t) { return gamma * std::cos(omega * t); };

  for (int i = 0; i < n_steps; i++) {
    double ti = tx[i];
    double xi = xx[i], vi = vv[i];

    double k1x = vi;
    double k1v = -delta * vi - alpha * xi - beta * xi * xi * xi + force(ti);

    double xm = xi + dt / 2.0 * k1x;
    double vm = vi + dt / 2.0 * k1v;
    double tm = ti + dt / 2.0;
    double k2x = vm;
    double k2v = -delta * vm - alpha * xm - beta * xm * xm * xm + force(tm);

    xm = xi + dt / 2.0 * k2x;
    vm = vi + dt / 2.0 * k2v;
    double k3x = vm;
    double k3v = -delta * vm - alpha * xm - beta * xm * xm * xm + force(tm);

    double xe = xi + dt * k3x;
    double ve = vi + dt * k3v;
    double te = ti + dt;
    double k4x = ve;
    double k4v = -delta * ve - alpha * xe - beta * xe * xe * xe + force(te);

    xx[i+1] = xi + dt / 6.0 * (k1x + 2.0 * k2x + 2.0 * k3x + k4x);
    vv[i+1] = vi + dt / 6.0 * (k1v + 2.0 * k2v + 2.0 * k3v + k4v);
    tx[i+1] = te;
  }

  std::vector<double> t_out, x_out, v_out;
  t_out.reserve(n_total);
  for (int i = 0; i < n_total; i++) {
    if (tx[i] >= transient) {
      t_out.push_back(tx[i] - transient);
      x_out.push_back(xx[i]);
      v_out.push_back(vv[i]);
    }
  }

  return DataFrame::create(
    Named("t") = wrap(t_out),
    Named("x") = wrap(x_out),
    Named("v") = wrap(v_out)
  );
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
