#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

namespace {

std::vector<double> kuramoto_derivative(
    const std::vector<double>& theta,
    const NumericVector& omega,
    double coupling) {
  const int n = theta.size();
  double mean_cos = 0.0;
  double mean_sin = 0.0;

  for (int i = 0; i < n; ++i) {
    mean_cos += std::cos(theta[i]);
    mean_sin += std::sin(theta[i]);
  }
  mean_cos /= static_cast<double>(n);
  mean_sin /= static_cast<double>(n);

  std::vector<double> out(n);
  for (int i = 0; i < n; ++i) {
    out[i] =
      omega[i] +
      coupling * (
        mean_sin * std::cos(theta[i]) -
        mean_cos * std::sin(theta[i])
      );
  }
  return out;
}

}  // namespace

// [[Rcpp::export]]
NumericMatrix kuramoto_simulate_cpp_impl(
    int n_keep,
    int n_transient,
    double dt,
    double coupling,
    NumericVector omega,
    NumericVector theta0) {
  const int n = omega.size();
  if (n < 2) stop("omega must contain at least two oscillators");
  if (theta0.size() != n) stop("theta0 and omega must have the same length");
  if (n_keep < 1) stop("n_keep must be positive");
  if (n_transient < 0) stop("n_transient must be non-negative");
  if (!R_finite(dt) || dt <= 0.0) stop("dt must be positive and finite");

  const int total_steps = n_transient + n_keep;
  NumericMatrix out(n_keep + 1, n);

  std::vector<double> theta(n);
  for (int i = 0; i < n; ++i) {
    theta[i] = theta0[i];
  }

  int output_row = 0;
  if (n_transient == 0) {
    for (int i = 0; i < n; ++i) out(0, i) = theta[i];
    output_row = 1;
  }

  for (int step = 1; step <= total_steps; ++step) {
    std::vector<double> k1 = kuramoto_derivative(theta, omega, coupling);

    std::vector<double> temp(n);
    for (int i = 0; i < n; ++i) {
      temp[i] = theta[i] + 0.5 * dt * k1[i];
    }
    std::vector<double> k2 = kuramoto_derivative(temp, omega, coupling);

    for (int i = 0; i < n; ++i) {
      temp[i] = theta[i] + 0.5 * dt * k2[i];
    }
    std::vector<double> k3 = kuramoto_derivative(temp, omega, coupling);

    for (int i = 0; i < n; ++i) {
      temp[i] = theta[i] + dt * k3[i];
    }
    std::vector<double> k4 = kuramoto_derivative(temp, omega, coupling);

    for (int i = 0; i < n; ++i) {
      theta[i] +=
        dt * (k1[i] + 2.0 * k2[i] + 2.0 * k3[i] + k4[i]) / 6.0;
    }

    if (step >= n_transient) {
      if (n_transient > 0 && step == n_transient) {
        for (int i = 0; i < n; ++i) out(0, i) = theta[i];
        output_row = 1;
      } else if (output_row <= n_keep) {
        for (int i = 0; i < n; ++i) {
          out(output_row, i) = theta[i];
        }
        ++output_row;
      }
    }
  }

  return out;
}
