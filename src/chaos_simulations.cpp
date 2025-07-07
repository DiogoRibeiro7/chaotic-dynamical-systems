#include <Rcpp.h>
using namespace Rcpp;

//' Fast logistic map simulation (C++ implementation)
//' 
//' High-performance C++ implementation of the logistic map for large simulations.
//' 
//' @param n Integer number of iterations
//' @param r Numeric growth rate parameter
//' @param x0 Numeric initial condition
//' @return NumericVector containing the orbit
//' @export
// [[Rcpp::export]]
NumericVector simulate_logistic_map_cpp(int n, double r, double x0) {
  NumericVector x(n);
  x[0] = x0;
  
  for (int i = 1; i < n; i++) {
    x[i] = r * x[i-1] * (1 - x[i-1]);
  }
  
  return x;
}

//' Fast Hénon map simulation (C++ implementation)
//' 
//' High-performance C++ implementation of the Hénon map for large simulations.
//' 
//' @param n Integer number of iterations
//' @param a Numeric parameter a
//' @param b Numeric parameter b  
//' @param x0 Numeric initial x value
//' @param y0 Numeric initial y value
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
    x[i] = 1 - a * x[i-1] * x[i-1] + y[i-1];
    y[i] = b * x[i-1];
  }
  
  return DataFrame::create(Named("x") = x, Named("y") = y);
}

//' Fast threshold exceedances identification (C++ implementation)
//' 
//' High-performance identification of threshold exceedances.
//' 
//' @param x NumericVector of observations
//' @param threshold Numeric threshold value
//' @return IntegerVector of exceedance indices (1-based)
//' @export
// [[Rcpp::export]]
IntegerVector threshold_exceedances_cpp(NumericVector x, double threshold) {
  std::vector<int> indices;
  int n = x.size();
  
  for (int i = 0; i < n; i++) {
    if (x[i] > threshold) {
      indices.push_back(i + 1);  // R uses 1-based indexing
    }
  }
  
  return wrap(indices);
}

//' Fast cluster exceedances computation (C++ implementation)
//' 
//' High-performance clustering of exceedances using runs method.
//' 
//' @param indices IntegerVector of sorted exceedance indices
//' @param run_length Integer maximum gap for clustering
//' @return List containing cluster information
//' @export
// [[Rcpp::export]]
List cluster_exceedances_cpp(IntegerVector indices, int run_length) {
  if (indices.size() == 0) {
    return List::create(Named("clusters") = List::create(),
                       Named("n_clusters") = 0);
  }
  
  std::vector<std::vector<int>> clusters;
  std::vector<int> current_cluster;
  current_cluster.push_back(indices[0]);
  
  for (int i = 1; i < indices.size(); i++) {
    if (indices[i] - indices[i-1] <= run_length) {
      current_cluster.push_back(indices[i]);
    } else {
      clusters.push_back(current_cluster);
      current_cluster.clear();
      current_cluster.push_back(indices[i]);
    }
  }
  clusters.push_back(current_cluster);
  
  List cluster_list(clusters.size());
  for (size_t i = 0; i < clusters.size(); i++) {
    cluster_list[i] = wrap(clusters[i]);
  }
  
  return List::create(Named("clusters") = cluster_list,
                     Named("n_clusters") = (int)clusters.size());
}