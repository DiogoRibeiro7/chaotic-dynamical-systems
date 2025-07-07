#include <Rcpp.h>
using namespace Rcpp;

//' Fast logistic map simulation (C++ implementation)
//' 
//' Efficient C++ implementation of the logistic map for improved performance
//' with large datasets.
//' 
//' @param n Number of iterations
//' @param r Parameter r of the logistic map
//' @param x0 Initial value
//' @return Numeric vector of the time series
//' @export
// [[Rcpp::export]]
NumericVector simulate_logistic_map_cpp(int n, double r, double x0) {
  NumericVector x(n);
  x[0] = x0;
  
  for (int i = 1; i < n; i++) {
    x[i] = r * x[i-1] * (1.0 - x[i-1]);
  }
  
  return x;
}

//' Fast threshold exceedance detection (C++ implementation)
//' 
//' Efficient C++ implementation for finding threshold exceedances.
//' 
//' @param x Numeric vector of data
//' @param threshold Threshold value
//' @return IntegerVector of indices where x > threshold
//' @export
// [[Rcpp::export]]
IntegerVector threshold_exceedances_cpp(NumericVector x, double threshold) {
  std::vector<int> indices;
  
  for (int i = 0; i < x.size(); i++) {
    if (x[i] > threshold) {
      indices.push_back(i + 1);  // R uses 1-based indexing
    }
  }
  
  return wrap(indices);
}

//' Fast cluster size computation (C++ implementation)
//' 
//' Efficient C++ implementation for computing cluster sizes of exceedances.
//' 
//' @param exceedance_indices Integer vector of exceedance indices
//' @param run_length Maximum gap between exceedances in same cluster
//' @return IntegerVector of cluster sizes
//' @export
// [[Rcpp::export]]
IntegerVector cluster_sizes_cpp(IntegerVector exceedance_indices, int run_length) {
  if (exceedance_indices.size() == 0) {
    return IntegerVector(0);
  }
  
  std::vector<int> cluster_sizes;
  int current_cluster_size = 1;
  
  for (int i = 1; i < exceedance_indices.size(); i++) {
    if (exceedance_indices[i] - exceedance_indices[i-1] <= run_length) {
      current_cluster_size++;
    } else {
      cluster_sizes.push_back(current_cluster_size);
      current_cluster_size = 1;
    }
  }
  
  // Add the last cluster
  cluster_sizes.push_back(current_cluster_size);
  
  return wrap(cluster_sizes);
}

//' Fast extremal index estimation using runs method (C++ implementation)
//' 
//' Efficient C++ implementation of the runs estimator for extremal index.
//' 
//' @param x Numeric vector of data
//' @param threshold Threshold value
//' @param run_length Run length parameter
//' @return Numeric scalar of extremal index estimate
//' @export
// [[Rcpp::export]]
double extremal_index_runs_cpp(NumericVector x, double threshold, int run_length) {
  // Find exceedances
  IntegerVector exceed_indices = threshold_exceedances_cpp(x, threshold);
  
  if (exceed_indices.size() == 0) {
    return NA_REAL;
  }
  
  // Count clusters
  IntegerVector sizes = cluster_sizes_cpp(exceed_indices, run_length);
  int num_clusters = sizes.size();
  int total_exceedances = exceed_indices.size();
  
  if (num_clusters == 0) {
    return NA_REAL;
  }
  
  return (double)num_clusters / (double)total_exceedances;
}

//' Fast block maxima computation (C++ implementation)
//' 
//' Efficient C++ implementation for computing block maxima.
//' 
//' @param x Numeric vector of data
//' @param block_size Size of each block
//' @return NumericVector of block maxima
//' @export
// [[Rcpp::export]]
NumericVector block_maxima_cpp(NumericVector x, int block_size) {
  int n = x.size();
  int num_blocks = n / block_size;
  
  NumericVector maxima(num_blocks);
  
  for (int b = 0; b < num_blocks; b++) {
    double block_max = x[b * block_size];
    
    for (int i = 1; i < block_size; i++) {
      int idx = b * block_size + i;
      if (idx < n && x[idx] > block_max) {
        block_max = x[idx];
      }
    }
    
    maxima[b] = block_max;
  }
  
  return maxima;
}

//' Fast quantile computation (C++ implementation)
//' 
//' Simple quantile computation for threshold selection.
//' 
//' @param x Numeric vector of data
//' @param prob Probability (0 to 1)
//' @return Numeric scalar quantile value
//' @export
// [[Rcpp::export]]
double quantile_cpp(NumericVector x, double prob) {
  NumericVector sorted_x = clone(x);
  std::sort(sorted_x.begin(), sorted_x.end());
  
  int n = sorted_x.size();
  double index = prob * (n - 1);
  int lower = (int)floor(index);
  int upper = (int)ceil(index);
  
  if (lower == upper) {
    return sorted_x[lower];
  } else {
    double weight = index - lower;
    return sorted_x[lower] * (1.0 - weight) + sorted_x[upper] * weight;
  }
}