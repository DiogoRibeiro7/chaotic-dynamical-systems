# Performance Analysis and Benchmarking

This document summarizes the performance optimization features implemented in the chaoticds package.

## C++ Implementation Overview

The package now includes optimized C++ implementations for computationally intensive functions:

### Core C++ Functions
- `simulate_logistic_map_cpp()` - Fast logistic map simulation
- `extremal_index_runs_cpp()` - Optimized extremal index estimation
- `block_maxima_cpp()` - Efficient block maxima computation
- `threshold_exceedances_cpp()` - Fast threshold exceedance detection
- `cluster_sizes_cpp()` - Optimized cluster size analysis
- `quantile_cpp()` - Fast quantile computation

### Wrapper Functions with Auto-Selection
- `simulate_logistic_map_fast()` - Automatically selects R or C++ based on data size
- `extremal_index_runs_fast()` - Auto-optimized extremal index estimation
- `block_maxima_fast()` - Auto-optimized block maxima computation

## Performance Benchmarking

### Built-in Benchmarking Tools
- `benchmark_implementations()` - Compare R vs C++ performance
- `performance_analysis()` - Comprehensive performance analysis
- Automated performance regression testing in CI/CD

### Expected Performance Gains
- **Simulation**: 5-10x speedup for datasets > 5,000 observations
- **Extremal Index**: 3-5x speedup for datasets > 10,000 observations  
- **Block Maxima**: 2-4x speedup for datasets > 50,000 observations
- **Memory Usage**: Linear scaling with optimized memory management

## Testing Coverage

The performance optimizations are thoroughly tested:
- **Unit Tests**: Verify C++ and R implementations produce identical results
- **Performance Tests**: Ensure C++ implementations are faster
- **Edge Case Tests**: Handle empty data, extreme parameters
- **Integration Tests**: Test auto-selection logic
- **Regression Tests**: Prevent performance degradation

## Usage Guidelines

### Automatic Optimization
```r
# These functions automatically choose the best implementation
data <- simulate_logistic_map_fast(50000, r = 3.8, x0 = 0.2)
theta <- extremal_index_runs_fast(data, threshold, run_length = 3)
maxima <- block_maxima_fast(data, 100)
```

### Manual Control
```r
# Force C++ implementation (if available)
data <- simulate_logistic_map_fast(10000, r = 3.8, x0 = 0.2, use_cpp = TRUE)

# Force R implementation
data <- simulate_logistic_map_fast(10000, r = 3.8, x0 = 0.2, use_cpp = FALSE)
```

### Performance Analysis
```r
# Run comprehensive performance analysis
results <- performance_analysis()

# Custom benchmarking
benchmark_results <- benchmark_implementations(sizes = c(1000, 10000, 100000))
```

## Memory Management

### Efficient Workflows
- Automatic detection of large datasets
- Chunked processing for very large datasets (> 100k observations)
- Memory-efficient algorithms with reduced overhead
- Garbage collection optimization

### Best Practices
- Use sampling for threshold selection on large datasets
- Process data in chunks for memory-constrained environments
- Monitor memory usage with built-in tools
- Use C++ implementations for computational bottlenecks

This implementation provides both the high-performance computing capabilities needed for large-scale analysis and the ease-of-use required for interactive exploration, making the chaoticds package suitable for a wide range of applications in extreme value analysis of chaotic dynamical systems.