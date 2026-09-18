# Memory Management Guide
## chaoticds Package

**Purpose**: Best practices for efficient memory usage when working with large datasets

---

## Quick Reference

| Data Size | Memory Strategy | Expected RAM |
|-----------|----------------|--------------|
| < 10K | Default | < 1 MB |
| 10K - 100K | Vectorized R | 1-10 MB |
| 100K - 1M | C++ functions | 10-100 MB |
| 1M - 10M | C++ + monitoring | 100MB - 1GB |
| > 10M | Chunked processing | Constant |

---

## 1. Understanding Memory Usage

### 1.1 R Object Sizes

```r
library(pryr)

# Check size of objects
n <- 1000000
x <- simulate_logistic_map_cpp(n, 3.8, 0.2)

object_size(x)  # ~7.6 MB (8 bytes per double * 1M)

# Compare to R version
x_r <- simulate_logistic_map(n, 3.8, 0.2)
object_size(x_r)  # Same size, but slower to create
```

### 1.2 Memory Overhead

**R overhead per vector**:
- Object header: ~40 bytes
- ALTREP optimization: Lazy evaluation for sequences
- Copy-on-modify: R may create copies

**C++ overhead**:
- Minimal overhead (~40 bytes header)
- No lazy evaluation
- Direct memory allocation

---

## 2. Memory-Efficient Coding Practices

### 2.1 Pre-allocation

**Bad** - Growing vectors:
```r
results <- c()
for (i in 1:1000000) {
  results <- c(results, compute(i))  # Reallocates every iteration
}
# Memory: Reallocations create O(n²) copies
# Time: O(n²) instead of O(n)
```

**Good** - Pre-allocated vectors:
```r
results <- numeric(1000000)
for (i in 1:1000000) {
  results[i] <- compute(i)  # No reallocation
}
# Memory: Single allocation
# Time: O(n)
```

**Best** - Vectorized or C++:
```r
results <- compute_vectorized(1:1000000)
# Memory: Single allocation
# Time: Optimized
```

### 2.2 Avoid Unnecessary Copies

**Copy detection**:
```r
library(pryr)

x <- 1:1000000
address(x)  # e.g., "0x7f8b3c000000"

y <- x  # No copy yet (COW = copy-on-write)
address(y)  # Same address

y[1] <- 0  # NOW it copies
address(y)  # Different address
```

**Minimize copies**:
```r
# BAD - creates copy
modify_vector <- function(vec) {
  vec[1] <- 0  # Copies entire vector
  return(vec)
}

# BETTER - use C++ for in-place modification
# or work with indices instead of values
```

### 2.3 Garbage Collection

**Explicit GC**:
```r
# After large operations
for (i in 1:100) {
  large_data <- simulate_logistic_map_cpp(1000000, 3.8, 0.2)
  # ... process ...
  rm(large_data)

  if (i %% 10 == 0) {
    gc()  # Force garbage collection every 10 iterations
  }
}
```

**Automatic GC monitoring**:
```r
# Check GC frequency
gcinfo(TRUE)  # Enable GC messages
# ... run code ...
gcinfo(FALSE)  # Disable

# Check GC stats
gc()  # Shows current memory usage and triggers GC
```

---

## 3. Memory-Efficient Functions

### 3.1 Using Chunked Processing

For datasets larger than RAM:

```r
# Instead of loading entire dataset
x <- read_large_file("10GB_data.txt")  # May crash

# Use chunked processing
result <- extremal_index_chunked(
  x,
  threshold = 0.95,
  run_length = 2,
  chunk_size = 100000  # Only load 100K at a time
)
```

### 3.2 File-Based Processing

```r
# Process directly from file
block_maxima_result <- block_maxima_from_file(
  file_path = "large_timeseries.txt",
  block_size = 100,
  skip_lines = 1  # Skip header
)

# Memory usage: O(block_size) instead of O(n)
```

### 3.3 Streaming Algorithms

For unbounded data streams:

```r
# Example: Online mean calculation
online_mean <- function(new_value, old_mean, n) {
  old_mean + (new_value - old_mean) / (n + 1)
}

# Process data incrementally
mean_val <- 0
for (i in seq_along(data_stream)) {
  mean_val <- online_mean(data_stream[i], mean_val, i - 1)
}
# Memory usage: O(1)
```

---

## 4. Monitoring Memory Usage

### 4.1 Real-time Monitoring

```r
library(pryr)

# Before operation
mem_before <- mem_used()
cat("Memory before:", format(mem_before, units = "auto"), "\n")

# Run operation
x <- simulate_logistic_map_cpp(10000000, 3.8, 0.2)

# After operation
mem_after <- mem_used()
cat("Memory after:", format(mem_after, units = "auto"), "\n")
cat("Memory used:", format(mem_after - mem_before, units = "auto"), "\n")

# Check object size
cat("Object size:", format(object_size(x), units = "auto"), "\n")
```

### 4.2 Memory Profiling

```r
library(profvis)

# Profile memory allocations
p <- profvis({
  results <- run_demo(n = 100000)
}, prof_mem = TRUE)

print(p)
# Shows both time and memory allocations
```

### 4.3 Setting Memory Limits

```r
# Limit R's memory usage (Linux/Mac)
# In .Renviron or at startup
R_MAX_VSIZE=8G  # Limit to 8 GB

# Check current limits
memory.limit()  # Windows only
mem.limits()    # All platforms (if available)
```

---

## 5. Large Dataset Strategies

### 5.1 Dataset Size Categories

**Small (< 10K observations)**:
```r
# No special handling needed
x <- simulate_logistic_map(10000, 3.8, 0.2)
results <- run_demo(n = 10000)
```

**Medium (10K - 1M observations)**:
```r
# Use C++ implementations
x <- simulate_logistic_map_cpp(500000, 3.8, 0.2)

# Monitor memory occasionally
if (length(x) > 100000) {
  cat("Data size:", format(object_size(x), units = "auto"), "\n")
}
```

**Large (1M - 10M observations)**:
```r
# Use C++ + explicit GC
x <- simulate_logistic_map_cpp(5000000, 3.8, 0.2)

# Process in stages
threshold <- quantile_cpp(x, 0.95)
theta <- extremal_index_runs_cpp(x, threshold, 2)

# Clean up intermediate results
rm(threshold)
gc()
```

**Very Large (> 10M observations)**:
```r
# Use chunked processing
theta <- extremal_index_chunked(
  x,
  threshold = quantile_cpp(x, 0.95),
  run_length = 2,
  chunk_size = 1000000
)

# Or process from file
if (file.exists("huge_data.txt")) {
  bm <- block_maxima_from_file("huge_data.txt", block_size = 100)
}
```

### 5.2 Parallel Processing Memory

When using parallel processing:

```r
# Each worker uses memory
n_cores <- 4
per_core_memory <- object_size(x)

# Total memory = per_core_memory * n_cores + overhead
total_memory <- per_core_memory * n_cores * 1.2  # 20% overhead

if (total_memory > available_memory) {
  warning("Insufficient memory for parallel processing")
  n_cores <- 1  # Fall back to sequential
}

# Use parallel processing
result <- bootstrap_extremal_index_parallel(
  x, threshold, run_length,
  B = 1000,
  n_cores = n_cores
)
```

---

## 6. Memory Leaks and Debugging

### 6.1 Detecting Memory Leaks

**Test for leaks**:
```r
library(pryr)

# Baseline memory
gc()
mem_baseline <- mem_used()

# Run function many times
for (i in 1:1000) {
  x <- simulate_logistic_map_cpp(10000, 3.8, 0.2)
  theta <- extremal_index_runs_cpp(x, quantile(x, 0.95), 2)
  rm(x, theta)

  if (i %% 100 == 0) {
    gc()
    mem_current <- mem_used()
    cat(sprintf("Iteration %d: %s\n", i,
                format(mem_current - mem_baseline, units = "auto")))
  }
}

# If memory grows over time → potential leak
# If memory stays constant → no leak
```

### 6.2 Common Memory Issues

**Issue 1: Circular references**
```r
# Creates circular reference (R garbage collector handles this)
a <- list()
b <- list()
a$ref <- b
b$ref <- a

# Clean up
rm(a, b)
gc()
```

**Issue 2: Unclosed connections**
```r
# BAD
con <- file("data.txt", "r")
# ... forgot to close ...
# Memory leak if repeated

# GOOD
con <- file("data.txt", "r")
on.exit(close(con))  # Always closes
data <- readLines(con)
```

**Issue 3: Large environments**
```r
# Functions capture their environment
create_function <- function() {
  large_data <- rnorm(1000000)  # 8 MB

  function(x) {
    x + 1  # Doesn't use large_data, but captures it
  }
}

f <- create_function()
# f now has 8 MB in its environment even though unused

# SOLUTION: Don't create large objects in function environments
```

### 6.3 Debugging Memory Problems

**Using Rprof**:
```r
# Profile memory allocations
Rprof("memory_profile.out", memory.profiling = TRUE)

# Run code
results <- run_demo(n = 100000)

Rprof(NULL)

# Analyze profile
summaryRprof("memory_profile.out", memory = "both")
```

**Using gc() for diagnostics**:
```r
# Detailed GC information
gc(verbose = TRUE)

# Shows:
# - Ncells: Number of cons cells (language objects)
# - Vcells: Number of vector cells (data)
# - Memory usage in MB
```

---

## 7. Platform-Specific Considerations

### 7.1 Windows

```r
# Check memory limit
memory.limit()  # Returns size in MB

# Increase memory limit
memory.limit(size = 16000)  # Set to 16 GB

# Note: Cannot exceed physical RAM + virtual memory
```

### 7.2 Linux/Mac

```r
# Check system memory
system("free -h")  # Linux
system("vm_stat")  # Mac

# Monitor R process
system("ps aux | grep R")

# Set R memory limit (in .Renviron)
# R_MAX_VSIZE=16G
```

### 7.3 High Performance Computing (HPC)

```r
# On HPC cluster, respect memory limits
# Check allocated memory (SLURM example)
# #SBATCH --mem=32G

# Monitor memory usage
library(future)
plan(multisession, workers = 8)

# Process data respecting memory limits
# ...
```

---

## 8. Best Practices Summary

### 8.1 DO's

✅ Pre-allocate vectors
✅ Use C++ implementations for large data
✅ Monitor memory usage for n > 1M
✅ Use chunked processing for n > 10M
✅ Call gc() after large operations
✅ Close file connections with on.exit()
✅ Use vectorized operations
✅ Test for memory leaks
✅ Set appropriate memory limits
✅ Use profiling tools

### 8.2 DON'Ts

❌ Grow vectors in loops
❌ Create unnecessary copies
❌ Keep large temporary objects
❌ Ignore memory warnings
❌ Use R loops for large n
❌ Leave connections open
❌ Process entire file if chunking possible
❌ Use parallel processing without checking memory
❌ Ignore garbage collection
❌ Assume unlimited memory

### 8.3 Emergency Memory Management

If you encounter "out of memory" errors:

1. **Immediate action**:
   ```r
   rm(list = ls())  # Remove all objects
   gc()             # Force garbage collection
   ```

2. **Reduce data size**:
   ```r
   # Subsample data
   x_small <- x[seq(1, length(x), by = 10)]  # Take every 10th point
   ```

3. **Use chunked processing**:
   ```r
   # Process in smaller chunks
   chunk_size <- floor(length(x) / 10)
   ```

4. **Increase memory limit**:
   ```r
   # Windows
   memory.limit(size = 16000)
   ```

5. **Use external memory**:
   ```r
   # Process from file instead of loading into memory
   ```

---

## 9. Example Workflows

### 9.1 Small Data Workflow

```r
# Standard workflow for n < 100K
x <- simulate_logistic_map(50000, 3.8, 0.2)
bm <- block_maxima(x, 100)
gev_fit <- fit_gev(bm)
theta <- extremal_index_runs(x, quantile(x, 0.95), 2)
```

Memory usage: < 5 MB
No special handling needed

### 9.2 Medium Data Workflow

```r
# Optimized workflow for 100K < n < 1M
x <- simulate_logistic_map_cpp(500000, 3.8, 0.2)

# Monitor memory
cat("Data size:", format(object_size(x), units = "auto"), "\n")

# Use C++ functions
threshold <- quantile_cpp(x, 0.95)
theta <- extremal_index_runs_cpp(x, threshold, 2)

# Clean up
rm(threshold)
gc()
```

Memory usage: ~50 MB
Explicit monitoring and GC

### 9.3 Large Data Workflow

```r
# Memory-efficient workflow for n > 1M
n <- 5000000

# Simulate with C++
x <- simulate_logistic_map_cpp(n, 3.8, 0.2)
cat("Simulated data:", format(object_size(x), units = "auto"), "\n")

# Chunked analysis
threshold <- quantile_cpp(x, 0.95)
theta <- extremal_index_chunked(x, threshold, 2, chunk_size = 500000)

cat("Extremal index:", theta, "\n")

# Bootstrap with parallel processing
boot <- bootstrap_extremal_index_parallel(
  x, threshold, 2,
  B = 1000,
  n_cores = 4
)

# Clean up aggressively
rm(x, threshold)
gc()
```

Memory usage: ~500 MB peak
Chunked processing and parallel bootstrap

### 9.4 Very Large Data Workflow

```r
# File-based workflow for n > 10M

# Assume data in file "large_data.txt"
file_path <- "large_data.txt"

# Process block maxima from file
bm <- block_maxima_from_file(file_path, block_size = 100)

cat("Block maxima computed:", length(bm), "blocks\n")
cat("Memory usage:", format(object_size(bm), units = "auto"), "\n")

# Fit GEV
gev_fit <- fit_gev(bm)

# For full extremal index, would need to load/process in chunks
# ... chunked processing implementation ...
```

Memory usage: Constant (< 100 MB)
File-based processing, never load full dataset

---

## 10. Troubleshooting

| Problem | Cause | Solution |
|---------|-------|----------|
| "Cannot allocate vector" | Insufficient memory | Use chunked processing or reduce data size |
| Slow performance | Memory swapping | Increase RAM or reduce data size |
| Memory grows over time | Memory leak or not cleaning up | Check for leaks, call gc() |
| Crash during parallel processing | Insufficient memory for workers | Reduce n_cores or data size |
| Unexpected memory usage | Copies being created | Check with address(), avoid modifications |
| File too large to load | Limited RAM | Use file-based or streaming processing |

---

## Conclusion

Effective memory management is crucial for analyzing large chaotic dynamical systems. Key strategies:

1. **Choose the right tools** - C++ for large data, R for small
2. **Monitor proactively** - Check memory usage before problems occur
3. **Process smartly** - Use chunking and streaming for very large data
4. **Clean up** - Explicit rm() and gc() calls
5. **Test thoroughly** - Check for leaks and performance degradation

With these practices, you can efficiently analyze datasets from thousands to billions of observations.

---

**Guide Version**: 1.0
**Last Updated**: 2025-01-13
