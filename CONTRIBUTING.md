# Contributing Guidelines

We welcome contributions! Please keep commits focused and well described.
Use the following format for commit messages:

```
Short summary (50 characters or less)

Optional longer description explaining the motivation and approach.
```

Run the unit tests before submitting a pull request:

```bash
R -q -e 'devtools::test()'
R -q -e 'devtools::check(manual = FALSE)'
```

## C++ fast paths

Every new compute-heavy function (simulators, estimators, inner-loop
routines) ships with a `_cpp` fast path alongside the R reference. The R
version is the spec and stays the entry point in examples and vignettes;
the `_cpp` variant lives under `src/additional_optimizations.cpp` (or
`src/fast_algorithms.cpp` for EVT primitives), mirrors the R argument
names and defaults, and uses the same arithmetic ordering so a parity test
at `tolerance = 1e-8` passes.

Workflow after editing C++:

```r
Rcpp::compileAttributes()   # regenerates R/RcppExports.R + src/RcppExports.cpp
devtools::document()        # picks up @export tags, updates NAMESPACE + .Rd
devtools::load_all()        # recompiles the DLL and exposes the new function
```

A parity test in `tests/testthat/test-cpp-performance.R` is required.
An auto-routing `_fast` wrapper (see `R/fast-functions.R`) is encouraged
when the function has a clear "small data → R, large data → C++"
threshold.
