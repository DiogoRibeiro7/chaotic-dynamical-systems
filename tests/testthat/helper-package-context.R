# Ensure standalone testthat::test_dir() runs have package context.
# devtools::test() and R CMD check handle this automatically, but this
# helper keeps direct testthat invocations consistent.
if (!"package:chaoticds" %in% search()) {
  suppressPackageStartupMessages(library(chaoticds))
}

# Load packaged example datasets into the test environment when available.
for (nm in c("logistic_ts", "henon_ts", "ar1_ts")) {
  try(data(list = nm, package = "chaoticds", envir = environment()), silent = TRUE)
}
