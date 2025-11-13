# Generate documentation without package compilation
library(roxygen2)
library(pkgload)

# Set working directory
setwd(".")

message("Generating documentation using roxygen2...")

# Try to generate docs with source loading only
tryCatch({
  # Use roxygen2 with explicit load method
  old_env <- Sys.getenv("R_LOAD_METHOD")
  Sys.setenv(R_LOAD_METHOD = "source")

  roxygenise(".", roclets = c("rd", "namespace"), load_code = pkgload::ns_load)

  Sys.setenv(R_LOAD_METHOD = old_env)

  message("\n=== Documentation generation complete ===")

  # Count .Rd files
  rd_files <- list.files("man", pattern = "\\.Rd$")
  message(sprintf("Total .Rd files: %d", length(rd_files)))
  message("\nRecently generated files:")
  system("ls -lt man/*.Rd | head -20")

}, error = function(e) {
  message("Error: ", e$message)
  message("\nAttempting simplified approach...")

  # Fallback: just update NAMESPACE
  try({
    roxygenise(".", roclets = "namespace", load_code = pkgload::ns_load)
    message("NAMESPACE updated")
  }, silent = FALSE)
})
