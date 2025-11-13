# Script to regenerate documentation
# This bypasses package installation issues

library(roxygen2)

# Generate documentation
message("Generating documentation...")
tryCatch({
  roxygenise(".", roclets = c("rd", "namespace"))
  message("Documentation generated successfully!")

  # Count .Rd files
  rd_files <- list.files("man", pattern = "\\.Rd$", full.names = FALSE)
  message(sprintf("Generated %d .Rd files", length(rd_files)))

}, error = function(e) {
  message("Error generating documentation: ", e$message)
  message("\nTrying alternative approach...")

  # Alternative: just update NAMESPACE
  try({
    roxygenise(".", roclets = "namespace")
    message("NAMESPACE updated")
  })
})
