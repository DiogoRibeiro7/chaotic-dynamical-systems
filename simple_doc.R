# Simple documentation generation
suppressPackageStartupMessages({
  library(roxygen2)
  library(desc)
})

message("Generating documentation...")

# Generate docs
roxygenise(package.dir = ".", clean = FALSE)

# Count results
rd_count <- length(list.files("man", pattern = "\\.Rd$"))
message(sprintf("\nGenerated %d .Rd files in man/", rd_count))
