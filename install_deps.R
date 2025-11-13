# Install missing dependencies
packages <- c("assertthat", "evd", "checkmate", "ggplot2", "Rcpp")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Installing %s...", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  } else {
    message(sprintf("%s already installed", pkg))
  }
}

message("All dependencies installed!")
