#!/bin/bash

# setup.sh - Development Environment Setup for chaoticds Package
# This script sets up a complete R package development environment

set -euo pipefail  # Exit on error, treat unset vars as errors, catch pipeline failures

# trap any error to provide a helpful message
trap 'print_error "Setup failed. Check $LOG_FILE for details."' ERR

# Flag for minimal installation (skip heavy packages)
MINIMAL=0
# Log file for capturing setup output
LOG_FILE="setup.log"

# Parse command line arguments
for arg in "$@"; do
    case $arg in
        --minimal)
            MINIMAL=1
            shift
            ;;
        *)
            ;;
    esac
done

echo "============================================"
echo "Setting up chaoticds Package Development Environment"
echo "============================================"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_header() {
    echo -e "\n${BLUE}=== $1 ===${NC}"
}

# Check if R is installed
check_r_installation() {
    print_header "Checking R Installation"
    
    if ! command -v R &> /dev/null; then
        print_error "R is not installed or not in PATH"
        echo "Please install R from https://cran.r-project.org/"
        echo "Required version: R >= 4.0.0"
        exit 1
    fi
    
    R_VERSION=$(R --version | head -1 | grep -oP 'R version \K[0-9]+\.[0-9]+\.[0-9]+')
    print_status "Found R version: $R_VERSION"
    
    # Check if R version is >= 4.0.0
    if R -e "if(getRversion() < '4.0.0') quit(status=1)" --slave 2>/dev/null; then
        print_status "R version is compatible"
    else
        print_error "R version must be >= 4.0.0"
        exit 1
    fi
}

# Create directory structure
setup_directories() {
    print_header "Setting Up Directory Structure"
    
    # Core package directories
    directories=(
        "R"
        "man"
        "tests/testthat"
        "data"
        "data-raw"
        "inst/shiny"
        "vignettes"
        "src"
        ".github/workflows"
    )
    
    for dir in "${directories[@]}"; do
        if [[ ! -d "$dir" ]]; then
            mkdir -p "$dir"
            print_status "Created directory: $dir"
        else
            print_status "Directory exists: $dir"
        fi
    done
}

# Install required R packages
install_r_packages() {
    print_header "Installing R Packages"

    print_status "Installing core development packages..."

    trap '' ERR
    set +e
    MINIMAL_SETUP=$MINIMAL R --slave <<'RSCRIPT'
    # Helper to install packages with error handling
    install_if_needed <- function(pkg) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        cat("Installing", pkg, "...\n")
        tryCatch(
          install.packages(pkg,
                           repos = 'https://cloud.r-project.org',
                           dependencies = TRUE,
                           Ncpus = 2,
                           INSTALL_opts = '--no-docs --no-html'),
          error = function(e) {
            message("Warning: failed to install ", pkg, ": ", e$message)
          }
        )
      } else {
        cat(pkg, "already installed\n")
      }
    }
    
    # Core development packages
    dev_packages <- c(
      'devtools',
      'usethis',
      'testthat',
      'roxygen2',
      'pkgdown',
      'renv',
      'remotes'
    )
    
    # Package dependencies
    deps <- c(
      'Rcpp',
      'assertthat',
      'ggplot2',
      'evd',
      'evir',
      'ismev',
      'extRemes',
      'checkmate',
      'shiny',
      'shinydashboard',
      'plotly',
      'rmarkdown',
      'knitr',
      'microbenchmark'
    )

    # Minimal set of dependencies for quick setup
    minimal_deps <- c(
      'Rcpp',
      'assertthat',
      'ggplot2',
      'checkmate',
      'rmarkdown',
      'knitr'
    )
    
    # Install packages - optionally skip heavy ones
    if (Sys.getenv('MINIMAL_SETUP') == '1') {
      cat('Running minimal installation...\n')
      all_packages <- c(dev_packages, minimal_deps)
    } else {
      all_packages <- c(dev_packages, deps)
    }
    
    cat("Installing", length(all_packages), "packages...\n")
    for (pkg in all_packages) {
      install_if_needed(pkg)
    }

    cat("Package installation complete!\n")
RSCRIPT
    R_EXIT=$?
    set -e
    trap 'print_error "Setup failed. Check $LOG_FILE for details."' ERR

    if [[ $R_EXIT -eq 0 ]]; then
        print_status "R packages installed successfully"
    else
        print_warning "Failed to install some R packages - continuing"
    fi
}

# Initialize renv for reproducible environment
setup_renv() {
    print_header "Setting Up renv Environment"

    if ! R --slave -e "if(!requireNamespace('renv', quietly=TRUE)) quit(status=1)" 2>/dev/null; then
        print_warning "renv package not installed - skipping renv setup"
        return
    fi

    set +e
    if [[ ! -f "renv.lock" ]]; then
        print_status "Initializing renv..."
        R --slave -e "renv::init(); renv::snapshot()" >/dev/null
        R_STATUS=$?
        if [[ $R_STATUS -eq 0 ]]; then
            print_status "renv initialized and snapshot created"
        else
            print_warning "renv initialization failed"
        fi
    else
        print_status "Found existing renv.lock, restoring environment..."
        R --slave -e "renv::restore()" >/dev/null
        R_STATUS=$?
        if [[ $R_STATUS -eq 0 ]]; then
            print_status "renv environment restored"
        else
            print_warning "renv restore failed"
        fi
    fi
    set -e
}

# Create or update DESCRIPTION file
create_description() {
    print_header "Creating/Updating DESCRIPTION File"
    
    if [[ ! -f "DESCRIPTION" ]]; then
        cat > DESCRIPTION << 'EOF'
Package: chaoticds
Type: Package
Title: Extreme Value Analysis for Chaotic Dynamical Systems
Version: 0.1.0
Authors@R: 
    person("Diogo", "Ribeiro", 
           email = "dfr@esmad.ipp.pt", 
           role = c("aut", "cre"),
           comment = c(ORCID = "0009-0001-2022-7072"))
Description: Provides tools for extreme value analysis in chaotic dynamical 
    systems, including simulation of chaotic maps, extremal index estimation,
    block maxima and peaks-over-threshold methods, bootstrap confidence intervals,
    and interactive exploration tools.
License: MIT + file LICENSE
Encoding: UTF-8
LazyData: true
RoxygenNote: 7.2.3
Depends: 
    R (>= 4.0.0)
Imports:
    Rcpp (>= 1.0.0),
    assertthat,
    ggplot2,
    stats,
    checkmate
Suggests:
    testthat (>= 3.0.0),
    evd,
    evir,
    ismev,
    extRemes,
    shiny,
    shinydashboard,
    plotly,
    rmarkdown,
    knitr,
    microbenchmark
LinkingTo: Rcpp
VignetteBuilder: knitr
URL: https://github.com/DiogoRibeiro7/chaotic-dynamical-systems
BugReports: https://github.com/DiogoRibeiro7/chaotic-dynamical-systems/issues
EOF
        print_status "Created DESCRIPTION file"
    else
        print_status "DESCRIPTION file already exists"
    fi
}

# Create or update NAMESPACE file
create_namespace() {
    print_header "Creating/Updating NAMESPACE File"
    
    if [[ ! -f "NAMESPACE" ]]; then
        cat > NAMESPACE << 'EOF'
# Generated by roxygen2: do not edit by hand

export(acf_decay)
export(block_maxima)
export(bootstrap_extremal_index)
export(cluster_exceedances)
export(cluster_histogram)
export(cluster_sizes)
export(cluster_summary)
export(d_check)
export(exceedance_indices)
export(exceedances)
export(extremal_index_intervals)
export(extremal_index_runs)
export(fit_gev)
export(fit_gpd)
export(hill_estimates)
export(hill_plot)
export(hitting_times)
export(launch_explorer)
export(marked_point_process)
export(mean_residual_life)
export(mixing_coefficients)
export(mrl_plot)
export(plot_hts)
export(run_demo)
export(select_threshold)
export(simulate_henon_map)
export(simulate_logistic_map)
export(simulate_orbit)
export(threshold_diagnostics)
export(threshold_exceedances)
importFrom(Rcpp,sourceCpp)
useDynLib(chaoticds, .registration=TRUE)
EOF
        print_status "Created NAMESPACE file"
    else
        print_status "NAMESPACE file already exists"
    fi
}

# Create MIT license
create_license() {
    print_header "Creating License Files"
    
    if [[ ! -f "LICENSE" ]]; then
        cat > LICENSE << 'EOF'
YEAR: 2025
COPYRIGHT HOLDER: Diogo Ribeiro
EOF
        print_status "Created LICENSE file"
    fi
    
    if [[ ! -f "LICENSE.md" ]]; then
        cat > LICENSE.md << 'EOF'
# MIT License

Copyright (c) 2025 Diogo Ribeiro

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
EOF
        print_status "Created LICENSE.md file"
    fi
}

# Setup testing infrastructure
setup_testing() {
    print_header "Setting Up Testing Infrastructure"
    
    # Create testthat.R
    if [[ ! -f "tests/testthat.R" ]]; then
        cat > tests/testthat.R << 'EOF'
library(testthat)
library(chaoticds)

test_check("chaoticds")
EOF
        print_status "Created tests/testthat.R"
    fi
    
    # Create sample test file
    if [[ ! -f "tests/testthat/test-simulate.R" ]]; then
        cat > tests/testthat/test-simulate.R << 'EOF'
test_that("logistic map simulation works", {
  series <- simulate_logistic_map(100, 3.8, 0.2)
  
  expect_length(series, 100)
  expect_type(series, "double")
  expect_true(all(is.finite(series)))
  expect_equal(series[1], 0.2)
})

test_that("henon map simulation works", {
  orbit <- simulate_henon_map(100, 1.4, 0.3, 0.1, 0.1)
  
  expect_s3_class(orbit, "data.frame")
  expect_equal(nrow(orbit), 100)
  expect_equal(ncol(orbit), 2)
  expect_named(orbit, c("x", "y"))
  expect_equal(orbit$x[1], 0.1)
  expect_equal(orbit$y[1], 0.1)
})
EOF
        print_status "Created sample test file"
    fi
}

# Setup CI/CD with GitHub Actions
setup_github_actions() {
    print_header "Setting Up GitHub Actions CI/CD"
    
    if [[ ! -f ".github/workflows/R-CMD-check.yaml" ]]; then
        cat > .github/workflows/R-CMD-check.yaml << 'EOF'
# Workflow derived from https://github.com/r-lib/actions/tree/v2/examples
# Need help debugging build failures? Start at https://github.com/r-lib/actions#where-to-find-help
on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]

name: R-CMD-check

jobs:
  R-CMD-check:
    runs-on: ${{ matrix.config.os }}

    name: ${{ matrix.config.os }} (${{ matrix.config.r }})

    strategy:
      fail-fast: false
      matrix:
        config:
          - {os: macos-latest,   r: 'release'}
          - {os: windows-latest, r: 'release'}
          - {os: ubuntu-latest,   r: 'devel', http-user-agent: 'release'}
          - {os: ubuntu-latest,   r: 'release'}
          - {os: ubuntu-latest,   r: 'oldrel-1'}

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      R_KEEP_PKG_SOURCE: yes

    steps:
      - uses: actions/checkout@v3

      - uses: r-lib/actions/setup-pandoc@v2

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: ${{ matrix.config.r }}
          http-user-agent: ${{ matrix.config.http-user-agent }}
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::rcmdcheck
          needs: check

      - uses: r-lib/actions/check-r-package@v2
        with:
          upload-snapshots: true
EOF
        print_status "Created GitHub Actions workflow"
    fi
}

# Create .gitignore
create_gitignore() {
    print_header "Creating .gitignore"
    
    if [[ ! -f ".gitignore" ]]; then
        cat > .gitignore << 'EOF'
# History files
.Rhistory
.Rapp.history

# Session Data files
.RData

# User-specific files
.Ruserdata

# Example code in package build process
*-Ex.R

# Output files from R CMD build
/*.tar.gz

# Output files from R CMD check
/*.Rcheck/

# RStudio files
.Rproj.user/

# produced vignettes
vignettes/*.html
vignettes/*.pdf

# OAuth2 token, see https://github.com/hadley/httr/releases/tag/v0.3
.httr-oauth

# knitr and R markdown default cache directories
*_cache/
/cache/

# Temporary files created by R markdown
*.utf8.md
*.knit.md

# R Environment Variables
.Renviron

# pkgdown site
docs/

# renv
renv/library/
renv/local/
renv/cellar/
renv/lock/
renv/python/
renv/staging/

# IDE files
.vscode/
*.swp
*.swo

# OS generated files
.DS_Store
.DS_Store?
._*
.Spotlight-V100
.Trashes
ehthumbs.db
Thumbs.db

# Package specific
inst/doc
EOF
        print_status "Created .gitignore"
    fi
}

# Setup pkgdown for documentation website
setup_pkgdown() {
    print_header "Setting Up pkgdown"
    
    if [[ ! -f "_pkgdown.yml" ]]; then
        cat > _pkgdown.yml << 'EOF'
url: ~

template:
  bootstrap: 5

home:
  title: "chaoticds: Extreme Value Analysis for Chaotic Dynamical Systems"

reference:
- title: "Simulation Functions"
  desc: "Functions for simulating chaotic dynamical systems"
  contents:
  - simulate_logistic_map
  - simulate_henon_map
  - simulate_orbit

- title: "Extremal Index Estimation"
  desc: "Methods for estimating the extremal index"
  contents:
  - extremal_index_runs
  - extremal_index_intervals
  - bootstrap_extremal_index

- title: "Block Maxima Methods"
  desc: "Block maxima approach to extreme value analysis"
  contents:
  - block_maxima
  - fit_gev

- title: "Peaks Over Threshold"
  desc: "Peaks-over-threshold methods"
  contents:
  - exceedances
  - fit_gpd
  - mean_residual_life
  - mrl_plot

- title: "Threshold Selection"
  desc: "Tools for selecting appropriate thresholds"
  contents:
  - threshold_diagnostics
  - hill_estimates
  - hill_plot

- title: "Cluster Analysis"
  desc: "Analysis of exceedance clusters"
  contents:
  - threshold_exceedances
  - cluster_exceedances
  - cluster_sizes
  - cluster_summary
  - cluster_histogram

- title: "Interactive Tools"
  desc: "Interactive exploration and visualization"
  contents:
  - launch_explorer

- title: "Utility Functions"
  desc: "Supporting utilities and diagnostics"
  contents:
  - hitting_times
  - plot_hts
  - acf_decay
  - mixing_coefficients
  - d_check

articles:
- title: "Getting Started"
  navbar: ~
  contents:
  - estimating-theta-logistic
  - block-maxima-vs-pot-henon
EOF
        print_status "Created _pkgdown.yml"
    fi
}

# Generate documentation
generate_docs() {
    print_header "Generating Documentation"

    if ! R --slave -e "if(!requireNamespace('devtools', quietly=TRUE) || !requireNamespace('roxygen2', quietly=TRUE)) quit(status=1)" 2>/dev/null; then
        print_warning "devtools/roxygen2 not installed - skipping documentation"
        return
    fi

    print_status "Running roxygen2 to generate documentation..."
    set +e
    R --slave -e "library(devtools); document()" >/dev/null
    if [[ $? -eq 0 ]]; then
        print_status "Documentation generated"
    else
        print_warning "Documentation generation failed"
    fi
    set -e
}

# Run package check
run_package_check() {
    print_header "Running Package Checks"
    
    print_status "Running devtools::check()..."
    R --slave -e "
    library(devtools)
    check_result <- check()
    if (length(check_result\$errors) > 0) {
      cat('ERRORS found:\n')
      cat(paste(check_result\$errors, collapse = '\n'))
      quit(status = 1)
    }
    if (length(check_result\$warnings) > 0) {
      cat('WARNINGS found:\n')
      cat(paste(check_result\$warnings, collapse = '\n'))
    }
    cat('Package check completed successfully!\n')
    " 2>/dev/null || {
        print_warning "Package check found issues - please review and fix"
        return 1
    }
    
    print_status "Package checks passed!"
}

# Main setup function
main() {
    print_header "Starting Development Environment Setup"
    echo "Logs: $LOG_FILE"
    # Capture all output to log file as well as stdout
    exec > >(tee -a "$LOG_FILE") 2>&1
    if [[ "$MINIMAL" -eq 1 ]]; then
        print_status "Minimal installation mode enabled"
    fi
    
    # Change to script directory
    cd "$(dirname "$0")"
    
    # Run setup steps
    check_r_installation
    setup_directories
    install_r_packages
    create_description
    create_namespace
    create_license
    create_gitignore
    setup_testing
    setup_github_actions
    setup_pkgdown
    setup_renv
    generate_docs
    
    print_header "Setup Complete!"
    print_status "Development environment is ready!"
    
    echo ""
    echo "Next steps:"
    echo "1. Load the package: R -e 'devtools::load_all()'"
    echo "2. Run tests: R -e 'devtools::test()'"
    echo "3. Check package: R -e 'devtools::check()'"
    echo "4. Build documentation: R -e 'pkgdown::build_site()'"
    echo ""
    echo "Happy coding! 🚀"
}

# Run main function
main "$@"
