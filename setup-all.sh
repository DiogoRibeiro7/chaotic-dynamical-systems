#!/usr/bin/env bash
# Set up R and Python environments for chaoticds

# Restore R packages via renv
Rscript -e "if (!require('renv')) install.packages('renv'); renv::restore()" || exit 1

# Install Python dependencies via Poetry
poetry install || exit 1
