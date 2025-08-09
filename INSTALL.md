# Installation

Run the setup script to install all dependencies and initialise `renv`:

```bash
./setup.sh --minimal  # omit heavy optional packages
```

The script logs all output to `setup.log`, which you can inspect if something goes wrong during installation.

This will install the required R packages, create the basic directory structure and set up testing infrastructure. You can also restore the environment manually and install the package from source:

```r
install.packages("renv")
renv::restore()
devtools::install_local(".")
```

To run the test suite manually, make sure the `testthat`, `checkmate`, and `assertthat`
packages are installed:

```r
install.packages(c("testthat", "checkmate", "assertthat"))
```

For the optional Python utilities, execute `./setup-all.sh`.
