# Package startup tasks

Registers global variables to appease R CMD check, and wires up the
Rcpp-compiled DLL so the package's \`\_cpp\` functions resolve at
\`.Call\` time.

## Usage

``` r
.onLoad(libname, pkgname)
```
