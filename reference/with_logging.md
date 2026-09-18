# Evaluate an expression with error logging

This helper evaluates \`expr\` and writes any warning or error message
to \`log_file\` before re-throwing the error. A custom informational
\`msg\` can be provided to annotate the log when evaluation begins. It
is useful for scripts that should record failures for later inspection
while still stopping execution.

## Usage

``` r
with_logging(expr, log_file = "chaoticds.log", msg = NULL)
```

## Arguments

- expr:

  An expression to evaluate.

- log_file:

  Character string giving the path to a log file. Defaults to
  "chaoticds.log" in the current directory.

- msg:

  Optional message to record before evaluating \`expr\` for context.

## Value

The result of evaluating \`expr\` if successful.

## Examples

``` r
tmp <- tempfile()
try(with_logging(stop("oops"), tmp, msg = "example"))
#> Error in eval(substitute(expr), parent.frame()) : oops
readLines(tmp)
#> [1] "2026-09-18 23:20:09.334483 INFO: example"
#> [2] "2026-09-18 23:20:09.334829 ERROR: oops"  
```
