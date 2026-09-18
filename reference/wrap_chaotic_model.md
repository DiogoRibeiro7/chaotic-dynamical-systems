# Wrap fitted model objects with chaoticds metadata

Internal helper to standardize fitted model objects while preserving
their original structure and class behavior.

## Usage

``` r
wrap_chaotic_model(fit, model, method, threshold = NULL)
```

## Arguments

- fit:

  Underlying fitted model object.

- model:

  Character scalar identifying model family (e.g., "gev", "gpd").

- method:

  Character scalar identifying fitting backend.

- threshold:

  Optional numeric threshold for POT/GPD models.

## Value

The same object with class \`chaotic_model\` prepended and metadata
stored in attributes.
