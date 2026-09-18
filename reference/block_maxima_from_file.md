# Memory-efficient block maxima for very large datasets

Computes block maxima without loading entire dataset into memory at
once.

## Usage

``` r
block_maxima_from_file(file_path, block_size, skip_lines = 0)
```

## Arguments

- file_path:

  Path to file containing data (one value per line)

- block_size:

  Block size

- skip_lines:

  Number of lines to skip at start

## Value

Numeric vector of block maxima
