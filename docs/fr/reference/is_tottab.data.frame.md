# Test function to detect cells in total tables

Test function to detect cells in total tables

## Usage

``` r
# S3 method for class 'data.frame'
is_tottab(x, ..., partial = FALSE)
```

## Arguments

- x:

  The object to test, to get a field in, or to modify.

- ...:

  Used in methods to add arguments in the future.

- partial:

  Should partial total tabs be counted as total tabs ? Default to FALSE.

## Value

A list of logical vectors, with the data.frame column's tottab fields.
