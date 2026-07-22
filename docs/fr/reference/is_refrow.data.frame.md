# Test function to detect cells in reference rows

Test function to detect cells in reference rows

## Usage

``` r
# S3 method for class 'data.frame'
is_refrow(x, ..., partial = TRUE)
```

## Arguments

- x:

  The object to test, to get a field in, or to modify.

- ...:

  Used in methods to add arguments in the future.

- partial:

  Should partial reference rows be counted as reference rows ? Default
  to FALSE.

## Value

A list of logical vectors with the in_refrow fields.
