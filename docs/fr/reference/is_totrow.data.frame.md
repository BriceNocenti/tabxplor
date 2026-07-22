# Test function to detect cells in total rows

Test function to detect cells in total rows

## Usage

``` r
# S3 method for class 'data.frame'
is_totrow(x, ..., partial = FALSE)
```

## Arguments

- x:

  The object to test, to get a field in, or to modify.

- ...:

  Used in methods to add arguments in the future.

- partial:

  Should partial total rows be counted as total rows ? Default to FALSE.

## Value

A list of logical vectors, with the data.frame column's totrow fields.
