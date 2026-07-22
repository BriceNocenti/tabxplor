# Stop the tabxplor parallel worker pool

Shuts down the persistent mirai daemons tabxplor starts for
`tab(..., parallel = )`. The pool is otherwise reused for the whole
session and cleaned up automatically when the package is unloaded; call
this to release the workers earlier.

## Usage

``` r
tab_parallel_stop()
```

## Value

`invisible(NULL)`, called for its side effect.

## Examples

``` r
# \donttest{
# after tab(..., parallel = TRUE)
tab_parallel_stop()
# }
```
