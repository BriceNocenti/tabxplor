# Stop the tabxplor parallel worker pool

Shuts down the persistent mirai daemons tabxplor starts under
`options(tabxplor.parallel = )`. The pool is otherwise reused for the
whole session and cleaned up when the package is unloaded; call this to
release the workers earlier.

## Usage

``` r
tab_parallel_stop()
```

## Value

`invisible(NULL)`, called for its side effect.

## See also

[tabxplor-options](https://bricenocenti.github.io/tabxplor/reference/tabxplor-options.md)
for `tabxplor.parallel`, the switch that starts the pool.

## Examples

``` r
# \donttest{
# after options(tabxplor.parallel = TRUE)
tab_parallel_stop()
# }
```
