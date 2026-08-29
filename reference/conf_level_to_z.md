# Convert confidence levels into z thresholds

Turn one or several confidence levels into the two-sided normal (z)
thresholds they correspond to, rounded for readability. It is a
convenience for writing the `zscore` color break scale
([`set_color_breaks`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md))
in the vocabulary you already use elsewhere — confidence levels —
instead of remembering that 95 % is 1.96. The scale itself always stores
plain z magnitudes, so `conf_level_to_z(0.95)` and `1.96` are strictly
interchangeable.

## Usage

``` r
conf_level_to_z(conf_level, digits = 2)
```

## Arguments

- conf_level:

  A numeric vector of confidence levels, each between 0 and 1 (e.g.
  `c(0.95, 0.99)`).

- digits:

  Number of digits to round to (default 2). Rounding keeps color legends
  readable (`"+1.96"` rather than `"+1.959964"`); pass `Inf` for the
  exact values.

## Value

A numeric vector of positive z thresholds, the same length as
`conf_level`.

## Examples

``` r
conf_level_to_z(c(0.95, 0.99))
#> [1] 1.96 2.58

# the default `zscore` break scale (color = "contrib", color_signif = "guaranteed_effect")
conf_level_to_z(c(0.95, 0.99, 0.9999, 1 - 2e-9))
#> [1] 1.96 2.58 3.89 6.00

# \donttest{
set_color_breaks(zscore = conf_level_to_z(c(0.95, 0.999)))
set_color_breaks(zscore = c(2, 3, 4, 6))  # or plain z values, identically
# }
```
