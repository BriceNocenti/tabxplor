# Cut or transform a numeric variable

Turn a number into the form you want to read it in: cut it into quantile
groups or into bands at the mean and one standard deviation either side
(it becomes an ordinary factor), or transform it and keep it a number.
This is the same operation `tab(shape =)` and
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)`(shape =)`
perform, exposed on its own vector, so a column you cut by hand is
identical to one they cut.

## Usage

``` r
shape_numeric_var(x, shape, w = NULL, name = NULL, ordered = TRUE)
```

## Arguments

- x:

  A numeric vector.

- shape:

  A single string, or a number of groups — see *The shapes* below.

- w:

  Optional weights, the same length as `x`. Quantile breaks and the
  mean/SD landmarks are then weighted — equal shares of the
  *population*, not of the sample.

- name:

  The variable's name, written onto the **first level only**
  (`"age: [18,35) low"`), so a table that names the variable nowhere
  else still says what the levels are levels *of*. `NULL` (the default
  here) writes nothing. It applies to the two CUTS, whose levels are
  intervals and band words; `"values_to_levels"` keeps the raw values,
  which name themselves.

- ordered:

  Whether the resulting factor is `ordered`. Bands and quantile groups
  do have a real order; a model fit does not want one (an ordered factor
  takes polynomial contrasts instead of contrasts against a reference),
  which is why
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  cuts unordered.

## Value

For a cut: a factor whose levels carry the real cut points and, in
words, where each group sits. For `"log"` / `"sqrt"`: a numeric vector.

## The mean of a transform is not the transform of the mean

`"log"` and `"sqrt"` replace the values, so a crosstab column then shows
the mean *of the logarithm* — a different quantity from the logarithm of
the mean, and smaller.
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
renames the column for that reason (`age` becomes `log_age`), so no
header promises the wrong quantity; do the same if you shape a column by
hand.

## The shapes

- `"linear"`: the number as it is — one slope in a model, one mean in a
  crosstab. The default for a column variable, and what `shape` is
  spelled out as when nothing is done.

- `"log"`: replace the variable by its logarithm — diminishing returns.
  Needs strictly positive values.

- `"sqrt"`: replace the variable by its square root. Needs non-negative
  values.

- `"sd_bands"`: four bands cut at the mean and one standard deviation
  either side. Each level names its own cut (`[30,48) ; < mean`), so the
  label can be checked against the interval beside it. The cut points
  mean the same thing across sub-samples of one variable, where quantile
  breaks move with each one; but the bands are NOT balanced, and on a
  skewed variable a landmark falling outside the data is dropped (an
  exponential variable gets three bands, not four).

- `"median"`: two groups of equal size, cut at the median — the coarsest
  reading of a number.

- `"terciles"`: three groups of equal size.

- `"quartiles"`: four groups of equal size. The counts are balanced, so
  every group answers on a comparable base.

- `"quintiles"`: five groups of equal size.

- `"deciles"`: ten groups of equal size. Reads a gradient, but ten rows
  need a large sample to keep each base usable.

- `"values_to_levels"`: one level per distinct value, in numeric order.
  Right for a counted number or a 1-7 scale; unreadable for a continuous
  one, which is what `"auto"` decides.

- an integer `k` (2 to 20): `k` quantile groups of equal size.

## See also

[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) and
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md),
whose `shape` argument is this function applied once to the whole
population, before any sub-table or sub-model is split off.

## Examples

``` r
# \donttest{
age <- forcats::gss_cat$age
table(shape_numeric_var(age, "sd_bands", name = "age"))
#> 
#> age: 18 to 29 ; < -1σ 30 to 47 ; below mean 48 to 64 ; above mean 
#>                  3816                  7691                  6063 
#>      65 to 89 ; > +1σ 
#>                  3837 
table(shape_numeric_var(age, "quartiles"))
#> 
#> 18 to 32 33 to 45 46 to 58 59 to 89 
#>     5101     5549     5097     5660 
# }
```
