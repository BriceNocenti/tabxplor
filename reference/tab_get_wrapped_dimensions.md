# Get the number of actual rows and the max character length of a table after being wrapped (count `\n` as a linebreak).

Get the number of actual rows and the max character length of a table
after being wrapped (count `\n` as a linebreak).

## Usage

``` r
tab_get_wrapped_dimensions(tabs, no_tab_vars = FALSE, width_pad = 4L)
```

## Arguments

- tabs:

  A data.frame.

- no_tab_vars:

  For data.frame of class `tabxplor_tab`, remove `tab_vars`.

- width_pad:

  Number of characters lengths between columns.

## Value

A list with the row count and the max character width.
