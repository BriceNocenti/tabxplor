# Printing method for class tabxplor_tab

Printing method for class tabxplor_tab

## Usage

``` r
# S3 method for class 'tabxplor_tab'
print(
  x,
  width = NULL,
  ...,
  n = 100,
  max_extra_cols = NULL,
  max_footer_lines = NULL,
  min_row_var = 30,
  get_text = FALSE
)
```

## Arguments

- x:

  Object to format or print.

- width:

  Width of text output to generate.

- ...:

  Passed on to `tbl_format_setup()`.

- n:

  Number of rows to show.

- max_extra_cols:

  Number of extra columns to print abbreviated information for, if the
  width is too small for the entire tibble.

- max_footer_lines:

  Maximum number of footer lines.

- min_row_var:

  Minimum number of characters for the row variable. Default to 30.

- get_text:

  Set to `TRUE` to get the text as a character vector instead of a
  printed output.

## Value

A printed table.
