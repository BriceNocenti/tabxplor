# Coercion between two tab

Coercion between two tab

## Usage

``` r
tab_cast(x, to, ..., x_arg = "", to_arg = "")

tab_ptype2(x, y, ..., x_arg = "", y_arg = "")

# S3 method for class 'tabxplor_tab.tabxplor_tab'
vec_ptype2(x, y, ...)

# S3 method for class 'tabxplor_tab.tabxplor_tab'
vec_cast(x, to, ...)

# S3 method for class 'tabxplor_tab.tbl_df'
vec_ptype2(x, y, ...)

# S3 method for class 'tbl_df.tabxplor_tab'
vec_ptype2(x, y, ...)

# S3 method for class 'tabxplor_tab.tbl_df'
vec_cast(x, to, ...)

# S3 method for class 'tbl_df.tabxplor_tab'
vec_cast(x, to, ...)

# S3 method for class 'tabxplor_tab.data.frame'
vec_ptype2(x, y, ...)

# S3 method for class 'data.frame.tabxplor_tab'
vec_ptype2(x, y, ...)

# S3 method for class 'tabxplor_tab.data.frame'
vec_cast(x, to, ...)

# S3 method for class 'data.frame.tabxplor_tab'
vec_cast(x, to, ...)

gtab_cast(x, to, ..., x_arg = "", to_arg = "")

gtab_ptype2(x, y, ..., x_arg = "", y_arg = "")

# S3 method for class 'tabxplor_grouped_tab.tabxplor_grouped_tab'
vec_ptype2(x, y, ...)

# S3 method for class 'tabxplor_grouped_tab.tabxplor_grouped_tab'
vec_cast(x, to, ...)

# S3 method for class 'tabxplor_grouped_tab.grouped_df'
vec_ptype2(x, y, ...)

# S3 method for class 'grouped_df.tabxplor_grouped_tab'
vec_ptype2(x, y, ...)

# S3 method for class 'tabxplor_grouped_tab.grouped_df'
vec_cast(x, to, ...)

# S3 method for class 'grouped_df.tabxplor_grouped_tab'
vec_cast(x, to, ...)

# S3 method for class 'tabxplor_grouped_tab.tabxplor_tab'
vec_ptype2(x, y, ...)

# S3 method for class 'tabxplor_tab.tabxplor_grouped_tab'
vec_ptype2(x, y, ...)

# S3 method for class 'tabxplor_grouped_tab.tabxplor_tab'
vec_cast(x, to, ...)

# S3 method for class 'tabxplor_tab.tabxplor_grouped_tab'
vec_cast(x, to, ...)

# S3 method for class 'tabxplor_grouped_tab.tbl_df'
vec_ptype2(x, y, ...)

# S3 method for class 'tbl_df.tabxplor_grouped_tab'
vec_ptype2(x, y, ...)

# S3 method for class 'tabxplor_grouped_tab.tbl_df'
vec_cast(x, to, ...)

# S3 method for class 'tbl_df.tabxplor_grouped_tab'
vec_cast(x, to, ...)

# S3 method for class 'tabxplor_grouped_tab.data.frame'
vec_ptype2(x, y, ...)

# S3 method for class 'data.frame.tabxplor_grouped_tab'
vec_ptype2(x, y, ...)

# S3 method for class 'tabxplor_grouped_tab.data.frame'
vec_cast(x, to, ...)

# S3 method for class 'data.frame.tabxplor_grouped_tab'
vec_cast(x, to, ...)
```

## Arguments

- x, y, to:

  Subclasses of data frame.

- ...:

  For future extensions.

- x_arg:

  Argument names for x and y. These are used in error messages to inform
  the user about the locations of incompatible types.

- to_arg:

  Argument names for x and to. These are used in error messages to
  inform the user about the locations of incompatible types.

- y_arg:

  Argument names for x and y. These are used in error messages to inform
  the user about the locations of incompatible types.

## Value

A tibble of class `tabxplor_tab`.

A tibble of class `tabxplor_tab`.

A tibble of class `tabxplor_tab`.

A tibble of class `tabxplor_tab`.

A tibble of class `tabxplor_tab`.

A tibble.

A tibble of class `tabxplor_tab`.

A tibble.

A tibble of class `tabxplor_tab`.

A data.frame.

A tibble of class `tabxplor_tab`.

A data.frame.

An object of class `tabxplor_grouped_tab`.

An object of class `tabxplor_grouped_tab`.

An object of class `tabxplor_grouped_tab`.

An object of class `grouped_df`.

An object of class `tabxplor_grouped_tab`.

An object of class `grouped_df`.

An object of class `tabxplor_grouped_tab`.

An object of class `tabxplor_tab`.

An object of class `tabxplor_grouped_tab`.

An object of class `tabxplor_tab`.

An object of class `tabxplor_grouped_tab`.

An object of class `tbl_df`.

An object of class `tabxplor_grouped_tab`.

An object of class `tbl_df`.

An object of class `tabxplor_grouped_tab`.

An data.frame.

An object of class `tabxplor_grouped_tab`.

An data.frame.

## Functions

- `vec_ptype2(tabxplor_tab.tabxplor_tab)`: find common ptype between
  tabxplor_tab and tabxplor_tab

- `vec_cast(tabxplor_tab.tabxplor_tab)`: convert tabxplor_tab to
  tabxplor_tab

- `vec_ptype2(tabxplor_tab.tbl_df)`: find common ptype between
  tabxplor_tab and tbl_df

- `vec_ptype2(tbl_df.tabxplor_tab)`: find common ptype between tbl_df
  and tabxplor_tab

- `vec_cast(tabxplor_tab.tbl_df)`: convert tbl_df to tabxplor_tab

- `vec_cast(tbl_df.tabxplor_tab)`: convert tabxplor_tab to tbl_df

- `vec_ptype2(tabxplor_tab.data.frame)`: find common ptype between
  tabxplor_tab and data.frame

- `vec_ptype2(data.frame.tabxplor_tab)`: find common ptype between
  data.frame and tabxplor_tab

- `vec_cast(tabxplor_tab.data.frame)`: convert data.frame to
  tabxplor_tab

- `vec_cast(data.frame.tabxplor_tab)`: convert tabxplor_tab to
  data.frame

- `vec_ptype2(tabxplor_grouped_tab.tabxplor_grouped_tab)`: find common
  ptype between tabxplor_grouped_tab and tabxplor_grouped_tab

- `vec_cast(tabxplor_grouped_tab.tabxplor_grouped_tab)`: convert
  tabxplor_grouped_tab to tabxplor_grouped_tab

- `vec_ptype2(tabxplor_grouped_tab.grouped_df)`: find common ptype
  between tabxplor_grouped_tab and grouped_df

- `vec_ptype2(grouped_df.tabxplor_grouped_tab)`: find common ptype
  between grouped_df and tabxplor_grouped_tab

- `vec_cast(tabxplor_grouped_tab.grouped_df)`: convert grouped_df to
  tabxplor_grouped_tab

- `vec_cast(grouped_df.tabxplor_grouped_tab)`: convert
  tabxplor_grouped_tab to grouped_df

- `vec_ptype2(tabxplor_grouped_tab.tabxplor_tab)`: find common ptype
  between tabxplor_grouped_tab and tabxplor_tab

- `vec_ptype2(tabxplor_tab.tabxplor_grouped_tab)`: find common ptype
  between tabxplor_tab and tabxplor_grouped_tab

- `vec_cast(tabxplor_grouped_tab.tabxplor_tab)`: convert tabxplor_tab to
  tabxplor_grouped_tab

- `vec_cast(tabxplor_tab.tabxplor_grouped_tab)`: convert
  tabxplor_grouped_tab to tabxplor_tab

- `vec_ptype2(tabxplor_grouped_tab.tbl_df)`: find common ptype between
  tabxplor_grouped_tab and tbl_df

- `vec_ptype2(tbl_df.tabxplor_grouped_tab)`: find common ptype between
  tbl_df and tabxplor_grouped_tab

- `vec_cast(tabxplor_grouped_tab.tbl_df)`: convert tbl_df to
  tabxplor_grouped_tab

- `vec_cast(tbl_df.tabxplor_grouped_tab)`: convert tabxplor_grouped_tab
  to tbl_df

- `vec_ptype2(tabxplor_grouped_tab.data.frame)`: find common ptype
  between tabxplor_grouped_tab and data.frame

- `vec_ptype2(data.frame.tabxplor_grouped_tab)`: find common ptype
  between data.frame and tabxplor_grouped_tab

- `vec_cast(tabxplor_grouped_tab.data.frame)`: convert data.frame to
  tabxplor_grouped_tab

- `vec_cast(data.frame.tabxplor_grouped_tab)`: convert
  tabxplor_grouped_tab to data.frame
