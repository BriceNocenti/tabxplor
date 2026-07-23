# CSS for the colour spans of [`tab_md`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)

A thin wrapper around
[`tab_css(chrome = FALSE)`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md),
kept for discoverability alongside
[`tab_md`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md).
The stylesheet does not depend on the table – classes name a palette
**slot**, not a break – so `tabs` is ignored and one stylesheet styles
every table in a document.

## Usage

``` r
tab_md_css(tabs = NULL, ...)
```

## Arguments

- tabs:

  Ignored (the CSS is table-independent). Kept so `tab_md_css(tabs)`
  still reads naturally.

- ...:

  Passed to
  [`tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  (`theme`, `style_tag`, `file`).

## Value

A character string of CSS (invisible when `file` is given).

## See also

[`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md),
which is the generator and also styles `tab_kable(engine = "html")`.

## Examples

``` r
cat(tab_md_css())
#> <style>
#> .p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
#> .p1,.tabxplor-tab .p1{color:#02A5B3;}
#> .p2,.tabxplor-tab .p2{color:#0891C9;}
#> .p3,.tabxplor-tab .p3{color:#0267C7;}
#> .p4,.tabxplor-tab .p4{color:#300DFD;}
#> .m1,.tabxplor-tab .m1{color:#DCA331;}
#> .m2,.tabxplor-tab .m2{color:#DE7C01;}
#> .m3,.tabxplor-tab .m3{color:#DD5301;}
#> .m4,.tabxplor-tab .m4{color:#D60103;}
#> .o1,.tabxplor-tab .o1{background-color:#DFFCFF;}
#> .o2,.tabxplor-tab .o2{background-color:#D7EFFF;}
#> .o3,.tabxplor-tab .o3{background-color:#CEE3FF;}
#> .o4,.tabxplor-tab .o4{background-color:#BBCCFF;}
#> .u1,.tabxplor-tab .u1{background-color:#FFF4E1;}
#> .u2,.tabxplor-tab .u2{background-color:#FFE6D3;}
#> .u3,.tabxplor-tab .u3{background-color:#FFD7C8;}
#> .u4,.tabxplor-tab .u4{background-color:#FFBAAF;}
#> </style>
```
