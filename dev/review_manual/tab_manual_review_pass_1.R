

library(devtools)
load_all()
options(tabxplor.parallel = TRUE, tabxplor.cleannames = TRUE, tabxplor.print = "kable")



pc18 <- readRDS("~/Data/Pratiques culturelles/Pratiques culturelles 2018/pc18.rds")
musique_vars <- c("ROCK", "JAZZ", "CLASSIQUE", "VARIETE", "ELECTRO", "METAL", "CHANSON", "WORLD", "RAP", "TRADI")
pc18 <- pc18 |>
  select(-any_of(c("CHANSON", "WORLD", "TRADI", "VARIETE", "RNB", "ELECTRO", "RAP", "METAL", "ROCK", "JAZZ", "OPERA", "CLASSIQUE"))) |>
  rename(any_of(c(CHANSON   = "E1001", WORLD = "E1002", TRADI = "E1003", VARIETE   = "E1004", RNB= "E1005", ELECTRO = "E1006", 
RAP= "E1007", METAL= "E1008", ROCK= "E1009", JAZZ= "E1010", OPERA= "E1011", CLASSIQUE = "E1012"
  )))
pc18$CHANSON   <- forcats::fct_recode(pc18$CHANSON,  "1-Chanson ou variété française" = "1-Chansons ou variétés françaises", "2-Non" = "2-Non")
pc18$WORLD     <- forcats::fct_recode(pc18$WORLD,  "1-World" = "1-Musiques du monde", "2-Non" = "2-Non")
pc18$TRADI     <- forcats::fct_recode(pc18$TRADI,  "1-Tradi" = "1-Musiques traditionnelles",  "2-Non" = "2-Non")
pc18$VARIETE   <- forcats::fct_recode(pc18$VARIETE, "1-Variété inter- nationale" = "1-Variétés internationales", "2-Non" = "2-Non")
pc18$ELECTRO   <- forcats::fct_recode(pc18$ELECTRO, "1-Électro, techno" = "1-Musiques électroniques, techno", "2-Non"   = "2-Non")
pc18$RAP       <- forcats::fct_recode(pc18$RAP, "1-Rap" = "1-Hip hop, rap", "2-Non"          = "2-Non")
pc18$METAL     <- forcats::fct_recode(pc18$METAL,"1-Metal, hard rock" = "1-Metal, hard rock","2-Non"= "2-Non")
pc18$ROCK      <- forcats::fct_recode(pc18$ROCK, "1-Pop, rock" = "1-Pop, rock", "2-Non"       = "2-Non")
pc18$JAZZ      <- forcats::fct_recode(pc18$JAZZ, "1-Jazz" = "1-Jazz", "2-Non"  = "2-Non")
pc18$CLASSIQUE <- forcats::fct_recode(pc18$CLASSIQUE, "1-Classique" = "1-Musique classique", "2-Non" = "2-Non")
pc18 <- pc18 |> dplyr::select(-any_of(c("NB_MUSIQUE"))) |> score_from_lv1(name = "NB_MUSIQUES", vars_list = musique_vars)

pc18_young <- pc18 |> dplyr::filter(AGE >= 18 & AGE < 25) #  593 individuals


# color = c("diff", "ratio"), color_signif = "ignore"
# set_color_breaks(breaks = list(pct_ratio = c(1.2, 1.5, 2, 4), mean_diff = NULL ) )
set_color_breaks(pct_ratio = list(over = c(NA, 1.5, 2, 4), under = c(NA, 1.5, 2, 4)))

#   exploratory tables with several row_vars and several col_vars (nb_cine, NB_CONCERTS = numeric variable)
rows1 <- c("DIPLOM", "CRITREVENU", "CRITAGE", "CSTOTR")
cols1 <- c("CONCERTS", "TELE", "JV", "nb_cine", "NB_CONCERTS")

### base row and col percentages tests ---

tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "row", color = TRUE, na = "drop")
# Color legends
# - For text colors, but the colored "+5; +10; +20; +30 etc." in the color legends in bold, 
#    to match the visual sensation of the real numbers in the table.
# tab_export() with "kable"
# - In complex display cells like "100% (n= 849)" or "3 (σ7 )", the padding alignment is not right
#   on kable : we forgot default was DejaVu Sans, not a monospace font ! 
#   To solve this, we’ll first try to stay with DejaVu Sans (in which all numbers have the same width)
#    but pad with several spaces at once (2 ? 3 ?) ; if it does not work, we can try DejaVu Sans Mono, 
#    but its look and readability is not so good in light mode.
#   Same would happen on complex displays kept as text in Excel exports, I guess, so it may need a fix too.
#   Of course, console and md, that are used with monospace fonts, are currently perfectly padded and should stay so.
# - Names of col_vars a bit too big (put same font size than normal column, but no bold/keep plain font weight)
#     Maybe too much top/bottom padding are names of col_vars. 
#   With numeric vars : keep names of col_vars, but put in bold here only (more important) ;
#     but since the "level" column name repeat it, just print "mean" or "mean (sd)" (plain font weights)
# tooltips :
# - with pct="row" the 100% total column interactive tooltips have a useless "ratio: ×1" (opposite for the 100% row with pct="col") ; it should be invisible here
# - the reference row, here the total row, also have a useless "ratio: ×1", the right thing to display would be "ratio: ref" (like the current "diff : ref")
#   maybe simplify a bit for reference rows or columns since "diff: ref ; ratio: ref" is not very useful, and only say "ref" for this part (keeping the very important "n: " part)
# - on Positron Viewer, it’s impossible to access the tooltip of the rightmost column (here : NB_CONCERTS)
#    ensure tooltips of the last column are printed at it’s left rather than at it’s right ? 
# - with numeric col_vars, it’s written "diff: ××1.3" ; we should verify it’s is in fact, in this case, diff or ratio, and fix the double cross problem

tab(pc18, DIPLOM, c(TELE, JV), wt = POND, pct = "col", color = TRUE, na = "drop")
tab(pc18, c(DIPLOM, CSTOTR), c(TELE, JV), wt = POND, pct = "col", color = TRUE, na = "drop")
# with pct = "col", for an unkown reason, these is far too much left and right padding and the 
#  table is not compact enough (barely fits on the Viewer = difficult for the user to compare numbers)
# - here `n` is printed on it’s own row when there is only 1 row_vars, with looks good ;
#   the problem being that **the `n` rows disappears when there is two col_vars or more** !
# - so we should ensure n row always display with `add_n=TRUE` (default). 

tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "row", color = TRUE, na = "drop") |>
  tab_export(theme = "dark", transpose = TRUE)
# tab_export() with "kable" and theme = "dark" :
# - for formattings of the Light mode table are lost, the resulting table is far less compact,
#   so only a few rows and columns currently fit the screen. Bottom padding for table cells and 
#   maybe top padding for headers, are far too big. Too much right and left padding too everywhere.
# - Color legends (footers) appear the same text size than table text, but should me smaller, 
#   Too much padding here too. 
# - **Rule should be**: the dark mode should mimic the light mode, apart from the colors and few formatting specificities
#   ensuring better readability in dark mode (only justified exceptions). 
# - Overall base background color too light (far lighter than colored backgrounds in cells, so not #111111)
# - Names of col_vars appear in grey, difficult to read : go to white. 

tab(pc18_young, all_of(rows1), all_of(cols1),wt = POND, pct = "row", na = "drop", 
  color = TRUE, chi2 = TRUE # test = TRUE
)
# - there’s not `test = TRUE` possibility, only chi2 = TRUE (but it’s F Welch for numeric vars) : 
#   chi2 argument must still work, but be soft-deprecated in favor of `test` ; and it should be 
#   consistent across functions (except for tab_many which is itself deprecated and can stay with Chi2)
# - Here mirai parallelisation also fails with `chi2 = TRUE` (it still fails with no numeric col_vars)
#   with this error : 
#      "Error in `mirai_map()`:
#      ℹ In index: 3.
#      Caused by error in `map2()`:
#      ℹ In index: 1.
#      ℹ With name: CONCERTS.
#      Caused by error in `rep()`:
#      ! invalid 'times' argument"

### `transpose = TRUE` tests ---
tab(pc18, DIPLOM, CONCERTS, wt = POND, pct = "row", color = TRUE, na = "drop") |>
  tab_export(transpose = TRUE)
# `transpose = TRUE` works with 2 variables, but mess with the formatting a bit in a unexcepted way:
#   with pct = "col" at `n` row is added with add_n ; here it appears as "100% (n=849)" display,
#   which takes too much horizontal space ;
# **Rule should be** : for one row var and one col var, `pct = "row", tab_tranpose = TRUE` should
#    be (near) the same as just `pct = "col"`, but handling better the complex cases where the tranposition
#    directly with the vctrs fields mess with the column-level attributes.

tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "row", color = TRUE, na = "drop") |>
  tab_export(transpose = TRUE)
# `transpose = TRUE` : error "`tab_transpose()` does not support tables with `tab_vars` yet."
# - It would be complicated for tab_vars (no symmetric), but it should work for several row_vars : 
#   let’s improve the function that way ; doing it at export must remove the former limitations
#   of the vctrs fields (ex : for a numeric col_vars, going to rows with tabxplor_fmt meant
#   losing the color column-level attribute, etc. ; at export level it’s possible to use the 
#   data in the fmt fields but transpose rows and columns). 


### "grey_non_signif" and "guaranteed_effect" tests ---

tab(pc18, all_of(rows1), all_of(cols1),wt = POND, pct = "row", na = "drop", 
  color = TRUE, color_signif = "grey_non_signif"
)
tab(pc18, all_of(rows1), all_of(cols1),wt = POND, pct = "row", na = "drop", 
  color = TRUE, color_signif = "guaranteed_effect"
)
# When `ci = "diff"` is not provided, `color_signif = "grey_non_signif"` and "guaranteed_effect" grey out all cells : 
# - like in former implementation with `diff_ci` color, when the user asks for colors with signif,
#   diff confidence intervals should be provided by default, since color and color_signif
#   are first level arguments that should be user-friendly and force the calculation of their required computations
#  (edge case would be when the user force to `ci="cell"`, maybe fail with an error here is the most readable ?)

withr::with_options(list(tabxplor.print = "console"), { # look at both tables on top of another to compare
  tab(pc18_young, DIPLOM, CONCERTS, wt = POND, pct = "row", na = "drop", color = TRUE) |> print()
  tab(pc18_young, DIPLOM, CONCERTS, wt = POND, pct = "row", na = "drop", 
      color = TRUE, ci = "diff", color_signif = "grey_non_signif"
    ) |> 
   print()
}
)
tab(pc18_young, DIPLOM, CONCERTS, wt = POND, pct = "row", na = "drop", 
color = TRUE, ci = "diff", color_signif = "grey_non_signif"
)
# - cells with non significant differences from reference see their text color, but also 
#   their ratio color dissapearing, which is reassuring. Is ci also tested on ratio here,
#   (or useless, not significant in diff means not significant in ratio ?) ? 
# - tooltips : only the diff prints the ci, for ex "diff: +16% [-1.4;27.8]" ; 
#   ratio should also print the ci in this case, "ratio: ×1.3 [×1.x;1.x]", would it be 
#   easy and fast to calculate, or are there caveats (long to calculate because reference needed ? ;
#   if such a case, how to do, is there a way or it’s too difficult/long ?) ?
# - by the way : here the console output use a very bright background colors palette, 
#   which seems to be the wrong one ; I think text color use the Light mode palette too ; 
#   it should detect as IDE level if theme is light or dark and apply the right palette.

list(
  tab(pc18_young, DIPLOM, CONCERTS, wt = POND, pct = "row", na = "drop", color = TRUE), 
  tab(pc18_young, DIPLOM, CONCERTS, wt = POND, pct = "row", na = "drop", 
    color = TRUE, ci = "diff", color_signif = "grey_non_signif"
  )
) |> `class<-`(c("tabxplor_tabs", "list"))
# Using a list of tabs may merge them at the vctrs fields step, so columns-level attributes 
#  are merged and, here, we lose the "grey_non_signif" and tooltips with ci of the second table
# - I thought for list of tables and html export, we behaviour was "use a html container to display 
#   both tables on top of one another" (simpler, no merge as vctrs fields level, etc. ; 
#   simply, the user will provide the already merged table of tab() with several row_vars 
#   to merge everything on the same table, since it ensures the columns are the same etc.)

tab(pc18_young, all_of(rows1), all_of(cols1),wt = POND, pct = "row", na = "drop", 
  color = TRUE, ci = "diff", color_signif = "guaranteed_effect"
)
# a kind of a hack to see what should be colored (avoiding the 0 and 1 bound in the manual breaks)
tab(pc18_young, all_of(rows1), all_of(cols1),wt = POND, pct = "row", na = "drop", 
  color = TRUE, color_signif = "guaranteed_effect", ci = "diff",
  color_breaks = list(pct_diff = c(0.00001, 0.05, 0.15, 0.25), pct_ratio = c(1.151, 1.5, 2, 4)/1.15)
)
# I can see a cell (CSTOTR "Professions intermédiaires" × "2 à 4 concerts") printing : 
#  "diff:+7% [0.4; 16.6]", which seem inconsistent since if 0 is not in the confidence interval
#  it should always have colors. 
# - It’s because the breaks for "guaranteed_effect" should always, 
#  by definition, start at 0 (otherwise we do not see all that is guaranteed), and here it 
#  starts at +5% like a normal "grey_non_signif". The rule implemented in the past was : 
#  "substact the first break to all breaks to offset them and have the first break always being 0".
#  testthat tests are truly needed here, to see if this auto breaks offset is consistent, with edge cases, etc.
# - Ratio should have the same mecanism around the 1 bound. For example : `c(1.15, 1.5, 2, 4)/1.15``
# - With my hack to see the cells that should really be colored here, all cells with a text color also have a bg color, 
#    and the reverse too, which is reassuring.
# - (The user can’t provide it itself, since 0 is not allowed in breaks : it must be internal calculations)
# - tooltips : with "guaranteed_effect" the tooltip does’nt give the negative effect, 
#   and the user might not know the guaranteed_effect is the bound of the interval closest to zero,
#   so the tooltip should calculate and add "guaranteed_diff : +1.7%" at the start ; 
#   is the guaranteed ratio easy to have, or complicated (and useless ?) ? 


### `levels = "first"` tests ---
rows2 <- c("SEXE", "DIPLOM", "CRITREVENU", "CRITAGE")
cols2 <- c(musique_vars, "NB_MUSIQUES") # plus the related score as numeric variable
tab(pc18, all_of(rows2), all_of(cols2), wt = POND, pct = "row", color = TRUE, na = "drop", levels = "first")
# - obviously, with binary col_vars, variable names and first level are redundant, 
#   but at least it’s clear to everyone that each is a different variable. 


### tab_xl tests ----
tab(pc18, all_of(rows2), all_of(cols2), wt = POND, pct = "row", color = TRUE, na = "drop", 
  levels = "first", ref = 1
) |> 
  tab_xl(open = FALSE, path = "~/out/Excel_test")
# - Excel export should always print the path of the created file in console, otherwise there are 
#   many cases (like : default) where the user can’t find it
# - Color legends : "Background colour (ratio): Cells ≥ the Total row ×1.5; ×2; ×4" can’t use 
#   different background colors inside the same Excel cell I guess, but the result is that 
#   breaks are really difficult to read (light colors over a white background). 
#   The least worse fallback I can think about : -0.2 oklch lightness same hue (keep same chroma or, when not possible, the cap chroma for L and H)
#    (not recalculated for each cell, not to hinder performance)
# - separated mean and sd columns for numeric variable. Here for example name is "NB_MUSIQUES",
#    columns are "NB_MUSIQUES" and "NB_MUSIQUES_sd" : very redundant, and since it’s Excel not tibble
#    col names can be duplicated. So I would want, for the levels headers : "mean" "sd" (variable name already above) ; 
#     and since it’s short names, reduce the columns width if possible (still ensuring all digits can be printed on all lines, and variable name fits) . 
#   Not only in Excel, but anywhere mean and sd cols are separated.
# - The title displays "levels by multi (tabbed by row_var)" : clearly, zero word is informative, it was not made for such 
#    tables of tables with many rows and col vars in the first place (and it was made for when only tab_vars gave multi tables).
#    Since we can’t print all variables names, what would be a good fallback here ? Same for sheet name, not informative. 
#    Make me propositions about what default title to give, for inline title (can be a bit long) and sheet name (must be short) both


### tab_md tests ----
tab(pc18, all_of(rows2), c(ROCK, JAZZ, CLASSIQUE), wt = POND, pct = "row", color = TRUE, na = "drop", 
  levels = "first", ref = 1
) |> 
  tab_export("md")
# - Is there no easy way to print markdown inside a html wrapper with css, 
#    using `tab_md_css()` internally ? like `tab_export("html_md")` ? What engine to use 
#    to convert markdown to html tables ? Should still pass `theme`.
#    If useless and user should use kable for this, I can understand.

# - The double text + bg spans make it difficult to read for humans. But I guess the user 
#   can just use only text colors, with no background colors.

# - The padding for number is currently a bit awkward, for example : 
# |        ROCK        | 
# |         Pop, rock  | 
# |-------------------:| 
# |  **23%**           | 
# | [    38%]{.p2 .o2} | 
# | [    52%]{.p3 .o3} | 
# | [    56%]{.p4 .o3} | 
# | [    62%]{.p4 .o3} | 
# | [    65%]{.p4 .o3} | 
# | [    66%]{.p4 .o3} | 
# | [    48%]{.p3 .o3} | 
# | ------------------ | 
# 
# A better padding, giving back a bit of horizontal space, would be 
#    (keep alignment between spanned numbers and non-spanned numbers / 
#     it’s ok to start the * of ref row/col after | without white space, if is saves horizontal space) : 
# |    ROCK        | 
# |     Pop, rock  | 
# |---------------:| 
# |**23%**         | 
# | [38%]{.p2 .o2} | 
# | [52%]{.p3 .o3} | 
# | [56%]{.p4 .o3} | 
# | [62%]{.p4 .o3} | 
# | [65%]{.p4 .o3} | 
# | [66%]{.p4 .o3} | 
# | [48%]{.p3 .o3} | 
# | -------------- | 


# - Padding is not respected between cells with one, and cells with two, pandoc spans : 
# | [    68%]{.p4 .o3} |
# |     [    48%]{.p2} |
# 
#   The more readable padding should be to align the percentages for human readability in monospace font : 
# | [    68%]{.p4 .o3} |
# | [    48%]{.p2    } |




# The current output is not valid markdown and doesn’t render to an html table with knitr / pandoc
# (problems : 
#     `| |` inside title row, used with several vars, is not valid ; just `|-|` on the title row make it work
#     two title rows is not valid, so "ROCK" "JAZZ" appear out of the rendered table. Remove (or workaround ?) ?
# )
# ```md
# |                |                 |        ROCK        | |        JAZZ        |
# |                | levels          |          Pop, rock | |               Jazz |
# |:---------------|:----------------|-------------------:| |-------------------:|
# | **SEXE**       | **Homme**       |            **54%** | |            **29%** |
# |                | Femme           |     [    42%]{.m2} | |                26% |
# |                | Total           |     [    48%]{.m1} | |                27% |
# ```
# 
# This is rendering on quarto editor render html, with color styles working well. 
# See an example of .md and example of rendered html at `dev/review_manual/tab_md_test.md` and `dev/review_manual/tab_md_test.htm`
# ```md
# <style>
# .p1{color:#02A5B3;}
# .p2{color:#0891C9;}
# .p3{color:#0267C7;}
# .p4{color:#300DFD;}
# .m1{color:#DCA331;}
# .m2{color:#DE7C01;}
# .m3{color:#DD5301;}
# .m4{color:#D60103;}
# .o1{background-color:#DFFCFF;}
# .o2{background-color:#D7EFFF;}
# .o3{background-color:#CEE3FF;}
# .o4{background-color:#BBCCFF;}
# .u1{background-color:#FFF4E1;}
# .u2{background-color:#FFE6D3;}
# .u3{background-color:#FFD7C8;}
# .u4{background-color:#FFBAAF;}
# </style>
# 
# |                |                 |        ROCK        | |        JAZZ        |
# |                | levels          |          Pop, rock | |               Jazz |
# |:---------------|:----------------|-------------------:|-|-------------------:|
# | **SEXE**       | **Homme**       |            **54%** | |            **29%** |
# |                | Femme           |     [    42%]{.m2} | |                26% |
# |                | Total           |     [    48%]{.m1} | |                27% |
# ```

# More generally, I want the markdown file to include everything for a good formatting 
#  inside the rendered html output (from quarto editor), using inline styles, yaml frontmatter, etc. :
# - `cat(tab_md_css())` styles should, like in kable, make all colored text in **bold** font weight
# - look at the rendered html in `dev/review_manual` to know how to customise styles for a more compact, 
#    better formatted html table, closer to the kable one (but it needs not to be as complex !). 
#   Avoid white elephants and long code, **the best result would be the better formatting for the less code/text**.
#   Here it’s not compact enough, too much blank space, and it certainly take all the space inside the html box, 
#    less padding everywhere, blank colums should be very thin, borders can be minimal but must be more readable, etc.
# - **Rule** : both the .md file, and the rendered html file, must be human readable + machine readable.



### tab html custom engine ---

# If you think this customisation and fine-tuning would be better done in it’s own Claude Code session, we can do that.
tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "row", color = TRUE, na = "drop") |> tab_export() # kable default
tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "row", color = TRUE, na = "drop") |>
  tab_export(engine = "html") |>
  `class<-`(c("kableExtra", "knitr_kable")) # bad workaround to look at the result in Positron Viewer pane
# - The result of tab_export(engine = "html") should render as html in both Positron interactive session (should render in Viewer pane) and .Rmd/.qmd rendering – like kableExtra does. 
# - The current workaround with classes may not be the best way to do it, but it actually renders the table in Positron Viewer pane
# - Anyway, the resulting html must continue to work with common css customisation, as kableExtra does, or with few specific variations. Our default formatting should only be that : a good, compact, readable default that can be overwritten.
# - About the current formatting (but I’m not sure it’s the right one, but it’s what renders in Viewer pane) : 
#   - wrapping in headers is not working, still shows "Télé:<br>occasionnel" on the same line
#   - The background colors are less readable than kable ones : they take the whole cell, kable have them around the text only ; they are rectangle, kable have modern rounded edges. On our custom html table, the backgrounds used in color legends are actually closer to what kable do. 
#   - the font is not the same, and I think kable actually use the wanted one (DejaVu Sans Condensed for text, DejaVu Sans for numbers), I’m not sure, but I think it’s not the same one
#   - It’s very compact, but padding betweens different rows is a bit too less to be readable and "modern compact".
#   - For text, a very small left and right padding, like perceptually around 1mm, would be better too, here it touches the border.
#   - With colors, borders between different column variable have many different colors, but should all be uniform black (in light mode). 
#   - The row under mouse pointer appear in in specific color, which is good, but it’s grey and kable yellow is more visible and modern.
#   - Tooltips are good in light mode. But a strange thing that both kable and custom html do : the first columns display the tooltip in one line, it’s good ; the several last columns tooltips appear like on 4 rows, which is less good.


tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "row", color = TRUE, na = "drop") |>
  tab_export(engine = "html", theme = "dark") |>
  `class<-`(c("kableExtra", "knitr_kable")) 
# - We’ll have to fine tune because, here, on my high-end display, contrasts are too high. 
#   We’ll try to go for "#CECDC3"" instead of "#ffffff" for base text and everything.
#   Dark may also be too dark, maybe try "#222222" : I’ll need to fine-tune background colors, by anyway there current result is not very readable (compared to light mode background colors)
# - On Positron IDE Viewer, the table rows are in dark, but the rest of the pane, empty, is all white, 
#  which is awful (should be the same background color than the base table).
# - Tooltips are still white text on black background : we should think about changing their colors in Dark mode. 





