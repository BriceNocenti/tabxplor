
# Manual reviews PASS 2 ----

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

#   `levels = "first"`
rows2 <- c("SEXE", "DIPLOM", "CRITREVENU", "CRITAGE")
cols2 <- c(musique_vars, "NB_MUSIQUES") # plus the related score as numeric variable




# `dev/review_manual/phase14e_html_engine.html`, custom html tables manual review : 
# - New default is much better. Dark mode / Light mode toggle working well in my web browser. 
# - The row_var column repeat the name on each row : only keep once, like in some other exports (ex : tab_md) ; 
#    on html would there be a way to print it vertically (not wasting vertical space depending on the number of levels, 
#    but rather wrapping on several vertical lines when needed). Same for Excel, see below, add a shared function, be consistent between export types ?
#   Also add an argument to totally remove both row_var and col vars names, consistent among the different exports.
# - The new html padding is nearly perfect, but it’s actually the old thousand-separator "(n=1 811)" that was wrong : 
#    more generally, in html and Excel exports, please replace all unbreakable spaces with with this good 1 digit sep,
#    everywhere paddind have to be aligned well. Also in normal console display etc., since it seems to be the right special char here ?
#   Ex. (not sure special chars still here after copy-paste): 
#      Divorced	  6% (n= 35)	14% (n= 88)	81% (n=  510)	100% (n=  633)
#      Widowed	  2% (n=  8)	17% (n= 67)	81% (n=  314)	100% (n=  389)
#      Married	  7% (n=123)	 9% (n=159)	84% (n=1 529)	100% (n=1 811)
#   Another alignement/padding problem from html, more difficult, bold cells are not perfectly aligned with plain font weight cells.
#   Here the only solution is to use the same rule than for "100% (n=  519)" : only the mean/main field goes in bold with text color ; the sd or any second field always stays in plain font weight. Same for html, and potentiel Excel equivalent (not needed for mean/sd since they are different columns in Excel), with shared functions
#       "4 (σ11  )" # greyed out text
#       "7 (σ13  )" # colored text
# - Table 2 "composite display + p-value row" in-cells padding is right, result is compact
# - Table 1 "theme = auto (toggle your OS dark mode)" is not compact enough : 
#    levels and Total columns are very wide for nothing, not any text calls for it even if wrap_cols var high. Main thing to fix.
#    number/fmt colums are better, but there’s still room for a more compact display. 
#    For example, a tvhours cell is half numbers half blank, even if the length of header would permit thinner ; 
#     a bit of blank between column is good for readability but not as much as this ? Keep an option for fixed width + would it be possible to add a reliable auto-width feature ?
# - numeric variables : when, I guess, sd is NA, the cells must still be padded for the mean to align 
#    with others means for maximum human readability assuming monospace font. Here it does something like (copy-pasted) : 
#       "relig	Other eastern       1.0
#        relig	Hinduism     1.7 (σ2.1)"
# - numeric variables : "tvhours" is still repeating for the variable name + the normal header (factor level), 
#    better keep the variable name + just write "mean (sd)" in the normal header / level row (or whatever display)
# - drop `inst/tab.css` with the custom html engine if it’s not needed anymore.


# `dev/review_manual/phase14e_html_engine.html` was right, but when I do it myself, in Positron Viewer pane, 
#  and web browser both, the text color actually change the borders colors (even greyed out cells), which is awful : 
#    I want all borders with the same color, here the same shade of white than normal text color.
tab(pc18, all_of(rows2), c(ROCK, JAZZ, CLASSIQUE), wt = POND, pct = "row", color = TRUE, na = "drop", 
    levels = "first", ref = 1
) |> 
  tab_export("kable")





# Excel exports : 
# - "2. **The Excel colour legend's background break-words** (14c). You asked for "-0.2 oklch lightness".
#     Delivered exactly — but I want you to see it: the result is **faint** " : 
#   I agree, this is too faint, chroma boost is really needed ; I’m not sure what the right value would be (but respect a bit the proportionallity of original chromas, or bad idea ?) 
# - For "row_var" column, same problem than with custom html : row variable name repeated on yeach row 
#   I’ve tested the "text orientation 90° / vertical" in Excel, the result is great. Remove the column name "row_var" here, not needed : same in custom html.
# - I would want font for numbers as "DejaVu Sans", rather than "DejaVu Sans Condensed" ; 
#   anyway, DejaVu Sans should always be the fallback when the Condensed version is not on the computer / not downloaded online
# - "14d — the Excel title" : it’s good. But with several row_vars it still does "levels by ROCK, JAZZ, CLASSIQUE +8 more". Please replace "levels" with the first row_vars names. max 2 names by default.
#   More generally, the right title is actually the opposite order : here " ROCK, JAZZ... by DIPLOME, CRITREVENU...", with dependent variables first (col_vars with pct = "row"), explanatory variables second (row_vars with pct = "row").

# md exports : 
tab_md_css()
tab(pc18, all_of(rows2), c(ROCK, JAZZ, CLASSIQUE), wt = POND, pct = "row", color = TRUE, na = "drop", 
    levels = "first", ref = 1
) |> 
  tab_export("md")
# - md is rendering well with quarto/pandoc 
# - It’s not good looking with no separator between variable name and numbers.  
#   |       Pop, rock  |
#   |-----------------:|
#   | *ROCK*           |
#   |  **54%**         |
#   
#   Should be : 
#   |       Pop, rock  |
#   |-----------------:|
#   | *ROCK*           |
#   | ---------------- |
#   |  **54%**         |
#
# - Same than in html and Excel : row_var names are now repeating on each line ; display each row_vars name one time only please.
#   In markdown, bold not needed for row_vars names (or tab_vars names). Keep italics for col_vars names (current  *ROCK*)



# transpose = TRUE 
tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "row", color = TRUE, na = "drop") |>
  tab_export()
tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "row", color = TRUE, na = "drop") |>
  tab_export(transpose = TRUE)
# Not working at all, it need to be thoroughly redesigned 
#  - all numeric variables cells are colors, when at lot shouldn’t be. 
#    Colors must be calculated first from the not-transposed vctrs fields, then the actual
#     transposition been done not on vctrs fields.
#  - no row_var column so no horizontal borders between different row vars, 
#     current first column name is "CONCERTS" (this one should be "levels" and "second"), 
#  - `n` row is at the end : should be just after total, and numeric variables after both
#  - obviously, many total column appears with `Total_DIPLOM` suffixes : it will be fixed with the new feature below, if it’s possible to only keep one total row with several row_vars
#    but even without this, the `_DIPLOM` is a sign the transposition is done with the vctrs field : it NEEDS to be done after, on the Excel or html writing, in order to fix the "column-level attributes breaks when they are transposed to rows because only one value can be kept" problem.



# new feature : only one Total column with several row_vars ? Need to be studied carefully !
# - With several row_vars, I would want to only keep the last Total row (like in columns), but I want to be sure : 
#   - Can it be done without breaking current formattings (but I think row_var column can be used for that) ?
#   - Can it be done without problems with reference rows when it’s the total ? But precisely, in the case the reference row is the total, tot_n would be in vctrs field, or is it precisely the wrong total (I think it is : with pct="row", percentages are calculated from Total col, but reference for comparison is of course Total row) ?
#   - A table with tab_vars should keep the current behaviour : several sub-total lines, but here they are useful because they do not repeat/duplicate the same thing.
#   - It points to a more thorough problem : the framework was never designed for several row_vars at the first place, but now it has become the standard behaviour and we should maybe redesign this to work better in the first place. 



### Other answers to your questions and flagged problems ----
# - "**`color_type = "bg"` is still vestigial** (flagged back in Phase 5): it picks the TEXT channel's
#   palette family only; the CHANNEL decides font-vs-fill." : yes, sure, deprecate.


tab(pc18, all_of(rows2), c(ROCK, JAZZ, CLASSIQUE), wt = POND, pct = "row", color = TRUE, na = "drop", 
    levels = "first", ref = 1
) |> 
  tab_kable(theme = "auto")
# **The VS Code / Positron webview hooks** (`body.vscode-dark`, `data-vscode-theme-kind`) for
#     `theme = "auto"` in the Viewer. The roadmap itself says to verify the DOM FIRST, and it is right:
#     R html usually lands in an **iframe**, while the class sits on the OUTER webview body — the hook
#     may never match, and shipping a selector that cannot fire is worse than not shipping it. Also
#     [vscode#176698](https://github.com/microsoft/vscode/issues/176698) reports `prefers-color-scheme`
#     resolving *light* under a dark theme in webviews, which would mean `auto`'s base layer is wrong in
#     the Viewer regardless. **What I need**: open a `tab_kable(theme = "auto")` in the Positron Viewer,
#     right-click → Inspect, and tell me whether the table's own `<body>` (not the outer one) carries
#     `vscode-dark`.
# - Result is always Dark, when I’m in Dark mode AND when I’m in Light mode in Positron, regarless.
# - See the detailed whole Positron devtools "Inspect" html in `dev/review_manual/Positron_Inspect_kable_theme_auto.html` : analyse it if needed.
# - In Positron Viewer pane, I still have a background problem in Viewer pane : 
#    the table itself have dark background, but the empty space below the table is plain white, which is awful, 
#    is there a way to turn it to the dark color ? 
# - In Positron Viewer pane, Dark mode, the text color actually change the borders colors (even greyed out cells), which is awful : 
#    I want all borders with the same color, here the same shade of white than normal text color.

tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "col", color = TRUE, na = "drop") |> 
  tab_kable(theme = "auto")
# **`pct = "col"` compactness** and the `min-width:10em` / `5.5em` : compactness is ok. 




