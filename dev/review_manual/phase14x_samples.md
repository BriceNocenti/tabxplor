# Phase 14x samples

## Item 3+5 -- grey_non_signif legend (generalized first threshold)

### pct difference (crosstable)
- terse:    difference (Total): -30 -20 -10 -5 +5 +10 +20 +30 [grey: non-significant or under ±5 points] 
- prose:    Shades of blue: cells ≥ the Total row +5; +10; +20; +30 points. Shades of yellow to red: cells ≤ the Total row -5; -10; -20; -30 points. Coloured: significantly different from the Total row (Newcombe score interval, 95% confidence), by at least the first colour threshold. Uncoloured: either not significant, or a difference under ±5 points. 
- FR terse: différence (Total) : -30 -20 -10 -5 +5 +10 +20 +30 [gris : non significatif ou sous ±5 points] 

### ratio (mean)
- terse:    ratio (White): ÷4 ÷2 ÷1.5 ×1.15 ×1.5 ×2 ×4 [grey: non-significant or under ×1.15] 

### standardized mean difference
- terse:    standardized difference (Total): -0.8 -0.5 -0.2 +0.2 +0.5 +0.8 [grey: non-significant or under ±0.2 SD] 

## Item 4 -- mean/sd joiner is a figure space in md (digit-width, aligns)

::: {.tabxplor-tab}
| race    |            mean (sd)  |
|:--------|----------------------:|
|         | *tvhours*             |
|         |                       |
|**White**|  **2.8** (σ2.3)       |
| Black   |     [4.2 (σ3.5)]{.p2} |
| Other   |      2.8 (σ2.4)       |
| Total   |      3.0 (σ2.6)       |

Nuances de bleu: cells ≥ the White row **[×1.15]{.p1}**; **[×1.5]{.p2}**; **[×2]{.p3}**; **[×4]{.p4}**. Nuances du jaune au rouge: cells ≤ the White row **[÷1.5]{.m1}**; **[÷2]{.m3}**; **[÷4]{.m4}**.
::: 

## Item 6 -- levels='first' NA handling (2-level col_var, na='keep')

NA column discarded (was kept before this fix), NA still counted in the base:

| marital       | Even  |            Total  |
|:--------------|------:|------------------:|
|               | *sex2*|                   |
| No answer     |  29%  |  100% (n=    17)  |
| Never married |  46%  |  100% (n= 5 416)  |
| Separated     |  46%  |  100% (n=   743)  |
| Divorced      |  46%  |  100% (n= 3 383)  |
| Widowed       |  44%  |  100% (n= 1 807)  |
| Married       |  47%  |  100% (n=10 117)  |
|**Total**      |**46%**|**100%** (n=21 483)| 
