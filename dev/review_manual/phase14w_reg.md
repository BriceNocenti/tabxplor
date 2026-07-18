# Phase 14w samples

## Binomial OR + empirical

::: {.tabxplor-tab}
|           | levels               |         Emp. %  |         Emp. OR  |        Model OR  |
|:----------|:---------------------|----------------:|-----------------:|-----------------:|
|           |                      | *married: 00-Not*|                  |                  |
|           |                      |                 |                  |                  |
| Constant  | Reference population |                 |                  |    1.20          |
|           |                      |                 |                  |                  |
| race      |**Other**             |  **52%   **     |     **1   **     |     **1   **     |
|           | Black                |   [72%***]{.p2} |   [2.36***]{.p3} |   [2.36***]{.p3} |
|           | White                |    49%***       |  1/1.13***       |  1/1.11**        |
|           |                      |                 |                  |                  |
| rincome   |**No answer**         |  **56%   **     |     **1   **     |     **1   **     |
|           | Don't know           |    50%          |  1/1.28          |  1/1.27          |
|           | Refused              |   [41%***]{.m2} | [1/1.84***]{.m1} | [1/1.81***]{.m1} |
|           | $25000 or more       |   [45%***]{.m2} | [1/1.58***]{.m1} |  1/1.50***       |
|           | $20000 - 24999       |    56%          |  1/1.02          |  1/1.00          |
|           | $15000 - 19999       |    58%          |    1.06          |    1.08          |
|           | $10000 - 14999       |    59%          |    1.12          |    1.13          |
|           | $8000 to 9999        |    63%          |    1.30          |    1.34          |
|           | $7000 to 7999        |    65%*         |    1.44*         |    1.50*         |
|           | $6000 to 6999        |    59%          |    1.12          |    1.15          |
|           | $5000 to 5999        |    60%          |    1.16          |    1.18          |
|           | $4000 to 4999        |    62%          |    1.26          |    1.29          |
|           | $3000 to 3999        |   [66%** ]{.p2} |   [1.53** ]{.p2} |   [1.51** ]{.p2} |
|           | $1000 to 2999        |   [67%** ]{.p2} |   [1.57** ]{.p2} |   [1.59** ]{.p2} |
|           | Lt $1000             |    63%          |    1.34          |    1.36          |
|           | Not applicable       |    57%          |    1.05          |    1.09          |
|           |                      |                 |                  |                  |
| Model fit |**N**                 |                 |                  |   **21 483**     |
|           |**LR vs null**        |                 |                  |   **<0.01%**     |
|           |**McFadden R2**       |                 |                  |    **0.033**     |
|           |**AIC**               |                 |                  |   **28 766**     |
|           |**BIC**               |                 |                  |   **28 910**     |

: Logistic regression: married by race, rincome

Model: logistic regression; odds ratios (vs the reference category).
Emp. % — Shades of blue: cells ≥ **[+5]{.p1}**; **[+10]{.p2}**; **[+20]{.p3}**; **[+30]{.p4}** points. Shades of yellow to red: cells ≤ **[-5]{.m1}**; **[-10]{.m2}**; **[-20]{.m3}**; **[-30]{.m4}** points. Coloured: significantly different from the reference category (Wald interval, 95% confidence), by at least the first colour threshold. Uncoloured: either not significant, or too small a difference to colour.
Emp. OR — Shades of blue: OR ≥ **[1.15]{.p1}**; **[1.5]{.p2}**; **[2]{.p3}**; **[4]{.p4}**. Shades of yellow to red: OR ≤ **[1/1.5]{.m1}**; **[1/2]{.m3}**; **[1/4]{.m4}**. Coloured: significantly different from the reference category (Wald interval on the log odds-ratio, 95% confidence), by at least the first colour threshold. Uncoloured: either not significant, or too small a difference to colour.
Model OR — Shades of blue: OR ≥ **[1.15]{.p1}**; **[1.5]{.p2}**; **[2]{.p3}**; **[4]{.p4}**. Shades of yellow to red: OR ≤ **[1/1.5]{.m1}**; **[1/2]{.m3}**; **[1/4]{.m4}**. Coloured: significantly different from the reference category (Wald interval on the log odds-ratio, 95% confidence), by at least the first colour threshold. Uncoloured: either not significant, or too small a difference to colour.
:::


## Binomial AME + empirical

::: {.tabxplor-tab}
|           | levels                 |         Emp. %  |      Emp. diff  |     Model AME (model %)  |
|:----------|:-----------------------|----------------:|----------------:|-------------------------:|
|           |                        | *married: 00-Not*|                 |                          |
|           |                        |                 |                 |                          |
| Constant  |**Reference population**|                 |                 |                          |
|           |                        |                 |                 |                          |
| race      |**Other**               |  **52%   **     |  **+0%   **     |        **(52.4%**)       |
|           | Black                  |   [72%***]{.p2} |  [+20%***]{.p2} | [+19.6%*** (72.2%)]{.p2} |
|           | White                  |    49%***       |    -3%***       |   -2.5%**  (49.3%)       |
|           |                        |                 |                 |                          |
| rincome   |**No answer**           |  **56%   **     |  **+0%   **     |        **(56.3%**)       |
|           | Don't know             |    50%          |    -6%          |   -5.9%    (50.2%)       |
|           | Refused                |   [41%***]{.m2} |  [-15%***]{.m2} | [-14.3%*** (41.2%)]{.m2} |
|           | $25000 or more         |   [45%***]{.m2} |  [-11%***]{.m2} |  [-9.8%*** (44.9%)]{.m1} |
|           | $20000 - 24999         |    56%          |    -0%          |   -0.1%    (55.8%)       |
|           | $15000 - 19999         |    58%          |    +1%          |   +1.8%    (57.7%)       |
|           | $10000 - 14999         |    59%          |    +3%          |   +2.9%    (59.0%)       |
|           | $8000 to 9999          |    63%          |    +6%          |   +6.9%    (62.6%)       |
|           | $7000 to 7999          |    65%*         |    +9%*         |   +9.5%*   (64.9%)       |
|           | $6000 to 6999          |    59%          |    +3%          |   +3.3%    (59.1%)       |
|           | $5000 to 5999          |    60%          |    +4%          |   +3.9%    (59.9%)       |
|           | $4000 to 4999          |    62%          |    +6%          |   +6.0%    (61.9%)       |
|           | $3000 to 3999          |   [66%** ]{.p2} |  [+10%** ]{.p2} |  [+9.6%**  (66.3%)]{.p1} |
|           | $1000 to 2999          |   [67%** ]{.p2} |  [+11%** ]{.p2} | [+10.8%**  (66.8%)]{.p2} |
|           | Lt $1000               |    63%          |    +7%          |   +7.2%    (63.3%)       |
|           | Not applicable         |    57%          |    +1%          |   +2.2%    (57.5%)       |
|           |                        |                 |                 |                          |
| Model fit |**N**                   |                 |                 |           **21 483**     |
|           |**LR vs null**          |                 |                 |           **<0.01%**     |
|           |**McFadden R2**         |                 |                 |            **0.033**     |
|           |**AIC**                 |                 |                 |           **28 766**     |
|           |**BIC**                 |                 |                 |           **28 910**     |

: Logistic regression: married by race, rincome

Model: logistic regression; marginal effects on the probability scale (percentage points) (sample-averaged); each cell shows the effect vs the reference level and, in parentheses, the adjusted predicted probability.
Model AME (model %) — Shades of blue: AME ≥ **[+5]{.p1}**; **[+10]{.p2}**; **[+20]{.p3}**; **[+30]{.p4}** points. Shades of yellow to red: AME ≤ **[-5]{.m1}**; **[-10]{.m2}**; **[-20]{.m3}**; **[-30]{.m4}** points. Coloured: significantly different from the reference category (Wald interval, 95% confidence), by at least the first colour threshold. Uncoloured: either not significant, or too small a difference to colour.
Emp. %, Emp. diff — Shades of blue: cells ≥ **[+5]{.p1}**; **[+10]{.p2}**; **[+20]{.p3}**; **[+30]{.p4}** points. Shades of yellow to red: cells ≤ **[-5]{.m1}**; **[-10]{.m2}**; **[-20]{.m3}**; **[-30]{.m4}** points. Coloured: significantly different from the reference category (Wald interval, 95% confidence), by at least the first colour threshold. Uncoloured: either not significant, or too small a difference to colour.
:::


## Multinomial

::: {.tabxplor-tab}
|           | levels                 |        Dem vs Ind  |        Rep vs Ind  |
|:----------|:-----------------------|-------------------:|-------------------:|
|           |                        | *party3: OR*       |                    |
|           |                        |                    |                    |
| Constant  |**Reference population**|  **1/1.54*****     |  **1/4.28*****     |
|           |                        |                    |                    |
| race      |**Other**               |       **1   **     |       **1   **     |
|           | Black                  |     [3.28***]{.p3} |   [1/1.52***]{.m1} |
|           | White                  |      1.08          |     [3.29***]{.p3} |
|           |                        |                    |                    |
| Model fit |**N**                   |     **20 935**     |                    |
|           |**LR vs null**          |     **<0.01%**     |                    |
|           |**McFadden R2**         |      **0.047**     |                    |
|           |**AIC**                 |     **43 194**     |                    |
|           |**BIC**                 |     **43 242**     |                    |

: Multinomial logistic regression: party3 by race

Model: multinomial logistic regression; odds ratios (each category vs the reference).
Shades of blue: OR ≥ **[1.15]{.p1}**; **[1.5]{.p2}**; **[2]{.p3}**; **[4]{.p4}**. Shades of yellow to red: OR ≤ **[1/1.5]{.m1}**; **[1/2]{.m3}**; **[1/4]{.m4}**. Coloured: significantly different from the reference category (Wald interval on the log odds-ratio, 95% confidence), by at least the first colour threshold. Uncoloured: either not significant, or too small a difference to colour.
:::


## Linear

::: {.tabxplor-tab}
|           | levels               |        Model β  |
|:----------|:---------------------|----------------:|
|           |                      | *tvhours*       |
|           |                      |                 |
| Constant  | Reference population |   2.58***       |
|           |                      |                 |
| race      | Other                |      0          |
|           | Black                |  [1.31***]{.p3} |
|           | White                |  -0.03          |
|           |                      |                 |
| marital   | No answer            |      0          |
|           | Never married        |   0.22          |
|           | Separated            |   0.63          |
|           | Divorced             |   0.33          |
|           | Widowed              |   1.15          |
|           | Married              |  -0.02          |
|           |                      |                 |
| Model fit |**N**                 |  **11 337**     |
|           |**F**                 |  **<0.01%**     |
|           |**R2**                |   **0.054**     |
|           |**Adjusted R2**       |   **0.054**     |
|           |**Residual SD**       |    **2.52**     |

: Linear regression: tvhours by race, marital

Model: linear regression; coefficients (mean difference vs the reference category).
Shades of blue: β ≥ **[+0.2]{.p1}**; **[+0.5]{.p3}**; **[+0.8]{.p4}** SD. Shades of yellow to red: β ≤ **[-0.2]{.m1}**; **[-0.5]{.m3}**; **[-0.8]{.m4}** SD. Coloured: significantly different from the reference category (Wald interval, 95% confidence), by at least the first colour threshold. Uncoloured: either not significant, or too small a difference to colour.
:::


## Comparison (title carries reference + effect)

::: {.tabxplor-tab}
|           | levels                 |            demo  | |            full  |
|:----------|:-----------------------|-----------------:|-|-----------------:|
| Constant  |**Reference population**|  **1.10** **     | |  **1.20   **     |
|           |                        |                  | |                  |
| race      |**Other**               |     **1   **     | |     **1   **     |
|           | Black                  |   [2.36***]{.p3} | |   [2.36***]{.p3} |
|           | White                  |  1/1.13***       | |  1/1.11**        |
|           |                        |                  | |                  |
| rincome   | No answer              |                  | |       1          |
|           | Don't know             |                  | |  1/1.27          |
|           | Refused                |                  | | [1/1.81***]{.m1} |
|           | $25000 or more         |                  | |  1/1.50***       |
|           | $20000 - 24999         |                  | |  1/1.00          |
|           | $15000 - 19999         |                  | |    1.08          |
|           | $10000 - 14999         |                  | |    1.13          |
|           | $8000 to 9999          |                  | |    1.34          |
|           | $7000 to 7999          |                  | |    1.50*         |
|           | $6000 to 6999          |                  | |    1.15          |
|           | $5000 to 5999          |                  | |    1.18          |
|           | $4000 to 4999          |                  | |    1.29          |
|           | $3000 to 3999          |                  | |   [1.51** ]{.p2} |
|           | $1000 to 2999          |                  | |   [1.59** ]{.p2} |
|           | Lt $1000               |                  | |    1.36          |
|           | Not applicable         |                  | |    1.09          |
|           |                        |                  | |                  |
| Model fit |**N**                   |   **21 483**     | |   **21 483**     |
|           |**LR vs null**          |   **<0.01%**     | |   **<0.01%**     |
|           |**McFadden R2**         |    **0.019**     | |    **0.033**     |
|           |**AIC**                 |   **29 139**     | |   **28 766**     |
|           |**BIC**                 |   **29 163**     | |   **28 910**     |

: Logistic regressions (models comparison): married, '00-Not' (OR)

Model: logistic regression of married ('00-Not'); odds ratios (vs the reference category).
Shades of blue: OR ≥ **[1.15]{.p1}**; **[1.5]{.p2}**; **[2]{.p3}**; **[4]{.p4}**. Shades of yellow to red: OR ≤ **[1/1.5]{.m1}**; **[1/2]{.m3}**; **[1/4]{.m4}**. Coloured: significantly different from the reference category (Wald interval on the log odds-ratio, 95% confidence), by at least the first colour threshold. Uncoloured: either not significant, or too small a difference to colour.
:::
