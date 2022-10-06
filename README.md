# Dreger_2022

This repository contains data of alfalfa quality Norberg population.

Here we are testing multiple traits (analytes e.g., lignin) to determine if:

- FD have an effect on those analytes.
- Assess if there was a quantitative relationship between lignin and FD classes.
- If there was interaction of FD and cutting time?
- If there were differences in lignin among genotypes of the same FD class.

Each analyte was determined for each plot with 3, 4 and 5 cuts per year.

Dreger's thesis: analysis of correlation between FD and quality traits collected Norberg population. Alfalfa population consists of seven genotypes with six different FD classes:

|    gen    | 44 | 61 | 104 | 112 | 144 | 201 | 202 |
|:---------:|:--:|:--:|:---:|:---:|:---:|:---:|:---:|
|     FD    |  4 |  6 |  5  |  1  |  3  |  2  |  3  |
| ID_2018_1 |  1 |  1 |  1  |  1  |  1  |  1  |  1  |
| ID_2018_2 |  1 |  1 |  1  |  1  |  1  |  1  |  1  |
| ID_2018_3 |  1 |  1 |  0  |  0  |  0  |  1  |  1  |

Because of the augmented design experiment, we were expecting 11 measurements in 201 and 201 by env (loc:yeat:cut). There are missing genotypes (in addition to 104 and 112):

|    env    | gen | count |
|:---------:|:---:|:-----:|
| ID_2018_3 | 202 |   9   |
| WA_2019_2 | 201 |   10  |
| ID_2018_3 | 201 |   3   |
| OR_2018_2 | 201 |   12  |
| OR_2018_3 | 201 |   10  |

OR_2018_2 with 12 measurements of 201 and OR_2018_3 with 11 measurements of 201 were fixed trasnferring one value from OR_2018_2 to OR_2018_3. Multi-environment trial dataset is composed by three locations (loc), two years, and 3, 4, or 5 cuts (samplings or harvests):

| factor | count |
|:------:|:-----:|
|   cut  | 3,4,5 |
|  year  |   2   |
|   loc  |   3   |
|   env  |   22  |

Cuts (harvest) depended on the year and location. In 2018 the field was established and only three cuts were done. Harvest interval by year and location. Three-cut system was used in 2018 in all locations. Four-cut system was used in ID 2019 and in OR 2019. Five-cut system was used in WA in 2019.

| Year  |    ID   |    OR   |     WA    |
|:-----:|:-------:|:-------:|:---------:|
|  2018 | 1,2,3,− | 1,2,3,− | 1,2,3,−,− |
|  2019 | 1,2,3,4 | 1,2,3,4 | 1,2,3,4,5 |

The field was established in 2018 and the cut number between years could be not accure as months:

|         |     |  2018  | 2018 | 2018 | 2018 | 2018 | 2018 | 2018 | 2019 | 2019 | 2019 | 2019 | 2019 | 2019 | 2020 |
|:-------:|:---:|:------:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|
|         |     |  Spri  | Spri | Summ | Summ | Summ | Fall | Wint | Spri | Summ | Summ | Summ | Fall | Wint | Spri |
|         | LOC |   Apr  |  May |  Jun |  Jul |  Aug |  Sep |   -  |  May |  Jun |  Jul |  Aug |  Sep |   -  |  May |
| Ori_CUT |  ID | Sowing |   -  |   -  |   1  |   2  |   3  |   -  |   1  |   -  |   2  |   3  |   4  |   -  |   1  |
|         |  OR | Sowing |   -  |   -  |   1  |   2  |   3  |   -  |   1  |   -  |   2  |   3  |   4  |   -  |   1  |
|         |  WA | Sowing |   -  |   -  |   1  |   2  |   3  |   -  |   1  |   2  |   3  |   4  |   5  |   -  |   1  |
| New_CUT |  ID | Sowing |   -  |   -  |   3  |   4  |   5  |   -  |   1  |   -  |   3  |   4  |   5  |   -  |   1  |
|         |  OR | Sowing |   -  |   -  |   3  |   4  |   5  |   -  |   1  |   -  |   3  |   4  |   5  |   -  |   1  |
|         |  WA | Sowing |   -  |   -  |   3  |   4  |   5  |   -  |   1  |   2  |   3  |   4  |   5  |   -  |   1  |

17 quality traits were collected (analyzed). There are environments where some traits were not collected:

| num |   Trait  | 2018_1 | 2018_2 | 2018_3 | 2019_1 | 2019_2 | 2019_3 | 2019_4 | 2019_5 |
|:---:|:--------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|
|  1  |   Cprot  |    1   |    1   |    1   |    1   |    1   |    1   |    1   |    1   |
|  2  |    ADF   |    1   |    1   |    1   |    1   |    1   |    1   |    1   |    1   |
|  3  |   aNDF   |    1   |    1   |    1   |    1   |    1   |    1   |    1   |    1   |
|  4  |    Ash   |    1   |    1   |    1   |    1   |    1   |    1   |    1   |    1   |
|  5  |    Ca    |    1   |    1   |    1   |    1   |    1   |    1   |    1   |    1   |
|  6  |     P    |    1   |    1   |    1   |    1   |    1   |    1   |    1   |    1   |
|  7  |     K    |    1   |    1   |    1   |    1   |    1   |    1   |    1   |    1   |
|  8  |    Mg    |    1   |    1   |    1   |    1   |    1   |    1   |    1   |    1   |
|  9  |    Fat   |    1   |    1   |    1   |    1   |    1   |    1   |    1   |    1   |
|  10 |  Lignin  |    1   |    1   |    1   |    1   |    1   |    1   |    1   |    1   |
|  11 |  Starch  |    1   |    0   |    0   |    1   |    1   |    1   |    1   |    1   |
|  12 |   Su_ES  |    1   |    0   |    0   |    1   |    1   |    1   |    1   |    1   |
|  13 |   Su_WS  |    1   |    0   |    0   |    1   |    1   |    1   |    1   |    1   |
|  14 | IVTDMD48 |    0   |    1   |    1   |    0   |    1   |    1   |    1   |    1   |
|  15 | IVTDMD30 |    0   |    1   |    1   |    0   |    1   |    1   |    1   |    1   |
|  16 |  dNDF30  |    0   |    1   |    1   |    0   |    1   |    1   |    1   |    1   |
|  17 |  dNDF48  |    0   |    1   |    1   |    0   |    1   |    1   |    1   |    1   |

## Flowchart and data analysis process

- Generate BLUP values ST0: single-trial analysis.

- Generate BLUP values ST1: MET analysis.

- Basic statistical metrics.

- ANOVA to check sources of variation.

Notes: tables generated by: <https://www.tablesgenerator.com/markdown_tables>

## Random and Mixed Effects Models

We are defining different factors as fixed or random:

| factor | levels | t_levels | factor |
|:------:|:------:|:--------:|:------:|
|   gen  |    7   |     7    |  Fixed |
|   FD   |    6   |     6    |  Fixed |
|   cut  |  3,4,5 |     5    | Random |
|  year  |    2   |     2    |  Fixed |
|   loc  |    3   |     3    |  Fixed |
|   env  |   22   |    22    |    -   |

The model used to obtain p-values in ANOVA.

This is the Mixed Effects Model used to calculate the ANOVA table:

``mod1 <- lmer(predicted.value ~ gen *loc* year + (1|cut) + (1|loc:year:cut), data = data)``

where $gen*loc*year$ were considered fixed effects. cut and the interaction of cut, year, and loc were considered random effects.

- I tested multiple models and chose the best model based on the lowest AIC. Most time over-parametrized the model will affect its accuracy of the model. This is our case when we include more combinations (Year, Loc, Cut, FD, Gen)
- FD and gen are redundant and will affect the model. Therefore it is better just to use Gen.
- I compared Year as a random or fixed effect and the best model was setting Year as a fixed effect.
- The p-values and the SS using a mixed model only retrieve values for the fixed effects.

## Fransen model 2

According to Fransen (09-15-22) the model requires some changes:

- Fall Dormancy must be included as a source of analysis and not just mixed with genotype.
- Year should be used as a random effect rather than fixed.

New model (mod2):
`mod2 <- lmer(predicted.value ~ FD * gen * loc + (1|year) + (1|cut)  + (1|year:cut) + (1|loc:year) + (1|FD:cut) + (1|gen:cut) + (1|loc:year:cut) + (1|FD:loc:cut) + (1|gen:loc:cut), data = data1)`

| factor | levels | t_levels | factor |
|:------:|:------:|:--------:|:------:|
|   gen  |    7   |     7    |  Fixed |
|   FD   |    6   |     6    |  Fixed |
|   cut  |  3,4,5 |     5    | Random |
|  year  |    2   |     2    | Random |
|   loc  |    3   |     3    |  Fixed |
|   env  |   22   |    22    |    -   |

They requiered a model including all posible interactions and retrieving residuals and all DF in a similar way as GLM:
`mod6 <- lm(predicted.value ~ FD + loc + year + cut + FD:loc + FD:year + FD:cut + FD:loc:year + FD:loc:cut + FD:year:cut + FD:loc:year:cut, data = data1)`
`anova(mod6)`

But using GLMM:

`mod3 <- lmer(predicted.value ~ FD + loc + FD:loc+ (1|year) + (1|cut)  + (1|year:cut) + (1|FD:year) + (1|loc:year) + (1|FD:cut) + (1|loc:cut) + (1|FD:loc:year) + (1|FD:loc:cut) + (1|loc:year:cut) + (1|FD:loc:year:cut), data = data1)`
`anova(mod3)`

## Solved: ranova: ANOVA-Like Table for Random-Effects

`ranova` compute an ANOVA-like table with tests of random-effect terms in the model.

`Df` degrees of freedom for the likelihood ratio test: the difference in number of model parameters.
`Pr(>Chisq)` is the p-value.

We can obtain the p-values of random and fixed terms.

In statistical analysis, the F-distribution assessment is used to analysis variance in a sample group.

The **denominator degrees of freedom** is the bottom portion of the F distribution ratio or degrees of freedom error: number of samples tested - number of samples groups.

Type III Analysis of Variance Table with Satterthwaite's method:

|        | Sum Sq  | Mean Sq | Num DF | Den DF  | F value | Pr(>F) |
|--------|---------|---------|--------|---------|---------|--------|
| FD     | 2.52053 | 0.50411 | 5      | 10.6645 | 1.8537  | 0.1851 |
| loc    | 0.17697 | 0.08848 | 2      | 2.7647  | 0.3254  | 0.7466 |
| FD:loc | 2.05429 | 0.20543 | 10     | 11.6861 | 0.7554  | 0.667  |

ANOVA-like table for random-effects: Single term deletions:

|                      | npar | logLik  | AIC    | LRT     | Df | Pr(>Chisq) |     |
|----------------------|------|---------|--------|---------|----|------------|-----|
| \<none>\             | 30   | -185.66 | 431.31 |         |    |            |     |
| (1\|year)            | 29   | -185.66 | 429.31 | 0       | 1  | 0.999449   |     |
| (1\|cut)             | 29   | -185.71 | 429.41 | 0.104   | 1  | 0.747134   |     |
| (1\|year:cut)        | 29   | -188.33 | 434.66 | 5.35    | 1  | 0.020718   | *   |
| (1\|FD:year)         | 29   | -185.96 | 429.91 | 0.6     | 1  | 0.438668   |     |
| (1\|loc:year)        | 29   | -188.7  | 435.41 | 6.097   | 1  | 0.013544   | *   |
| (1\|FD:cut)          | 29   | -189.44 | 436.89 | 7.58    | 1  | 0.005903   | **  |
| (1\|loc:cut)         | 29   | -185.66 | 429.31 | 0       | 1  | 1          |     |
| (1\|FD:loc:year)     | 29   | -186    | 430.01 | 0.698   | 1  | 0.403551   |     |
| (1\|FD:loc:cut)      | 29   | -185.66 | 429.31 | 0       | 1  | 1          |     |
| (1\|loc:year:cut)    | 29   | -246.4  | 550.8  | 121.493 | 1  | < 2.2e-16  | *** |
| (1\|FD:loc:year:cut) | 29   | -185.66 | 429.31 | 0       | 1  | 1          |     |


FD and genotype are nested factors, two separated ANOVA can be performed: One for FD class, and other for genotype.

Time of cutting (Cut) was taking as split factor.

When F test was significant (p < 0.05) Duncan's multipe range test.

Lignin comparisons among differnt FD classes: 


---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Confidence level: 95%
  Degrees of freedom method: Kenward-Roger 