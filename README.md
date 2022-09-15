# Dreger_2022

This repository contains data of alfalfa quality Norberg population.
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
