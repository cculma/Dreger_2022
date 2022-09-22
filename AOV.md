# Statistical Modelling

Statistical modelling is the process of applying statistical analysis to dataset. A statistical model is a mathematical representation of observed data.

How we can generalize the relationship between two (or more) variables to understand the true data. E.g., the relationship between X and Y by a line and sometimes you can extrapolate your data to predict other variable.

## Linear Modelling

Lm is to draw a straigth line between the independent (exporatory) and dependent (response) variable. Linear modeling follows the equation:

$$
y = \beta_{0} + \beta_{1}x + \varepsilon
$$

where $y$ is the dependent (response) variable and $x$ is the independent (predictor) variable. The random variable $\varepsilon$ is the error term in the model. In this context, error does not mean mistake but is a statistical term representing random fluctuations, measurement errors, or the effect of factors outside of our control.

Non linear model: sometimes the response is not linear, so you need to use non linear models e.g., toxicity or sudden changes in something (chemical tritation).

| Regression Model | Equation                  |
|------------------|---------------------------|
| Simple lineal    | $Y = a + bX$              |
| Quadratic        | $Y = a + bX^2$            |
| Logaritmic       | $Y = a + b \log X$        |
| Exponential      | $Y = ae^{bX}, e = 2.7183$ |

There are linear models with multiple $x$. A multiple linear regression model shows relationship between the dependent variable and multiple (two or more) independent variables.

$$
y = \beta_{0} + \beta_{1}x_{1} + \beta_{2}x_{2} + \beta_{p}x_{p} + \varepsilon
$$

where $y$ is the response (dependent) variable, $\beta_0$ to $\beta_0p$ are the coefficients, $x_1$ to $x_p$ are the predictor (independent or explanatory) variable and $\varepsilon$ is the error term in the model, noise or random error. The response can be ploted as an hyperplane.

## General Linear Model

GLM is a multivariate lm where one or few of variate is nonlinear regression. Example: Do age, gender, ethnicity, and going out to eat frequently all affect cholesterol levels?

- Dependent variable: cholesterol level.
- Independent variables:
  - Age (years).
  - Gender (M/F).
  - Ethnicity (W,A,L).
  - Freq of going out to eat (> 5 times/week or < 5 times/week).

## Linear Mixed Model

LMM or mixed-effects models are called mixed because thay simultaneously model fixed and random effects (factors?). Where the fixed effects represent population-level (i.e., average) effects that should persist across experiments and random effects model the extent to which these trends vary across levels of some grouping factor (e.g., participants or items). Fixed effects model average trends.

LMM can also help us avoid the statistical sin of pseudoreplication, a source of error in statistical inference where we treat the data as being independent when it is not.

Pseudoreplication leads to us exaggerating the size of the sample and thus the degrees of freedom and p-value, which can lead to erroneously concluding statistical significance tht does not actially exist (i.e. Type I error).

Pseudoreplication typically occurs either in an observational study with a hierarchical structure or a designed experiment with different spatial and/or temporal scales.

## AOV

The AOV table summarizes information about the sources of variation in the data.
SS represent variation present in the data.
DF, are associated with each SS and are related in the same way.
MS is the SS divided by its associated DF.
The ratio MS of the model / MS for error is an F statistic.
The F statistic test if the null Ho (none of the explanatory variables has any effect).
If Ho is true the computed F statistic (F Stat) is 18.8606.

You can use the p-value (Pr > F) to determine whether to reject the Ho.
The p-value (probability or observed significance level) is the pr of obtaining, by chance alone, an F statistic greater than the computed F statistic when the Ho is T.
Smaller p-value is stronger the evidence againts the Ho. Small p-value you can reject the Ho and conclude that at least one of the explanatory variables has an effect on Y (response variable).
