# Summary Statistics of a Set of Independent Variables by a Categorical Variable

Summarize one or more variables (x) by a categorical variable (y).
Variables on the right side of the formula, i.e. independent variables,
are summarized by the levels of a categorical variable on the left of
the formula. Optionally, an appropriate test is performed to test the
distribution of the independent variables across the levels of the
categorical variable.

## Usage

``` r
tableby(
  formula,
  data,
  na.action,
  subset = NULL,
  weights = NULL,
  strata,
  control = NULL,
  ...
)
```

## Arguments

- formula:

  an object of class [`formula`](https://rdrr.io/r/stats/formula.html);
  a symbolic description of the variables to be summarized by the group,
  or categorical variable, of interest. See "Details" for more
  information. To only view overall summary statistics, a one-sided
  formula can be used.

- data:

  an optional data frame, list or environment (or object coercible by
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html) to a data
  frame) containing the variables in the model. If not found in data,
  the variables are taken from `environment(formula)`, typically the
  environment from which the function is called.

- na.action:

  a function which indicates what should happen when the data contain
  `NA`s. The default is `na.tableby(TRUE)` if there is a by-variable,
  and `na.tableby(FALSE)` if there is not. This schema thus includes
  observations with `NA`s in x variables, but removes those with `NA` in
  the categorical group variable and strata (if used).

- subset:

  an optional vector specifying a subset of observations (rows of data)
  to be used in the results. Works as vector of logicals or an index.

- weights:

  a vector of weights. Using weights will disable statistical tests.

- strata:

  a vector of strata to separate summaries by an additional group.

- control:

  control parameters to handle optional settings within `tableby`. Two
  aspects of `tableby` are controlled with these: test options of RHS
  variables across levels of the categorical grouping variable, and x
  variable summaries within the grouping variable. Arguments for
  `tableby.control` can be passed to `tableby` via the `...` argument,
  but if a control object and `...` arguments are both supplied, the
  latter are used. See
  [`tableby.control`](https://mayoverse.github.io/arsenal/reference/tableby.control.md)
  for more details.

- ...:

  additional arguments to be passed to internal `tableby` functions or
  [`tableby.control`](https://mayoverse.github.io/arsenal/reference/tableby.control.md).

## Value

An object with class `c("tableby", "arsenal_table")`

## Details

The group variable (if any) is categorical, which could be an integer,
character, factor, or ordered factor. `tableby` makes a simple summary
of the counts within the k-levels of the independent variables on the
right side of the formula. Note that unused levels are dropped.

The `data` argument allows data.frames with label attributes for the
columns, and those labels will be used in the summary methods for the
`tableby` class.

The independent variables are a mixture of types: categorical
(discrete), numeric (continuous), and time to event (survival). These
variables are split by the levels of the group variable (if any), then
summarized within those levels, specific to the variable type. A
statistical test is performed to compare the distribution of the
independent variables across the levels of the grouping variable.

The tests differ by the independent variable type, but can be specified
explicitly in the formula statement or in the control function. These
tests are accepted:

- `anova`: analysis of variance test; the default test for continuous
  variables. When LHS variable has two levels, equivalent to two-sample
  t-test.

- `kwt`: Kruskal-Wallis Rank Test, optional test for continuous
  variables. When LHS variable has two levels, equivalent to Wilcoxon
  test.

- `wt`: An explicit Wilcoxon test.

- `medtest`: A median test.

- `chisq`: chi-square goodness of fit test for equal counts of a
  categorical variable across categories; the default for categorical or
  factor variables

- `fe`: Fisher's exact test for categorical variables

- `trend`: trend test for equal distribution of an ordered variable
  across a categorical variable; the default for ordered factor
  variables

- `logrank`: log-rank, the default for time-to-event variables

- `notest`: no test is performed.

To perform a mixture of asymptotic and rank-based tests on two different
continuous variables, an example formula is:
`formula = group ~ anova(age) + kwt(height)`. The test settings in
`tableby.control` apply to all independent variables of a given type.

The summary statistics reported for each independent variable within the
group variable can be set in
[`tableby.control`](https://mayoverse.github.io/arsenal/reference/tableby.control.md).

Finally, multiple by-variables can be set using
[`list()`](https://rdrr.io/r/base/list.html). See the examples for more
details.

## See also

[`arsenal_table`](https://mayoverse.github.io/arsenal/reference/arsenal_table.md),
[`anova`](https://rdrr.io/r/stats/anova.html),
[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html),
[`tableby.control`](https://mayoverse.github.io/arsenal/reference/tableby.control.md),
[`summary.tableby`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md),
[`tableby.internal`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md),
[`formulize`](https://mayoverse.github.io/arsenal/reference/formulize.md),
[`selectall`](https://mayoverse.github.io/arsenal/reference/selectall.md)

## Author

Jason Sinnwell, Beth Atkinson, Gregory Dougherty, and Ethan Heinzen,
adapted from SAS Macros written by Paul Novotny and Ryan Lennon

## Examples

``` r
data(mockstudy)
tab1 <- tableby(arm ~ sex + age, data=mockstudy)
summary(tab1, text=TRUE)
#> 
#> 
#> |             | A: IFL (N=428)  | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499)  | p value|
#> |:------------|:---------------:|:-----------------:|:---------------:|:---------------:|-------:|
#> |sex          |                 |                   |                 |                 |   0.190|
#> |-  Male      |   277 (64.7%)   |    411 (59.5%)    |   228 (60.0%)   |   916 (61.1%)   |        |
#> |-  Female    |   151 (35.3%)   |    280 (40.5%)    |   152 (40.0%)   |   583 (38.9%)   |        |
#> |Age in Years |                 |                   |                 |                 |   0.614|
#> |-  Mean (SD) | 59.673 (11.365) |  60.301 (11.632)  | 59.763 (11.499) | 59.985 (11.519) |        |
#> |-  Range     | 27.000 - 88.000 |  19.000 - 88.000  | 26.000 - 85.000 | 19.000 - 88.000 |        |
#> 

mylabels <- list(sex = "SEX", age ="Age, yrs")
summary(tab1, labelTranslations = mylabels, text=TRUE)
#> 
#> 
#> |             | A: IFL (N=428)  | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499)  | p value|
#> |:------------|:---------------:|:-----------------:|:---------------:|:---------------:|-------:|
#> |SEX          |                 |                   |                 |                 |   0.190|
#> |-  Male      |   277 (64.7%)   |    411 (59.5%)    |   228 (60.0%)   |   916 (61.1%)   |        |
#> |-  Female    |   151 (35.3%)   |    280 (40.5%)    |   152 (40.0%)   |   583 (38.9%)   |        |
#> |Age, yrs     |                 |                   |                 |                 |   0.614|
#> |-  Mean (SD) | 59.673 (11.365) |  60.301 (11.632)  | 59.763 (11.499) | 59.985 (11.519) |        |
#> |-  Range     | 27.000 - 88.000 |  19.000 - 88.000  | 26.000 - 85.000 | 19.000 - 88.000 |        |
#> 

tab3 <- tableby(arm ~ sex + age, data=mockstudy, test=FALSE, total=FALSE,
                numeric.stats=c("median","q1q3"), numeric.test="kwt")
summary(tab3, text=TRUE)
#> 
#> 
#> |             | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) |
#> |:------------|:--------------:|:-----------------:|:---------------:|
#> |sex          |                |                   |                 |
#> |-  Male      |  277 (64.7%)   |    411 (59.5%)    |   228 (60.0%)   |
#> |-  Female    |  151 (35.3%)   |    280 (40.5%)    |   152 (40.0%)   |
#> |Age in Years |                |                   |                 |
#> |-  Median    |     61.000     |      61.000       |     61.000      |
#> |-  Q1, Q3    | 53.000, 68.000 |  52.000, 69.000   | 52.000, 68.000  |
#> 

# multiple LHS
summary(tableby(list(arm, sex) ~ age, data = mockstudy, strata = ps), text = TRUE)
#> 
#> 
#> |ps |             | A: IFL (N=359)  | F: FOLFOX (N=550) | G: IROX (N=324) | Total (N=1233)  | p value |
#> |:--|:------------|:---------------:|:-----------------:|:---------------:|:---------------:|:-------:|
#> |0  |Age in Years |                 |                   |                 |                 |  0.740  |
#> |   |-  Mean (SD) | 60.101 (10.948) |  60.173 (11.096)  | 59.361 (11.904) | 59.935 (11.261) |         |
#> |   |-  Range     | 27.000 - 81.000 |  22.000 - 82.000  | 26.000 - 85.000 | 22.000 - 85.000 |         |
#> |1  |Age in Years |                 |                   |                 |                 |  0.582  |
#> |   |-  Mean (SD) | 60.579 (12.026) |  61.342 (11.918)  | 60.081 (11.037) | 60.800 (11.721) |         |
#> |   |-  Range     | 28.000 - 88.000 |  26.000 - 88.000  | 28.000 - 84.000 | 26.000 - 88.000 |         |
#> |2  |Age in Years |                 |                   |                 |                 |  0.067  |
#> |   |-  Mean (SD) | 56.842 (13.226) |  63.138 (9.303)   | 55.737 (13.523) | 59.254 (12.090) |         |
#> |   |-  Range     | 34.000 - 75.000 |  44.000 - 80.000  | 28.000 - 76.000 | 28.000 - 80.000 |         |
#> 
#> 
#> |ps |             |  Male (N=754)   | Female (N=479)  | Total (N=1233)  | p value |
#> |:--|:------------|:---------------:|:---------------:|:---------------:|:-------:|
#> |0  |Age in Years |                 |                 |                 |  0.614  |
#> |   |-  Mean (SD) | 59.757 (11.031) | 60.221 (11.637) | 59.935 (11.261) |         |
#> |   |-  Range     | 27.000 - 85.000 | 22.000 - 82.000 | 22.000 - 85.000 |         |
#> |1  |Age in Years |                 |                 |                 |  0.045  |
#> |   |-  Mean (SD) | 61.599 (11.748) | 59.500 (11.588) | 60.800 (11.721) |         |
#> |   |-  Range     | 26.000 - 88.000 | 28.000 - 88.000 | 26.000 - 88.000 |         |
#> |2  |Age in Years |                 |                 |                 |  0.805  |
#> |   |-  Mean (SD) | 59.618 (11.703) | 58.879 (12.646) | 59.254 (12.090) |         |
#> |   |-  Range     | 34.000 - 80.000 | 28.000 - 77.000 | 28.000 - 80.000 |         |
#> 

tab.test <- tableby(arm ~ kwt(age) + anova(bmi) + kwt(ast), data=mockstudy)
tests(tab.test)
#>           Group Variable    p.value                       Method
#> 1 Treatment Arm      age 0.63906143 Kruskal-Wallis rank sum test
#> 2 Treatment Arm      bmi 0.89165522           Linear Model ANOVA
#> 3 Treatment Arm      ast 0.03902803 Kruskal-Wallis rank sum test
```
