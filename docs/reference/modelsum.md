# Fit models over each of a set of independent variables with a response variable

Fit and summarize models for each independent (x) variable with a
response variable (y), with options to adjust by variables for each
model.

## Usage

``` r
modelsum(
  formula,
  family = "gaussian",
  data,
  adjust = NULL,
  na.action = NULL,
  subset = NULL,
  weights = NULL,
  id,
  strata,
  control = NULL,
  ...
)
```

## Arguments

- formula:

  an object of class [`formula`](https://rdrr.io/r/stats/formula.html);
  a symbolic description of the variables to be modeled. See "Details"
  for more information.

- family:

  similar mechanism to [`glm`](https://rdrr.io/r/stats/glm.html), where
  the model to be fit is driven by the family. Options include:
  binomial, gaussian, survival, poisson, negbin, clog, and ordinal.
  These can be passed as a string, as a function, or as a list resulting
  from a call to one of the functions. See
  [`modelsum.family`](https://mayoverse.github.io/arsenal/reference/modelsum.family.md)
  for details on survival, ordinal, negbin, and clog families.

- data:

  an optional data.frame, list or environment (or object coercible by
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html) to a data
  frame) containing the variables in the model. If not found in `data`,
  the variables are taken from `environment(formula)`, typically the
  environment from which `modelsum` is called.

- adjust:

  an object of class [`formula`](https://rdrr.io/r/stats/formula.html)
  or a list of formulas, listing variables to adjust by in all models.
  Specify as a one-sided formula, like: `~Age+ Sex`. If a list, the
  names are used for the summary function. Unadjusted models can be
  specified as `~ 1` or as a list: `list(Unadjusted = NULL)`.

- na.action:

  a function which indicates what should happen when the data contain
  `NA`s. The default (`NULL`) is to use the defaults of
  [`lm`](https://rdrr.io/r/stats/lm.html),
  [`glm`](https://rdrr.io/r/stats/glm.html), or
  [`coxph`](https://rdrr.io/pkg/survival/man/coxph.html), depending on
  the `family` specifications.

- subset:

  an optional vector specifying a subset of observations (rows of
  `data`) to be used in the results. If `strata` is missing, this works
  as vector of logicals or an index; otherwise, it should be a logical
  vector.

- weights:

  an optional vector specifying the weights to apply to each data
  observation (rows of `data`)

- id:

  A vector to identify clusters. Only used for
  [`relrisk`](https://mayoverse.github.io/arsenal/reference/modelsum.family.md)
  at this time.

- strata:

  a vector of strata to separate model summaries by an additional group.
  Note that for families like "clog", the "usual" strata term to
  indicate subject groupings should be given in the `adjust` argument.

- control:

  control parameters to handle optional settings within `modelsum`.
  Arguments for `modelsum.control` can be passed to `modelsum` via the
  `...` argument, but if a control object and `...` arguments are both
  supplied, the latter are used. See
  [`modelsum.control`](https://mayoverse.github.io/arsenal/reference/modelsum.control.md)
  for other details.

- ...:

  additional arguments to be passed to internal `modelsum` functions.

## Value

An object with class `c("modelsum", "arsenal_table")`

## See also

[`arsenal_table`](https://mayoverse.github.io/arsenal/reference/arsenal_table.md),
[`modelsum.control`](https://mayoverse.github.io/arsenal/reference/modelsum.control.md),
[`summary.modelsum`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md),
[`modelsum.internal`](https://mayoverse.github.io/arsenal/reference/modelsum.internal.md),
[`formulize`](https://mayoverse.github.io/arsenal/reference/formulize.md)

## Author

Jason Sinnwell, Patrick Votruba, Beth Atkinson, Gregory Dougherty, and
Ethan Heinzen, adapted from SAS Macro of the same name

## Examples

``` r

data(mockstudy)

tab1 <- modelsum(bmi ~ sex + age, data = mockstudy)
summary(tab1, text = TRUE)
#> 
#> 
#> |             |estimate |std.error |p.value |adj.r.squared |Nmiss |
#> |:------------|:--------|:---------|:-------|:-------------|:-----|
#> |(Intercept)  |27.491   |0.181     |< 0.001 |0.004         |33    |
#> |sex Female   |-0.731   |0.290     |0.012   |              |      |
#> |(Intercept)  |26.424   |0.752     |< 0.001 |0.000         |33    |
#> |Age in Years |0.013    |0.012     |0.290   |              |      |
#> 

tab2 <- modelsum(alk.phos ~ arm + ps + hgb, adjust = ~ age + sex,
                 family = "gaussian", data = mockstudy)
summary(tab2, text = TRUE)
#> 
#> 
#> |                        |estimate |std.error |p.value |adj.r.squared |Nmiss |
#> |:-----------------------|:--------|:---------|:-------|:-------------|:-----|
#> |(Intercept)             |175.548  |20.587    |< 0.001 |-0.001        |266   |
#> |Treatment Arm F: FOLFOX |-13.701  |8.730     |0.117   |              |      |
#> |Treatment Arm G: IROX   |-2.245   |9.860     |0.820   |              |      |
#> |Age in Years            |-0.017   |0.319     |0.956   |              |      |
#> |sex Female              |3.016    |7.521     |0.688   |              |      |
#> |(Intercept)             |148.391  |19.585    |< 0.001 |0.045         |266   |
#> |ps                      |46.721   |5.987     |< 0.001 |              |      |
#> |Age in Years            |-0.084   |0.311     |0.787   |              |      |
#> |sex Female              |1.169    |7.343     |0.874   |              |      |
#> |(Intercept)             |336.554  |32.239    |< 0.001 |0.031         |266   |
#> |hgb                     |-13.845  |2.137     |< 0.001 |              |      |
#> |Age in Years            |0.095    |0.314     |0.763   |              |      |
#> |sex Female              |-5.980   |7.516     |0.426   |              |      |
#> 

summary(tab2, show.intercept = FALSE, text = TRUE)
#> 
#> 
#> |                        |estimate |std.error |p.value |adj.r.squared |Nmiss |
#> |:-----------------------|:--------|:---------|:-------|:-------------|:-----|
#> |Treatment Arm F: FOLFOX |-13.701  |8.730     |0.117   |-0.001        |266   |
#> |Treatment Arm G: IROX   |-2.245   |9.860     |0.820   |              |      |
#> |Age in Years            |-0.017   |0.319     |0.956   |              |      |
#> |sex Female              |3.016    |7.521     |0.688   |              |      |
#> |ps                      |46.721   |5.987     |< 0.001 |0.045         |266   |
#> |Age in Years            |-0.084   |0.311     |0.787   |              |      |
#> |sex Female              |1.169    |7.343     |0.874   |              |      |
#> |hgb                     |-13.845  |2.137     |< 0.001 |0.031         |266   |
#> |Age in Years            |0.095    |0.314     |0.763   |              |      |
#> |sex Female              |-5.980   |7.516     |0.426   |              |      |
#> 

tab2.df <- as.data.frame(tab2)

tab2.df[1:5,]
#>     y.term  y.label strata.term adjustment model         term
#> 1 alk.phos alk.phos              adjusted1     1  (Intercept)
#> 2 alk.phos alk.phos              adjusted1     1 armF: FOLFOX
#> 3 alk.phos alk.phos              adjusted1     1   armG: IROX
#> 4 alk.phos alk.phos              adjusted1     1          age
#> 5 alk.phos alk.phos              adjusted1     1    sexFemale
#>                     label term.type     estimate  std.error      p.value
#> 1             (Intercept) Intercept 175.54808014 20.5866451 4.343327e-17
#> 2 Treatment Arm F: FOLFOX      Term -13.70061548  8.7296300 1.168035e-01
#> 3   Treatment Arm G: IROX      Term  -2.24497807  9.8600363 8.199294e-01
#> 4            Age in Years  Adjuster  -0.01740894  0.3187823 9.564575e-01
#> 5              sex Female  Adjuster   3.01598411  7.5209656 6.884824e-01
#>   adj.r.squared Nmiss
#> 1 -0.0006968528   266
#> 2 -0.0006968528   266
#> 3 -0.0006968528   266
#> 4 -0.0006968528   266
#> 5 -0.0006968528   266
```
