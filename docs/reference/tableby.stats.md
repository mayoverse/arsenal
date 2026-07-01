# tableby Summary Statistics Functions

A collection of functions that will report summary statistics. To create
a custom function, consider using a function with all three arguments
and `...`. See the
[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md)
vignette for an example.

## Usage

``` r
arsenal_sum(x, na.rm = TRUE, ...)

arsenal_min(x, na.rm = TRUE, ...)

arsenal_max(x, na.rm = TRUE, ...)

arsenal_mean(x, na.rm = TRUE, weights = NULL, ...)

arsenal_sd(x, na.rm = TRUE, weights = NULL, ...)

arsenal_var(x, na.rm = TRUE, weights = NULL, ...)

meansd(x, na.rm = TRUE, weights = NULL, ...)

meanse(x, na.rm = TRUE, weights = NULL, ...)

meanpmsd(x, na.rm = TRUE, weights = NULL, ...)

meanpmse(x, na.rm = TRUE, weights = NULL, ...)

meanCI(x, na.rm = TRUE, weights = NULL, conf.level = 0.95, ...)

medianrange(x, na.rm = TRUE, weights = NULL, ...)

medianmad(x, na.rm = TRUE, weights = NULL, ...)

arsenal_median(x, na.rm = TRUE, weights = NULL, ...)

arsenal_range(x, na.rm = TRUE, ...)

gmean(x, na.rm = TRUE, weights = NULL, ...)

gsd(x, na.rm = TRUE, weights = NULL, ...)

gmeansd(x, na.rm = TRUE, weights = NULL, ...)

gmeanCI(x, na.rm = TRUE, weights = NULL, conf.level = 0.95, ...)

Nsigntest(x, na.rm = TRUE, weights = NULL, ...)

Nevents(x, na.rm = TRUE, weights = NULL, ...)

medSurv(x, na.rm = TRUE, weights = NULL, ...)

medSurvCI(
  x,
  na.rm = TRUE,
  weights = NULL,
  robust = FALSE,
  conf.type = "log",
  ...
)

medSurvQuant(x, na.rm = TRUE, weights = NULL, robust = FALSE, ...)

NeventsSurv(x, na.rm = TRUE, weights = NULL, times = 1:5, ...)

NriskSurv(x, na.rm = TRUE, weights = NULL, times = 1:5, ...)

Nrisk(x, na.rm = TRUE, weights = NULL, times = 1:5, ...)

medTime(x, na.rm = TRUE, weights = NULL, ...)

q1q3(x, na.rm = TRUE, weights = NULL, ...)

medianq1q3(x, na.rm = TRUE, weights = NULL, ...)

iqr(x, na.rm = TRUE, weights = NULL, ...)

Nmiss(x, weights = NULL, ...)

Nmisspct(x, weights = NULL, ...)

Nmiss2(x, weights = NULL, ...)

Nmisspct2(x, weights = NULL, ...)

N(x, weights = NULL, ...)

Npct(x, weights = NULL, ...)

Nrowpct(
  x,
  levels = NULL,
  by,
  by.levels = sort(unique(by)),
  weights = NULL,
  ...,
  totallab = "Total"
)

count(x, levels = NULL, na.rm = TRUE, weights = NULL, ...)

countpct(x, levels = NULL, na.rm = TRUE, weights = NULL, ...)

pct(x, levels = NULL, na.rm = TRUE, weights = NULL, ...)

countN(x, levels = NULL, na.rm = TRUE, weights = NULL, ...)

countrowpct(
  x,
  levels = NULL,
  by,
  by.levels = sort(unique(by)),
  na.rm = TRUE,
  weights = NULL,
  ...,
  totallab = "Total"
)

rowpct(
  x,
  levels = NULL,
  by,
  by.levels = sort(unique(by)),
  na.rm = TRUE,
  weights = NULL,
  ...,
  totallab = "Total"
)

countcellpct(
  x,
  levels = NULL,
  by,
  by.levels = sort(unique(by)),
  na.rm = TRUE,
  weights = NULL,
  ...,
  totallab = "Total"
)

binomCI(x, levels = NULL, na.rm = TRUE, weights = NULL, conf.level = 0.95, ...)

rowbinomCI(
  x,
  levels = NULL,
  by,
  by.levels = sort(unique(by)),
  na.rm = TRUE,
  weights = NULL,
  conf.level = 0.95,
  ...,
  totallab = "Total"
)
```

## Arguments

- x:

  Usually a vector.

- na.rm:

  Should NAs be removed?

- ...:

  Other arguments.

- weights:

  A vector of weights.

- conf.level:

  Numeric, denoting what confidence level to use for confidence
  intervals.

- robust, conf.type:

  Passed to
  `survival::`[`survfit`](https://rdrr.io/pkg/survival/man/survfit.html).

- times:

  A vector of times to use for survival summaries.

- levels:

  A vector of levels that character `x`s should have.

- by:

  a vector of the by-values.

- by.levels:

  a vector of the levels of `by`.

- totallab:

  What to call the total "column"

## Value

Usually a vector of the appropriate numbers.

## Details

Not all these functions are exported, in order to avoid conflicting
NAMESPACES. Note also that the functions prefixed with `"arsenal_"` can
be referred to by their short names (e.g., `"min"` for `"arsenal_min"`).

## See also

[`includeNA`](https://mayoverse.github.io/arsenal/reference/NA.operations.md),
[`tableby.control`](https://mayoverse.github.io/arsenal/reference/tableby.control.md)
