# Control settings for `modelsum` function

Control test and summary settings for
[`modelsum`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
function.

## Usage

``` r
modelsum.control(
  digits = 3L,
  digits.ratio = 3L,
  digits.p = 3L,
  format.p = TRUE,
  show.adjust = TRUE,
  show.intercept = TRUE,
  conf.level = 0.95,
  ordinal.stats = c("OR", "CI.lower.OR", "CI.upper.OR", "p.value", "Nmiss"),
  binomial.stats = c("OR", "CI.lower.OR", "CI.upper.OR", "p.value", "concordance",
    "Nmiss"),
  gaussian.stats = c("estimate", "std.error", "p.value", "adj.r.squared", "Nmiss"),
  poisson.stats = c("RR", "CI.lower.RR", "CI.upper.RR", "p.value", "Nmiss"),
  negbin.stats = c("RR", "CI.lower.RR", "CI.upper.RR", "p.value", "Nmiss"),
  relrisk.stats = c("RR", "CI.lower.RR", "CI.upper.RR", "p.value", "Nmiss"),
  clog.stats = c("OR", "CI.lower.OR", "CI.upper.OR", "p.value", "concordance", "Nmiss"),
  survival.stats = c("HR", "CI.lower.HR", "CI.upper.HR", "p.value", "concordance",
    "Nmiss"),
  stat.labels = list(),
  ...
)
```

## Arguments

- digits:

  Numeric, denoting the number of digits after the decimal point for
  beta coefficients and standard errors.

- digits.ratio:

  Numeric, denoting the number of digits after the decimal point for
  ratios, e.g. OR, RR, HR.

- digits.p:

  Numeric, denoting the number of digits for p-values. See "Details",
  below.

- format.p:

  Logical, denoting whether to format p-values. See "Details", below.

- show.adjust:

  Logical, denoting whether to show adjustment terms.

- show.intercept:

  Logical, denoting whether to show intercept terms.

- conf.level:

  Numeric, giving the confidence level.

- ordinal.stats, binomial.stats, survival.stats, gaussian.stats,
  poisson.stats, negbin.stats, clog.stats, relrisk.stats:

  Character vectors denoting which stats to show for the various model
  types.

- stat.labels:

  A named list of labels for all the stats used above.

- ...:

  Other arguments (not in use at this time).

## Value

A list with settings to be used within the `modelsum` function.

## Details

If `format.p` is `FALSE`, `digits.p` denotes the number of significant
digits shown. The p-values will be in exponential notation if necessary.
If `format.p` is `TRUE`, `digits.p` will determine the number of digits
after the decimal point to show. If the p-value is less than the
resulting number of places, it will be formatted to show so.

## See also

[`modelsum`](https://mayoverse.github.io/arsenal/reference/modelsum.md),
[`summary.modelsum`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md),
[`modelsum.internal`](https://mayoverse.github.io/arsenal/reference/modelsum.internal.md)
