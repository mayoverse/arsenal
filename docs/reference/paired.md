# Summary Statistics of a Set of Independent Variables Paired Across Two Timepoints

Summarize one or more variables (x) by a paired time variable (y).
Variables on the right side of the formula, i.e. independent variables,
are summarized by the two time points on the left of the formula.
Optionally, an appropriate test is performed to test the distribution of
the independent variables across the time points.

## Usage

``` r
paired(
  formula,
  data,
  id,
  na.action,
  subset = NULL,
  strata,
  control = NULL,
  ...
)
```

## Arguments

- formula:

  an object of class [`formula`](https://rdrr.io/r/stats/formula.html)
  of the form `time ~ var1 + ...`. See "Details" for more information.

- data:

  an optional data frame, list or environment (or object coercible by
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html) to a data
  frame) containing the variables in the model. If not found in data,
  the variables are taken from `environment(formula)`, typically the
  environment from which the function is called.

- id:

  The vector giving IDs to match up data for the same subject across two
  timepoints.

- na.action:

  a function which indicates what should happen when the data contain
  `NA`s. The default is `na.paired("in.both")`. See
  [`na.paired`](https://mayoverse.github.io/arsenal/reference/paired.internal.md)
  for more details

- subset:

  an optional vector specifying a subset of observations (rows of data)
  to be used in the results. Works as vector of logicals or an index.

- strata:

  a vector of strata to separate summaries by an additional group.

- control:

  control parameters to handle optional settings within `paired`. Two
  aspects of `paired` are controlled with these: test options of RHS
  variables and x variable summaries. Arguments for `paired.control` can
  be passed to `paired` via the `...` argument, but if a control object
  and `...` arguments are both supplied, the latter are used. See
  [`paired.control`](https://mayoverse.github.io/arsenal/reference/paired.control.md)
  for more details.

- ...:

  additional arguments to be passed to internal `paired` functions or
  [`paired.control`](https://mayoverse.github.io/arsenal/reference/paired.control.md).

## Value

An object with class `c("paired", "tableby", "arsenal_table")`

## Details

Do note that this function piggybacks off of
[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md)
quite heavily, so there is no `summary.paired` function (for instance).

These tests are accepted:

- `paired.t`: a paired [`t-test`](https://rdrr.io/r/stats/t.test.html).

- `mcnemar`: [McNemar's
  test](https://rdrr.io/r/stats/mcnemar.test.html).

- `signed.rank`: a [signed rank
  test](https://rdrr.io/r/stats/wilcox.test.html).

- `signtest`: a sign test.

- `notest`: no test is performed.

## See also

[`arsenal_table`](https://mayoverse.github.io/arsenal/reference/arsenal_table.md),
[`paired.control`](https://mayoverse.github.io/arsenal/reference/paired.control.md),
[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md),
[`formulize`](https://mayoverse.github.io/arsenal/reference/formulize.md),
[`selectall`](https://mayoverse.github.io/arsenal/reference/selectall.md)

## Author

Jason Sinnwell, Beth Atkinson, Ryan Lennon, and Ethan Heinzen
