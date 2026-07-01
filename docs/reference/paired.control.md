# Control settings for `paired` function

Control test and summary settings for the
[`paired`](https://mayoverse.github.io/arsenal/reference/paired.md)
function.

## Usage

``` r
paired.control(
  diff = TRUE,
  numeric.test = "paired.t",
  cat.test = "mcnemar",
  ordered.test = "signed.rank",
  date.test = "paired.t",
  mcnemar.correct = TRUE,
  signed.rank.exact = NULL,
  signed.rank.correct = TRUE,
  ...
)
```

## Arguments

- diff:

  logical, telling `paired` whether to calculate a column of differences
  between time points.

- numeric.test:

  name of test for numeric RHS variables in `paired`: paired.t,
  signed.rank, signtest.

- cat.test:

  name of test for categorical variables: mcnemar

- ordered.test:

  name of test for ordered variables: signed.rank, signtest

- date.test:

  name of test to perform for date variables: paired.t, signed.rank,
  signtest

- mcnemar.correct, signed.rank.exact, signed.rank.correct:

  Options for statistical tests. See
  [`wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html) and
  [`mcnemar.test`](https://rdrr.io/r/stats/mcnemar.test.html) for
  details.

- ...:

  Arguments passed to
  [`tableby.control`](https://mayoverse.github.io/arsenal/reference/tableby.control.md)

## Value

A list with settings to be used within the
[`paired`](https://mayoverse.github.io/arsenal/reference/paired.md)
function.

## Details

Note that (with the exception of `total`) all arguments to
[`tableby.control`](https://mayoverse.github.io/arsenal/reference/tableby.control.md)
are accepted in this function (in fact, this function passes everything
through to
[`tableby.control`](https://mayoverse.github.io/arsenal/reference/tableby.control.md)).
However, there are different defaults for the statistical tests (shown
here). For details on the other arguments, please see the help page for
[`tableby.control`](https://mayoverse.github.io/arsenal/reference/tableby.control.md).

## See also

[`paired`](https://mayoverse.github.io/arsenal/reference/paired.md),
[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md),
[`tableby.control`](https://mayoverse.github.io/arsenal/reference/tableby.control.md),
[`summary.tableby`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md)

## Author

Ethan Heinzen
