# Control settings for `tableby` function

Control test and summary settings for the
[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md)
function.

## Usage

``` r
tableby.control(
  test = TRUE,
  total = TRUE,
  total.pos = c("after", "before"),
  test.pname = NULL,
  numeric.simplify = FALSE,
  cat.simplify = FALSE,
  cat.droplevels = FALSE,
  ordered.simplify = FALSE,
  date.simplify = FALSE,
  numeric.test = "anova",
  cat.test = "chisq",
  ordered.test = "trend",
  surv.test = "logrank",
  date.test = "kwt",
  selectall.test = "notest",
  test.always = FALSE,
  numeric.stats = c("Nmiss", "meansd", "range"),
  cat.stats = c("Nmiss", "countpct"),
  ordered.stats = c("Nmiss", "countpct"),
  surv.stats = c("Nmiss", "Nevents", "medSurv"),
  date.stats = c("Nmiss", "median", "range"),
  selectall.stats = c("Nmiss", "countpct"),
  stats.labels = list(),
  digits = 3L,
  digits.count = 0L,
  digits.pct = 1L,
  digits.p = 3L,
  format.p = TRUE,
  digits.n = 0L,
  conf.level = 0.95,
  wilcox.correct = FALSE,
  wilcox.exact = NULL,
  chisq.correct = FALSE,
  simulate.p.value = FALSE,
  B = 2000,
  times = 1:5,
  ...
)
```

## Arguments

- test:

  logical, telling `tableby` whether to perform tests of x variables
  across levels of the group variable.

- total:

  logical, telling `tableby` whether to calculate a column of totals
  across group variable.

- total.pos:

  One of `"before"` or `"after"`, denoting where to put the total column
  relative to the by-variable columns.

- test.pname:

  character string denoting the p-value column name in
  [`summary.tableby`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md).
  Modifiable also with
  [`modpval.tableby`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md).

- numeric.simplify, date.simplify:

  logical, tell `tableby` whether to condense numeric/date output to a
  single line. NOTE: this only simplifies to one line if there is only
  one statistic reported, such as `meansd`. In particular, if `Nmiss` is
  specified and there are missings, then the output is not simplified.

- cat.simplify, ordered.simplify:

  logical, tell `tableby` whether to remove the first level of the
  categorical/ordinal variable if binary. If `TRUE`, only the summary
  stats of the second level are reported (unless there's only one level,
  in which case it's reported). If `"label"`, the second level's label
  is additionally appended to the label. NOTE: this only simplifies to
  one line if there is only one statistic reported, such as `countpct`.
  In particular, if `Nmiss` is specified and there are missings, then
  the output is not simplified.

- cat.droplevels:

  Should levels be dropped for categorical variables? If set to true,
  p-values will not be displayed unless `test.always = TRUE` as well.

- numeric.test:

  name of test for numeric RHS variables in `tableby`: anova, kwt
  (Kruskal-Wallis), medtest (median test). If no LHS variable exists,
  then a mean is required for a univariate test.

- cat.test:

  name of test for categorical variables: chisq, fe (Fisher's Exact)

- ordered.test:

  name of test for ordered variables: trend

- surv.test:

  name of test for survival variables: logrank

- date.test:

  name of test for date variables: kwt

- selectall.test:

  name of test for date variables: notest

- test.always:

  Should the test be performed even if one or more by-group has 0
  observations? Relevant for kwt and anova.

- numeric.stats, cat.stats, ordered.stats, surv.stats, date.stats,
  selectall.stats:

  summary statistics to include for the respective class of RHS
  variables within the levels of the group LHS variable.

- stats.labels:

  A named list of labels for all the statistics function names, where
  the function name is the named element in the list and the value that
  goes with it is a string containing the formal name that will be
  printed in all printed renderings of the output, e.g.,
  `list(countpct="Count (Pct)")`. Any unnamed elements will be ignored.
  Passing `NULL` will disable labels.

- digits:

  Number of decimal places for numeric values.

- digits.count:

  Number of decimal places for count values.

- digits.pct:

  Number of decimal places for percents.

- digits.p:

  Number of decimal places for p-values.

- format.p:

  Logical, denoting whether to format p-values, or character, a
  [`glue`](https://glue.tidyverse.org/reference/glue.html) specification
  for how to format. See "Details", below.

- digits.n:

  Number of decimal places for N's in the header. Set it to NA to
  suppress the N's.

- conf.level:

  Numeric, denoting what confidence level to use for confidence
  intervals. (See, e.g.,
  [`binomCI`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md))

- wilcox.correct, wilcox.exact:

  See [`wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html)

- chisq.correct:

  logical, correction factor for chisq.test

- simulate.p.value:

  logical, simulate p-value for categorical tests (fe and chisq)

- B:

  number of simulations to perform for simulation-based p-value

- times:

  A vector of times to use for survival summaries.

- ...:

  additional arguments.

## Value

A list with settings to be used within the `tableby` function.

## Details

All tests can be turned off by setting `test` to FALSE. Otherwise, test
are set to default settings in this list, or set explicitly in the
formula of `tableby`.

If `format.p` is `FALSE`, `digits.p` denotes the number of significant
digits shown. The p-values will be in exponential notation if necessary.
If `format.p` is `TRUE`, `digits.p` will determine the number of digits
after the decimal point to show. If the p-value is less than the
resulting number of places, it will be formatted to show so. If
`format.p` is a character string, it will be treated as a
[`glue`](https://glue.tidyverse.org/reference/glue.html) specification:
the p-value is exposed as "p", and "digits.p" as "digits.p".

Options for statistics are described more thoroughly in the vignette and
are listed in
[tableby.stats](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)

## See also

[`anova`](https://rdrr.io/r/stats/anova.html),
[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html),
[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md),
[`summary.tableby`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md),
[`tableby.stats`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md).

## Author

Jason Sinnwell, Beth Atkinson, Ethan Heinzen, Terry Therneau, adapted
from SAS Macros written by Paul Novotny and Ryan Lennon

## Examples

``` r
set.seed(100)
## make 3+ categories for Response
mdat <- data.frame(Response=c(0,0,0,0,0,1,1,1,1,1),
                   Sex=sample(c("Male", "Female"), 10,replace=TRUE),
                   Age=round(rnorm(10,mean=40, sd=5)),
                   HtIn=round(rnorm(10,mean=65,sd=5)))

## allow default summaries in RHS variables, and pass control args to
## main function, to be picked up with ... when calling tableby.control
outResp <- tableby(Response ~ Sex + Age + HtIn, data=mdat, total=FALSE, test=TRUE)
outCtl <- tableby(Response ~ Sex + Age + HtIn, data=mdat,
                  control=tableby.control(total=TRUE, cat.simplify=TRUE,
                  cat.stats=c("Nmiss","countpct"),digits=1))
summary(outResp, text=TRUE)
#> 
#> 
#> |             |     0 (N=5)     |     1 (N=5)     | p value|
#> |:------------|:---------------:|:---------------:|-------:|
#> |Sex          |                 |                 |   1.000|
#> |-  Female    |    3 (60.0%)    |    3 (60.0%)    |        |
#> |-  Male      |    2 (40.0%)    |    2 (40.0%)    |        |
#> |Age          |                 |                 |   0.449|
#> |-  Mean (SD) | 39.400 (3.435)  | 40.800 (1.924)  |        |
#> |-  Range     | 36.000 - 44.000 | 39.000 - 44.000 |        |
#> |HtIn         |                 |                 |   0.771|
#> |-  Mean (SD) | 66.600 (6.504)  | 65.600 (3.578)  |        |
#> |-  Range     | 60.000 - 77.000 | 61.000 - 69.000 |        |
#> 
summary(outCtl, text=TRUE)
#> 
#> 
#> |             |   0 (N=5)   |   1 (N=5)   | Total (N=10) | p value|
#> |:------------|:-----------:|:-----------:|:------------:|-------:|
#> |Sex          |  2 (40.0%)  |  2 (40.0%)  |  4 (40.0%)   |   1.000|
#> |Age          |             |             |              |   0.449|
#> |-  Mean (SD) | 39.4 (3.4)  | 40.8 (1.9)  |  40.1 (2.7)  |        |
#> |-  Range     | 36.0 - 44.0 | 39.0 - 44.0 | 36.0 - 44.0  |        |
#> |HtIn         |             |             |              |   0.771|
#> |-  Mean (SD) | 66.6 (6.5)  | 65.6 (3.6)  |  66.1 (5.0)  |        |
#> |-  Range     | 60.0 - 77.0 | 61.0 - 69.0 | 60.0 - 77.0  |        |
#> 
```
