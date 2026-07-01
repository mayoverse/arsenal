# The paired function

## Introduction

Another one of the most common tables in medical literature includes
summary statistics for a set of variables paired across two time points.
Locally at Mayo, the SAS macro `%paired` was written to create summary
tables with a single call. With the increasing interest in R, we have
developed the function
[`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md) to
create similar tables within the R environment.

This vignette is light on purpose;
[`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md)
piggybacks off of tableby, so most documentation there applies here,
too.

## Simple Example

The first step when using the
[`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md)
function is to load the `arsenal` package. We can’t use `mockstudy` here
because we need a dataset with paired observations, so we’ll create our
own dataset.

``` r

library(arsenal)
dat <- data.frame(
  tp = paste0("Time Point ", c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2)),
  id = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 6),
  Cat = c("A", "A", "A", "B", "B", "B", "B", "A", NA, "B"),
  Fac = factor(c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A")),
  Num = c(1, 2, 3, 4, 4, 3, 3, 4, 0, NA),
  Ord = ordered(c("I", "II", "II", "III", "III", "III", "I", "III", "II", "I")),
  Lgl = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE),
  Dat = as.Date("2018-05-01") + c(1, 1, 2, 2, 3, 4, 5, 6, 3, 4),
  stringsAsFactors = FALSE
)
```

To create a simple table stratified by time point, use a `formula=`
statement to specify the variables that you want summarized and the
`id=` argument to specify the paired observations.

``` r

p <- paired(tp ~ Cat + Fac + Num + Ord + Lgl + Dat, data = dat, id = id, signed.rank.exact = FALSE)
summary(p)
```

|  | Time Point 1 (N=4) | Time Point 2 (N=4) | Difference (N=4) | p value |
|:---|:--:|:--:|:--:|---:|
| **Cat** |  |  |  | 1.000 |
|    A | 2 (50.0%) | 2 (50.0%) | 1 (50.0%) |  |
|    B | 2 (50.0%) | 2 (50.0%) | 1 (50.0%) |  |
| **Fac** |  |  |  | 0.261 |
|    A | 2 (50.0%) | 1 (25.0%) | 2 (100.0%) |  |
|    B | 1 (25.0%) | 2 (50.0%) | 1 (100.0%) |  |
|    C | 1 (25.0%) | 1 (25.0%) | 1 (100.0%) |  |
| **Num** |  |  |  | 0.391 |
|    Mean (SD) | 2.750 (1.258) | 3.250 (0.957) | 0.500 (1.000) |  |
|    Range | 1.000 - 4.000 | 2.000 - 4.000 | -1.000 - 1.000 |  |
| **Ord** |  |  |  | 0.174 |
|    I | 2 (50.0%) | 0 (0.0%) | 2 (100.0%) |  |
|    II | 1 (25.0%) | 1 (25.0%) | 1 (100.0%) |  |
|    III | 1 (25.0%) | 3 (75.0%) | 0 (0.0%) |  |
| **Lgl** |  |  |  | 1.000 |
|    FALSE | 2 (50.0%) | 1 (25.0%) | 2 (100.0%) |  |
|    TRUE | 2 (50.0%) | 3 (75.0%) | 1 (50.0%) |  |
| **Dat** |  |  |  | 0.182 |
|    Median | 2018-05-03 | 2018-05-04 | 0.500 |  |
|    Range | 2018-05-02 - 2018-05-06 | 2018-05-02 - 2018-05-07 | 0.000 - 1.000 |  |

The third column shows the difference between time point 1 and time
point 2. For categorical variables, it reports the percent of
observations from time point 1 which changed in time point 2.

## NAs

Note that by default, observations which do not have both timepoints are
removed. This is easily changed using the
`na.action = na.paired("<arg>")` argument. For example:

``` r

p <- paired(tp ~ Cat + Fac + Num + Ord + Lgl + Dat, data = dat, id = id,
            signed.rank.exact = FALSE, na.action = na.paired("fill"))
summary(p)
```

|  | Time Point 1 (N=6) | Time Point 2 (N=6) | Difference (N=6) | p value |
|:---|:--:|:--:|:--:|---:|
| **Cat** |  |  |  | 1.000 |
|    N-Miss | 2 | 1 | 2 |  |
|    A | 2 (50.0%) | 2 (40.0%) | 1 (50.0%) |  |
|    B | 2 (50.0%) | 3 (60.0%) | 1 (50.0%) |  |
| **Fac** |  |  |  | 0.261 |
|    N-Miss | 1 | 1 | 2 |  |
|    A | 2 (40.0%) | 2 (40.0%) | 2 (100.0%) |  |
|    B | 1 (20.0%) | 2 (40.0%) | 1 (100.0%) |  |
|    C | 2 (40.0%) | 1 (20.0%) | 1 (100.0%) |  |
| **Num** |  |  |  | 0.391 |
|    N-Miss | 1 | 2 | 2 |  |
|    Mean (SD) | 2.200 (1.643) | 3.250 (0.957) | 0.500 (1.000) |  |
|    Range | 0.000 - 4.000 | 2.000 - 4.000 | -1.000 - 1.000 |  |
| **Ord** |  |  |  | 0.174 |
|    N-Miss | 1 | 1 | 2 |  |
|    I | 2 (40.0%) | 1 (20.0%) | 2 (100.0%) |  |
|    II | 2 (40.0%) | 1 (20.0%) | 1 (100.0%) |  |
|    III | 1 (20.0%) | 3 (60.0%) | 0 (0.0%) |  |
| **Lgl** |  |  |  | 1.000 |
|    N-Miss | 1 | 1 | 2 |  |
|    FALSE | 3 (60.0%) | 2 (40.0%) | 2 (100.0%) |  |
|    TRUE | 2 (40.0%) | 3 (60.0%) | 1 (50.0%) |  |
| **Dat** |  |  |  | 0.182 |
|    N-Miss | 1 | 1 | 2 |  |
|    Median | 2018-05-04 | 2018-05-05 | 0.500 |  |
|    Range | 2018-05-02 - 2018-05-06 | 2018-05-02 - 2018-05-07 | 0.000 - 1.000 |  |

For more details, see the help page for
[`na.paired()`](https://mayoverse.github.io/arsenal/reference/paired.internal.md).

## Available Function Options

### Testing options

The tests used to calculate p-values differ by the variable type, but
can be specified explicitly in the formula statement or in the control
function.

The following tests are accepted:

- `paired.t`: A paired t-test.

- `mcnemar`: McNemar’s test.

- `signed.rank`: the signed-rank test.

- `signtest`: the sign test.

- `notest`: Don’t perform a test.

### `paired.control` settings

A quick way to see what arguments are possible to utilize in a function
is to use the [`args()`](https://rdrr.io/r/base/args.html) command.
Settings involving the number of digits can be set in `paired.control`
or in `summary.tableby`.

``` r

args(paired.control)
```

    ## function (diff = TRUE, numeric.test = "paired.t", cat.test = "mcnemar", 
    ##     ordered.test = "signed.rank", date.test = "paired.t", mcnemar.correct = TRUE, 
    ##     signed.rank.exact = NULL, signed.rank.correct = TRUE, ...) 
    ## NULL

### `summary.tableby` settings

Since the “paired” object inherits “tableby”, the `summary.tableby`
function is what’s actually used to format and print the table.

``` r

args(arsenal:::summary.tableby)
```

    ## function (object, ..., labelTranslations = NULL, text = FALSE, 
    ##     title = NULL, pfootnote = FALSE, term.name = "") 
    ## NULL
