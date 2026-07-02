# The freqlist function

## Overview

[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
is a function meant to produce output similar to SAS’s `PROC FREQ`
procedure when using the `/list` option of the `TABLE` statement.
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
provides options for handling missing or sparse data and can provide
cumulative counts and percentages based on subgroups. It depends on the
`knitr` package for printing.

``` r

require(arsenal)
```

### Sample dataset

For our examples, we’ll load the `mockstudy` data included with this
package and use it to create a basic table. Because they have fewer
levels, for brevity, we’ll use the variables arm, sex, and mdquality.s
to create the example table. We’ll retain NAs in the table creation. See
the appendix for notes regarding default NA handling and other useful
information regarding tables in R.

``` r

# load the data
data(mockstudy)

# retain NAs when creating the table using the useNA argument
tab.ex <- table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA="ifany")
```

## The `freqlist` object

The
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
function is an S3 generic (with methods for tables and formulas) which
returns an object of class `"freqlist"`.

``` r

example1 <- freqlist(tab.ex)

str(example1)
```

    List of 3
     $ Call   : language freqlist.table(object = tab.ex)
     $ control:List of 5
      ..$ sparse      : logi FALSE
      ..$ single      : logi FALSE
      ..$ dupLabels   : logi FALSE
      ..$ digits.count: int 0
      ..$ digits.pct  : int 2
     $ tables :List of 1
      ..$ :List of 6
      .. ..$ y         :List of 2
      .. .. ..$ term : chr ""
      .. .. ..$ label: chr ""
      .. ..$ strata    :List of 4
      .. .. ..$ term     : chr ""
      .. .. ..$ values   : chr ""
      .. .. ..$ label    : chr ""
      .. .. ..$ hasStrata: logi FALSE
      .. ..$ x         :List of 7
      .. .. ..$ arm        :List of 3
      .. .. .. ..$ variable: chr "arm"
      .. .. .. ..$ label   : chr "arm"
      .. .. .. ..$ term    : chr "arm"
      .. .. ..$ sex        :List of 3
      .. .. .. ..$ variable: chr "sex"
      .. .. .. ..$ label   : chr "sex"
      .. .. .. ..$ term    : chr "sex"
      .. .. ..$ mdquality.s:List of 3
      .. .. .. ..$ variable: chr "mdquality.s"
      .. .. .. ..$ label   : chr "mdquality.s"
      .. .. .. ..$ term    : chr "mdquality.s"
      .. .. ..$ Freq       :List of 3
      .. .. .. ..$ variable: chr "Freq"
      .. .. .. ..$ label   : chr "Freq"
      .. .. .. ..$ term    : chr "Freq"
      .. .. ..$ cumFreq    :List of 3
      .. .. .. ..$ variable: chr "cumFreq"
      .. .. .. ..$ label   : chr "Cumulative Freq"
      .. .. .. ..$ term    : chr "cumFreq"
      .. .. ..$ freqPercent:List of 3
      .. .. .. ..$ variable: chr "freqPercent"
      .. .. .. ..$ label   : chr "Percent"
      .. .. .. ..$ term    : chr "freqPercent"
      .. .. ..$ cumPercent :List of 3
      .. .. .. ..$ variable: chr "cumPercent"
      .. .. .. ..$ label   : chr "Cumulative Percent"
      .. .. .. ..$ term    : chr "cumPercent"
      .. ..$ tables    :List of 1
      .. .. ..$ :'data.frame':  18 obs. of  7 variables:
      .. .. .. ..$ arm        : Factor w/ 3 levels "A: IFL","F: FOLFOX",..: 1 1 1 1 1 1 2 2 2 2 ...
      .. .. .. ..$ sex        : Factor w/ 2 levels "Male","Female": 1 1 1 2 2 2 1 1 1 2 ...
      .. .. .. ..$ mdquality.s: Factor w/ 2 levels "0","1": 1 2 NA 1 2 NA 1 2 NA 1 ...
      .. .. .. ..$ Freq       : int [1:18] 29 214 34 12 118 21 31 285 95 21 ...
      .. .. .. ..$ cumFreq    : int [1:18] 29 243 277 289 407 428 459 744 839 860 ...
      .. .. .. ..$ freqPercent: num [1:18] 1.935 14.276 2.268 0.801 7.872 ...
      .. .. .. ..$ cumPercent : num [1:18] 1.93 16.21 18.48 19.28 27.15 ...
      .. ..$ hasWeights: logi FALSE
      .. ..$ na.options: chr "include"
     - attr(*, "class")= chr [1:2] "freqlist" "arsenal_table"

``` r

# view the data frame portion of freqlist output
head(as.data.frame(example1)) ## or use as.data.frame(example1)
```

         arm    sex mdquality.s Freq cumFreq freqPercent cumPercent
    1 A: IFL   Male           0   29      29   1.9346231   1.934623
    2 A: IFL   Male           1  214     243  14.2761841  16.210807
    3 A: IFL   Male        <NA>   34     277   2.2681788  18.478986
    4 A: IFL Female           0   12     289   0.8005337  19.279520
    5 A: IFL Female           1  118     407   7.8719146  27.151434
    6 A: IFL Female        <NA>   21     428   1.4009340  28.552368

## Basic output using `summary()`

The `summary` method for
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
relies on the `kable()` function (in the `knitr` package) for printing.
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) converts
the output to markdown which can be printed in the console or easily
rendered in Word, PDF, or HTML documents.

Note that you must supply `results="asis"` to properly format the
markdown output.

``` r

summary(example1)
```

| arm       | sex    | mdquality.s | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:----------|:-------|:------------|-----:|----------------:|--------:|-------------------:|
| A: IFL    | Male   | 0           |   29 |              29 |    1.93 |               1.93 |
|           |        | 1           |  214 |             243 |   14.28 |              16.21 |
|           |        | NA          |   34 |             277 |    2.27 |              18.48 |
|           | Female | 0           |   12 |             289 |    0.80 |              19.28 |
|           |        | 1           |  118 |             407 |    7.87 |              27.15 |
|           |        | NA          |   21 |             428 |    1.40 |              28.55 |
| F: FOLFOX | Male   | 0           |   31 |             459 |    2.07 |              30.62 |
|           |        | 1           |  285 |             744 |   19.01 |              49.63 |
|           |        | NA          |   95 |             839 |    6.34 |              55.97 |
|           | Female | 0           |   21 |             860 |    1.40 |              57.37 |
|           |        | 1           |  198 |            1058 |   13.21 |              70.58 |
|           |        | NA          |   61 |            1119 |    4.07 |              74.65 |
| G: IROX   | Male   | 0           |   17 |            1136 |    1.13 |              75.78 |
|           |        | 1           |  187 |            1323 |   12.47 |              88.26 |
|           |        | NA          |   24 |            1347 |    1.60 |              89.86 |
|           | Female | 0           |   14 |            1361 |    0.93 |              90.79 |
|           |        | 1           |  121 |            1482 |    8.07 |              98.87 |
|           |        | NA          |   17 |            1499 |    1.13 |             100.00 |

You can print a title for the table using the `title=` argument.

``` r

summary(example1, title="Basic freqlist output")
```

| arm       | sex    | mdquality.s | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:----------|:-------|:------------|-----:|----------------:|--------:|-------------------:|
| A: IFL    | Male   | 0           |   29 |              29 |    1.93 |               1.93 |
|           |        | 1           |  214 |             243 |   14.28 |              16.21 |
|           |        | NA          |   34 |             277 |    2.27 |              18.48 |
|           | Female | 0           |   12 |             289 |    0.80 |              19.28 |
|           |        | 1           |  118 |             407 |    7.87 |              27.15 |
|           |        | NA          |   21 |             428 |    1.40 |              28.55 |
| F: FOLFOX | Male   | 0           |   31 |             459 |    2.07 |              30.62 |
|           |        | 1           |  285 |             744 |   19.01 |              49.63 |
|           |        | NA          |   95 |             839 |    6.34 |              55.97 |
|           | Female | 0           |   21 |             860 |    1.40 |              57.37 |
|           |        | 1           |  198 |            1058 |   13.21 |              70.58 |
|           |        | NA          |   61 |            1119 |    4.07 |              74.65 |
| G: IROX   | Male   | 0           |   17 |            1136 |    1.13 |              75.78 |
|           |        | 1           |  187 |            1323 |   12.47 |              88.26 |
|           |        | NA          |   24 |            1347 |    1.60 |              89.86 |
|           | Female | 0           |   14 |            1361 |    0.93 |              90.79 |
|           |        | 1           |  121 |            1482 |    8.07 |              98.87 |
|           |        | NA          |   17 |            1499 |    1.13 |             100.00 |

Basic freqlist output {.table}

You can also easily pull out the `freqlist` data frame for more
complicated formatting or manipulation (e.g. with another function such
as [`xtable()`](https://rdrr.io/pkg/xtable/man/xtable.html) or
`pander()`) using `as.data.frame(summary())`:

``` r

head(as.data.frame(summary(example1)))
```

         arm    sex mdquality.s Freq Cumulative Freq Percent Cumulative Percent
    1 A: IFL   Male           0   29              29    1.93               1.93
    2                         1  214             243   14.28              16.21
    3                      <NA>   34             277    2.27              18.48
    4        Female           0   12             289    0.80              19.28
    5                         1  118             407    7.87              27.15
    6                      <NA>   21             428    1.40              28.55

## Using a formula with `freqlist`

Instead of passing a pre-computed table to
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md),
you can instead pass a formula, which will be in turn passed to the
[`xtabs()`](https://rdrr.io/r/stats/xtabs.html) function. Additional
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
arguments are passed through the `...` to the
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
table method.

Note that
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
sets the `addNA=TRUE` argument by default:

``` r

summary(freqlist(~ arm + sex + mdquality.s, data = mockstudy))
```

| Treatment Arm | sex | mdquality.s | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:---|:---|:---|---:|---:|---:|---:|
| A: IFL | Male | 0 | 29 | 29 | 1.93 | 1.93 |
|  |  | 1 | 214 | 243 | 14.28 | 16.21 |
|  |  | NA | 34 | 277 | 2.27 | 18.48 |
|  | Female | 0 | 12 | 289 | 0.80 | 19.28 |
|  |  | 1 | 118 | 407 | 7.87 | 27.15 |
|  |  | NA | 21 | 428 | 1.40 | 28.55 |
| F: FOLFOX | Male | 0 | 31 | 459 | 2.07 | 30.62 |
|  |  | 1 | 285 | 744 | 19.01 | 49.63 |
|  |  | NA | 95 | 839 | 6.34 | 55.97 |
|  | Female | 0 | 21 | 860 | 1.40 | 57.37 |
|  |  | 1 | 198 | 1058 | 13.21 | 70.58 |
|  |  | NA | 61 | 1119 | 4.07 | 74.65 |
| G: IROX | Male | 0 | 17 | 1136 | 1.13 | 75.78 |
|  |  | 1 | 187 | 1323 | 12.47 | 88.26 |
|  |  | NA | 24 | 1347 | 1.60 | 89.86 |
|  | Female | 0 | 14 | 1361 | 0.93 | 90.79 |
|  |  | 1 | 121 | 1482 | 8.07 | 98.87 |
|  |  | NA | 17 | 1499 | 1.13 | 100.00 |

One can also set NAs to an explicit value using
[`includeNA()`](https://mayoverse.github.io/arsenal/reference/NA.operations.md).

``` r

summary(freqlist(~ arm + sex + includeNA(mdquality.s, "Missing"), data = mockstudy))
```

| Treatment Arm | sex | includeNA(mdquality.s, “Missing”) | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:---|:---|:---|---:|---:|---:|---:|
| A: IFL | Male | 0 | 29 | 29 | 1.93 | 1.93 |
|  |  | 1 | 214 | 243 | 14.28 | 16.21 |
|  |  | Missing | 34 | 277 | 2.27 | 18.48 |
|  | Female | 0 | 12 | 289 | 0.80 | 19.28 |
|  |  | 1 | 118 | 407 | 7.87 | 27.15 |
|  |  | Missing | 21 | 428 | 1.40 | 28.55 |
| F: FOLFOX | Male | 0 | 31 | 459 | 2.07 | 30.62 |
|  |  | 1 | 285 | 744 | 19.01 | 49.63 |
|  |  | Missing | 95 | 839 | 6.34 | 55.97 |
|  | Female | 0 | 21 | 860 | 1.40 | 57.37 |
|  |  | 1 | 198 | 1058 | 13.21 | 70.58 |
|  |  | Missing | 61 | 1119 | 4.07 | 74.65 |
| G: IROX | Male | 0 | 17 | 1136 | 1.13 | 75.78 |
|  |  | 1 | 187 | 1323 | 12.47 | 88.26 |
|  |  | Missing | 24 | 1347 | 1.60 | 89.86 |
|  | Female | 0 | 14 | 1361 | 0.93 | 90.79 |
|  |  | 1 | 121 | 1482 | 8.07 | 98.87 |
|  |  | Missing | 17 | 1499 | 1.13 | 100.00 |

In fact, since [`xtabs()`](https://rdrr.io/r/stats/xtabs.html) allows
for left-hand-side weights, so does
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)!

``` r

mockstudy$weights <- c(10000, rep(1, nrow(mockstudy) - 1))
summary(freqlist(weights ~ arm + sex + addNA(mdquality.s), data = mockstudy))
```

| Treatment Arm | sex | addNA(mdquality.s) | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:---|:---|:---|---:|---:|---:|---:|
| A: IFL | Male | 0 | 29 | 29 | 0.25 | 0.25 |
|  |  | 1 | 214 | 243 | 1.86 | 2.11 |
|  |  | NA | 34 | 277 | 0.30 | 2.41 |
|  | Female | 0 | 12 | 289 | 0.10 | 2.51 |
|  |  | 1 | 118 | 407 | 1.03 | 3.54 |
|  |  | NA | 21 | 428 | 0.18 | 3.72 |
| F: FOLFOX | Male | 0 | 31 | 459 | 0.27 | 3.99 |
|  |  | 1 | 285 | 744 | 2.48 | 6.47 |
|  |  | NA | 10094 | 10838 | 87.79 | 94.26 |
|  | Female | 0 | 21 | 10859 | 0.18 | 94.44 |
|  |  | 1 | 198 | 11057 | 1.72 | 96.16 |
|  |  | NA | 61 | 11118 | 0.53 | 96.70 |
| G: IROX | Male | 0 | 17 | 11135 | 0.15 | 96.84 |
|  |  | 1 | 187 | 11322 | 1.63 | 98.47 |
|  |  | NA | 24 | 11346 | 0.21 | 98.68 |
|  | Female | 0 | 14 | 11360 | 0.12 | 98.80 |
|  |  | 1 | 121 | 11481 | 1.05 | 99.85 |
|  |  | NA | 17 | 11498 | 0.15 | 100.00 |

You can also specify multiple weights:

``` r

mockstudy$weights2 <- c(rep(1, nrow(mockstudy) - 1), 10000)
summary(freqlist(list(weights, weights2) ~ arm + sex + addNA(mdquality.s), data = mockstudy))
```

| Treatment Arm | sex | addNA(mdquality.s) | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:---|:---|:---|---:|---:|---:|---:|
| A: IFL | Male | 0 | 29 | 29 | 0.25 | 0.25 |
|  |  | 1 | 214 | 243 | 1.86 | 2.11 |
|  |  | NA | 34 | 277 | 0.30 | 2.41 |
|  | Female | 0 | 12 | 289 | 0.10 | 2.51 |
|  |  | 1 | 118 | 407 | 1.03 | 3.54 |
|  |  | NA | 21 | 428 | 0.18 | 3.72 |
| F: FOLFOX | Male | 0 | 31 | 459 | 0.27 | 3.99 |
|  |  | 1 | 285 | 744 | 2.48 | 6.47 |
|  |  | NA | 10094 | 10838 | 87.79 | 94.26 |
|  | Female | 0 | 21 | 10859 | 0.18 | 94.44 |
|  |  | 1 | 198 | 11057 | 1.72 | 96.16 |
|  |  | NA | 61 | 11118 | 0.53 | 96.70 |
| G: IROX | Male | 0 | 17 | 11135 | 0.15 | 96.84 |
|  |  | 1 | 187 | 11322 | 1.63 | 98.47 |
|  |  | NA | 24 | 11346 | 0.21 | 98.68 |
|  | Female | 0 | 14 | 11360 | 0.12 | 98.80 |
|  |  | 1 | 121 | 11481 | 1.05 | 99.85 |
|  |  | NA | 17 | 11498 | 0.15 | 100.00 |

| Treatment Arm | sex | addNA(mdquality.s) | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:---|:---|:---|---:|---:|---:|---:|
| A: IFL | Male | 0 | 29 | 29 | 0.25 | 0.25 |
|  |  | 1 | 214 | 243 | 1.86 | 2.11 |
|  |  | NA | 34 | 277 | 0.30 | 2.41 |
|  | Female | 0 | 12 | 289 | 0.10 | 2.51 |
|  |  | 1 | 118 | 407 | 1.03 | 3.54 |
|  |  | NA | 21 | 428 | 0.18 | 3.72 |
| F: FOLFOX | Male | 0 | 31 | 459 | 0.27 | 3.99 |
|  |  | 1 | 285 | 744 | 2.48 | 6.47 |
|  |  | NA | 95 | 839 | 0.83 | 7.30 |
|  | Female | 0 | 21 | 860 | 0.18 | 7.48 |
|  |  | 1 | 198 | 1058 | 1.72 | 9.20 |
|  |  | NA | 10060 | 11118 | 87.49 | 96.70 |
| G: IROX | Male | 0 | 17 | 11135 | 0.15 | 96.84 |
|  |  | 1 | 187 | 11322 | 1.63 | 98.47 |
|  |  | NA | 24 | 11346 | 0.21 | 98.68 |
|  | Female | 0 | 14 | 11360 | 0.12 | 98.80 |
|  |  | 1 | 121 | 11481 | 1.05 | 99.85 |
|  |  | NA | 17 | 11498 | 0.15 | 100.00 |

## Rounding percentage digits or changing variable names for printing

The `digits.pct=` argument takes a single numeric value and controls the
number of digits of percentages in the output. The `digits.count=`
argument takes a similar argument and controls the number of digits of
the count columns. The `labelTranslations=` argument is a named
character vector or list. Both options are applied in the following
example.

``` r

example2 <- freqlist(tab.ex, labelTranslations = c(arm = "Treatment Arm", sex = "Gender", mdquality.s = "LASA QOL"),
                      digits.pct = 1, digits.count = 1)
summary(example2)
```

| Treatment Arm | Gender | LASA QOL |  Freq | Cumulative Freq | Percent | Cumulative Percent |
|:--------------|:-------|:---------|------:|----------------:|--------:|-------------------:|
| A: IFL        | Male   | 0        |  29.0 |            29.0 |     1.9 |                1.9 |
|               |        | 1        | 214.0 |           243.0 |    14.3 |               16.2 |
|               |        | NA       |  34.0 |           277.0 |     2.3 |               18.5 |
|               | Female | 0        |  12.0 |           289.0 |     0.8 |               19.3 |
|               |        | 1        | 118.0 |           407.0 |     7.9 |               27.2 |
|               |        | NA       |  21.0 |           428.0 |     1.4 |               28.6 |
| F: FOLFOX     | Male   | 0        |  31.0 |           459.0 |     2.1 |               30.6 |
|               |        | 1        | 285.0 |           744.0 |    19.0 |               49.6 |
|               |        | NA       |  95.0 |           839.0 |     6.3 |               56.0 |
|               | Female | 0        |  21.0 |           860.0 |     1.4 |               57.4 |
|               |        | 1        | 198.0 |          1058.0 |    13.2 |               70.6 |
|               |        | NA       |  61.0 |          1119.0 |     4.1 |               74.6 |
| G: IROX       | Male   | 0        |  17.0 |          1136.0 |     1.1 |               75.8 |
|               |        | 1        | 187.0 |          1323.0 |    12.5 |               88.3 |
|               |        | NA       |  24.0 |          1347.0 |     1.6 |               89.9 |
|               | Female | 0        |  14.0 |          1361.0 |     0.9 |               90.8 |
|               |        | 1        | 121.0 |          1482.0 |     8.1 |               98.9 |
|               |        | NA       |  17.0 |          1499.0 |     1.1 |              100.0 |

## Additional examples

### Including combinations with frequencies of zero

The `sparse=` argument takes a single logical value as input. The
default option is `FALSE`. If set to `TRUE`, the sparse option will
include combinations with frequencies of zero in the list of results. As
our initial table did not have any such levels, we create a second table
to use in our example.

``` r

summary(freqlist(~ race + sex + arm, data = mockstudy, sparse = TRUE, digits.pct=1))
```

| Race | sex | Treatment Arm | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:---|:---|:---|---:|---:|---:|---:|
| African-Am | Male | A: IFL | 25 | 25 | 1.7 | 1.7 |
|  |  | F: FOLFOX | 24 | 49 | 1.6 | 3.3 |
|  |  | G: IROX | 16 | 65 | 1.1 | 4.3 |
|  | Female | A: IFL | 14 | 79 | 0.9 | 5.3 |
|  |  | F: FOLFOX | 25 | 104 | 1.7 | 6.9 |
|  |  | G: IROX | 11 | 115 | 0.7 | 7.7 |
| Asian | Male | A: IFL | 0 | 115 | 0.0 | 7.7 |
|  |  | F: FOLFOX | 10 | 125 | 0.7 | 8.3 |
|  |  | G: IROX | 1 | 126 | 0.1 | 8.4 |
|  | Female | A: IFL | 1 | 127 | 0.1 | 8.5 |
|  |  | F: FOLFOX | 4 | 131 | 0.3 | 8.7 |
|  |  | G: IROX | 2 | 133 | 0.1 | 8.9 |
| Caucasian | Male | A: IFL | 240 | 373 | 16.0 | 24.9 |
|  |  | F: FOLFOX | 352 | 725 | 23.5 | 48.4 |
|  |  | G: IROX | 195 | 920 | 13.0 | 61.4 |
|  | Female | A: IFL | 131 | 1051 | 8.7 | 70.1 |
|  |  | F: FOLFOX | 234 | 1285 | 15.6 | 85.7 |
|  |  | G: IROX | 136 | 1421 | 9.1 | 94.8 |
| Hawaii/Pacific | Male | A: IFL | 1 | 1422 | 0.1 | 94.9 |
|  |  | F: FOLFOX | 1 | 1423 | 0.1 | 94.9 |
|  |  | G: IROX | 0 | 1423 | 0.0 | 94.9 |
|  | Female | A: IFL | 0 | 1423 | 0.0 | 94.9 |
|  |  | F: FOLFOX | 2 | 1425 | 0.1 | 95.1 |
|  |  | G: IROX | 1 | 1426 | 0.1 | 95.1 |
| Hispanic | Male | A: IFL | 8 | 1434 | 0.5 | 95.7 |
|  |  | F: FOLFOX | 17 | 1451 | 1.1 | 96.8 |
|  |  | G: IROX | 12 | 1463 | 0.8 | 97.6 |
|  | Female | A: IFL | 4 | 1467 | 0.3 | 97.9 |
|  |  | F: FOLFOX | 11 | 1478 | 0.7 | 98.6 |
|  |  | G: IROX | 2 | 1480 | 0.1 | 98.7 |
| Native-Am/Alaska | Male | A: IFL | 1 | 1481 | 0.1 | 98.8 |
|  |  | F: FOLFOX | 0 | 1481 | 0.0 | 98.8 |
|  |  | G: IROX | 2 | 1483 | 0.1 | 98.9 |
|  | Female | A: IFL | 1 | 1484 | 0.1 | 99.0 |
|  |  | F: FOLFOX | 1 | 1485 | 0.1 | 99.1 |
|  |  | G: IROX | 0 | 1485 | 0.0 | 99.1 |
| Other | Male | A: IFL | 2 | 1487 | 0.1 | 99.2 |
|  |  | F: FOLFOX | 2 | 1489 | 0.1 | 99.3 |
|  |  | G: IROX | 1 | 1490 | 0.1 | 99.4 |
|  | Female | A: IFL | 0 | 1490 | 0.0 | 99.4 |
|  |  | F: FOLFOX | 2 | 1492 | 0.1 | 99.5 |
|  |  | G: IROX | 0 | 1492 | 0.0 | 99.5 |
| NA | Male | A: IFL | 0 | 1492 | 0.0 | 99.5 |
|  |  | F: FOLFOX | 5 | 1497 | 0.3 | 99.9 |
|  |  | G: IROX | 1 | 1498 | 0.1 | 99.9 |
|  | Female | A: IFL | 0 | 1498 | 0.0 | 99.9 |
|  |  | F: FOLFOX | 1 | 1499 | 0.1 | 100.0 |
|  |  | G: IROX | 0 | 1499 | 0.0 | 100.0 |

### Options for NA handling

The various `na.options=` allow you to include or exclude data with
missing values for one or more factor levels in the counts and
percentages, as well as show the missing data but exclude it from the
cumulative counts and percentages. The default option is to include all
combinations with missing values.

``` r

summary(freqlist(tab.ex, na.options="include"))
```

| arm       | sex    | mdquality.s | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:----------|:-------|:------------|-----:|----------------:|--------:|-------------------:|
| A: IFL    | Male   | 0           |   29 |              29 |    1.93 |               1.93 |
|           |        | 1           |  214 |             243 |   14.28 |              16.21 |
|           |        | NA          |   34 |             277 |    2.27 |              18.48 |
|           | Female | 0           |   12 |             289 |    0.80 |              19.28 |
|           |        | 1           |  118 |             407 |    7.87 |              27.15 |
|           |        | NA          |   21 |             428 |    1.40 |              28.55 |
| F: FOLFOX | Male   | 0           |   31 |             459 |    2.07 |              30.62 |
|           |        | 1           |  285 |             744 |   19.01 |              49.63 |
|           |        | NA          |   95 |             839 |    6.34 |              55.97 |
|           | Female | 0           |   21 |             860 |    1.40 |              57.37 |
|           |        | 1           |  198 |            1058 |   13.21 |              70.58 |
|           |        | NA          |   61 |            1119 |    4.07 |              74.65 |
| G: IROX   | Male   | 0           |   17 |            1136 |    1.13 |              75.78 |
|           |        | 1           |  187 |            1323 |   12.47 |              88.26 |
|           |        | NA          |   24 |            1347 |    1.60 |              89.86 |
|           | Female | 0           |   14 |            1361 |    0.93 |              90.79 |
|           |        | 1           |  121 |            1482 |    8.07 |              98.87 |
|           |        | NA          |   17 |            1499 |    1.13 |             100.00 |

``` r

summary(freqlist(tab.ex, na.options="showexclude"))
```

| arm       | sex    | mdquality.s | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:----------|:-------|:------------|-----:|----------------:|--------:|-------------------:|
| A: IFL    | Male   | 0           |   29 |              29 |    2.33 |               2.33 |
|           |        | 1           |  214 |             243 |   17.16 |              19.49 |
|           |        | NA          |   34 |              NA |      NA |                 NA |
|           | Female | 0           |   12 |             255 |    0.96 |              20.45 |
|           |        | 1           |  118 |             373 |    9.46 |              29.91 |
|           |        | NA          |   21 |              NA |      NA |                 NA |
| F: FOLFOX | Male   | 0           |   31 |             404 |    2.49 |              32.40 |
|           |        | 1           |  285 |             689 |   22.85 |              55.25 |
|           |        | NA          |   95 |              NA |      NA |                 NA |
|           | Female | 0           |   21 |             710 |    1.68 |              56.94 |
|           |        | 1           |  198 |             908 |   15.88 |              72.81 |
|           |        | NA          |   61 |              NA |      NA |                 NA |
| G: IROX   | Male   | 0           |   17 |             925 |    1.36 |              74.18 |
|           |        | 1           |  187 |            1112 |   15.00 |              89.17 |
|           |        | NA          |   24 |              NA |      NA |                 NA |
|           | Female | 0           |   14 |            1126 |    1.12 |              90.30 |
|           |        | 1           |  121 |            1247 |    9.70 |             100.00 |
|           |        | NA          |   17 |              NA |      NA |                 NA |

``` r

summary(freqlist(tab.ex, na.options="remove"))
```

| arm       | sex    | mdquality.s | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:----------|:-------|:------------|-----:|----------------:|--------:|-------------------:|
| A: IFL    | Male   | 0           |   29 |              29 |    2.33 |               2.33 |
|           |        | 1           |  214 |             243 |   17.16 |              19.49 |
|           | Female | 0           |   12 |             255 |    0.96 |              20.45 |
|           |        | 1           |  118 |             373 |    9.46 |              29.91 |
| F: FOLFOX | Male   | 0           |   31 |             404 |    2.49 |              32.40 |
|           |        | 1           |  285 |             689 |   22.85 |              55.25 |
|           | Female | 0           |   21 |             710 |    1.68 |              56.94 |
|           |        | 1           |  198 |             908 |   15.88 |              72.81 |
| G: IROX   | Male   | 0           |   17 |             925 |    1.36 |              74.18 |
|           |        | 1           |  187 |            1112 |   15.00 |              89.17 |
|           | Female | 0           |   14 |            1126 |    1.12 |              90.30 |
|           |        | 1           |  121 |            1247 |    9.70 |             100.00 |

### Frequency counts and percentages subset by factor levels

The `strata=` argument internally subsets the data by the specified
factor prior to calculating cumulative counts and percentages. By
default, when used each subset will print in a separate table. Using the
`single = TRUE` option when printing will collapse the subsetted result
into a single table.

``` r

example3 <- freqlist(tab.ex, strata = c("arm","sex"))
summary(example3)
```

| arm    | sex  | mdquality.s | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:-------|:-----|:------------|-----:|----------------:|--------:|-------------------:|
| A: IFL | Male | 0           |   29 |              29 |   10.47 |              10.47 |
|        |      | 1           |  214 |             243 |   77.26 |              87.73 |
|        |      | NA          |   34 |             277 |   12.27 |             100.00 |

| arm    | sex    | mdquality.s | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:-------|:-------|:------------|-----:|----------------:|--------:|-------------------:|
| A: IFL | Female | 0           |   12 |              12 |    7.95 |               7.95 |
|        |        | 1           |  118 |             130 |   78.15 |              86.09 |
|        |        | NA          |   21 |             151 |   13.91 |             100.00 |

| arm       | sex  | mdquality.s | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:----------|:-----|:------------|-----:|----------------:|--------:|-------------------:|
| F: FOLFOX | Male | 0           |   31 |              31 |    7.54 |               7.54 |
|           |      | 1           |  285 |             316 |   69.34 |              76.89 |
|           |      | NA          |   95 |             411 |   23.11 |             100.00 |

| arm       | sex    | mdquality.s | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:----------|:-------|:------------|-----:|----------------:|--------:|-------------------:|
| F: FOLFOX | Female | 0           |   21 |              21 |    7.50 |               7.50 |
|           |        | 1           |  198 |             219 |   70.71 |              78.21 |
|           |        | NA          |   61 |             280 |   21.79 |             100.00 |

| arm     | sex  | mdquality.s | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:--------|:-----|:------------|-----:|----------------:|--------:|-------------------:|
| G: IROX | Male | 0           |   17 |              17 |    7.46 |               7.46 |
|         |      | 1           |  187 |             204 |   82.02 |              89.47 |
|         |      | NA          |   24 |             228 |   10.53 |             100.00 |

| arm     | sex    | mdquality.s | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:--------|:-------|:------------|-----:|----------------:|--------:|-------------------:|
| G: IROX | Female | 0           |   14 |              14 |    9.21 |               9.21 |
|         |        | 1           |  121 |             135 |   79.61 |              88.82 |
|         |        | NA          |   17 |             152 |   11.18 |             100.00 |

``` r

#using the single = TRUE argument will collapse results into a single table for printing
summary(example3, single = TRUE)
```

| arm       | sex    | mdquality.s | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:----------|:-------|:------------|-----:|----------------:|--------:|-------------------:|
| A: IFL    | Male   | 0           |   29 |              29 |   10.47 |              10.47 |
|           |        | 1           |  214 |             243 |   77.26 |              87.73 |
|           |        | NA          |   34 |             277 |   12.27 |             100.00 |
|           | Female | 0           |   12 |              12 |    7.95 |               7.95 |
|           |        | 1           |  118 |             130 |   78.15 |              86.09 |
|           |        | NA          |   21 |             151 |   13.91 |             100.00 |
| F: FOLFOX | Male   | 0           |   31 |              31 |    7.54 |               7.54 |
|           |        | 1           |  285 |             316 |   69.34 |              76.89 |
|           |        | NA          |   95 |             411 |   23.11 |             100.00 |
|           | Female | 0           |   21 |              21 |    7.50 |               7.50 |
|           |        | 1           |  198 |             219 |   70.71 |              78.21 |
|           |        | NA          |   61 |             280 |   21.79 |             100.00 |
| G: IROX   | Male   | 0           |   17 |              17 |    7.46 |               7.46 |
|           |        | 1           |  187 |             204 |   82.02 |              89.47 |
|           |        | NA          |   24 |             228 |   10.53 |             100.00 |
|           | Female | 0           |   14 |              14 |    9.21 |               9.21 |
|           |        | 1           |  121 |             135 |   79.61 |              88.82 |
|           |        | NA          |   17 |             152 |   11.18 |             100.00 |

### Show only the “n” most common combinations in each table (`head()` and `sort()`)

You can now sort
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
objects, and, by taking the
[`head()`](https://rdrr.io/r/utils/head.html) of the summary, output the
most common frequencies. This looks the prettiest with `dupLabels=TRUE`.

``` r

head(summary(sort(example1, decreasing = TRUE), dupLabels = TRUE))
```



    |arm       |sex    |mdquality.s | Freq| Cumulative Freq| Percent| Cumulative Percent|
    |:---------|:------|:-----------|----:|---------------:|-------:|------------------:|
    |F: FOLFOX |Male   |1           |  285|             285|   19.01|              19.01|
    |A: IFL    |Male   |1           |  214|             499|   14.28|              33.29|
    |F: FOLFOX |Female |1           |  198|             697|   13.21|              46.50|
    |G: IROX   |Male   |1           |  187|             884|   12.47|              58.97|
    |G: IROX   |Female |1           |  121|            1005|    8.07|              67.04|
    |A: IFL    |Female |1           |  118|            1123|    7.87|              74.92|

### Change labels on the fly

``` r

labs <- c(arm = "Arm", sex = "Sex", mdquality.s = "QOL", freqPercent = "%")
labels(example1) <- labs
summary(example1)
```

| Arm       | Sex    | QOL | Freq | Cumulative Freq |     % | Cumulative Percent |
|:----------|:-------|:----|-----:|----------------:|------:|-------------------:|
| A: IFL    | Male   | 0   |   29 |              29 |  1.93 |               1.93 |
|           |        | 1   |  214 |             243 | 14.28 |              16.21 |
|           |        | NA  |   34 |             277 |  2.27 |              18.48 |
|           | Female | 0   |   12 |             289 |  0.80 |              19.28 |
|           |        | 1   |  118 |             407 |  7.87 |              27.15 |
|           |        | NA  |   21 |             428 |  1.40 |              28.55 |
| F: FOLFOX | Male   | 0   |   31 |             459 |  2.07 |              30.62 |
|           |        | 1   |  285 |             744 | 19.01 |              49.63 |
|           |        | NA  |   95 |             839 |  6.34 |              55.97 |
|           | Female | 0   |   21 |             860 |  1.40 |              57.37 |
|           |        | 1   |  198 |            1058 | 13.21 |              70.58 |
|           |        | NA  |   61 |            1119 |  4.07 |              74.65 |
| G: IROX   | Male   | 0   |   17 |            1136 |  1.13 |              75.78 |
|           |        | 1   |  187 |            1323 | 12.47 |              88.26 |
|           |        | NA  |   24 |            1347 |  1.60 |              89.86 |
|           | Female | 0   |   14 |            1361 |  0.93 |              90.79 |
|           |        | 1   |  121 |            1482 |  8.07 |              98.87 |
|           |        | NA  |   17 |            1499 |  1.13 |             100.00 |

You can also supply `labelTranslations=` to
[`summary()`](https://rdrr.io/r/base/summary.html).

``` r

summary(example1, labelTranslations = labs)
```

| Arm       | Sex    | QOL | Freq | Cumulative Freq |     % | Cumulative Percent |
|:----------|:-------|:----|-----:|----------------:|------:|-------------------:|
| A: IFL    | Male   | 0   |   29 |              29 |  1.93 |               1.93 |
|           |        | 1   |  214 |             243 | 14.28 |              16.21 |
|           |        | NA  |   34 |             277 |  2.27 |              18.48 |
|           | Female | 0   |   12 |             289 |  0.80 |              19.28 |
|           |        | 1   |  118 |             407 |  7.87 |              27.15 |
|           |        | NA  |   21 |             428 |  1.40 |              28.55 |
| F: FOLFOX | Male   | 0   |   31 |             459 |  2.07 |              30.62 |
|           |        | 1   |  285 |             744 | 19.01 |              49.63 |
|           |        | NA  |   95 |             839 |  6.34 |              55.97 |
|           | Female | 0   |   21 |             860 |  1.40 |              57.37 |
|           |        | 1   |  198 |            1058 | 13.21 |              70.58 |
|           |        | NA  |   61 |            1119 |  4.07 |              74.65 |
| G: IROX   | Male   | 0   |   17 |            1136 |  1.13 |              75.78 |
|           |        | 1   |  187 |            1323 | 12.47 |              88.26 |
|           |        | NA  |   24 |            1347 |  1.60 |              89.86 |
|           | Female | 0   |   14 |            1361 |  0.93 |              90.79 |
|           |        | 1   |  121 |            1482 |  8.07 |              98.87 |
|           |        | NA  |   17 |            1499 |  1.13 |             100.00 |

### Using `xtable()` to format and print `freqlist()` results

Fair warning: [`xtable()`](https://rdrr.io/pkg/xtable/man/xtable.html)
has kind of a steep learning curve. These examples are given without
explanation, for more advanced users.

``` r

require(xtable)
```

    Loading required package: xtable

``` r

# set up custom function for xtable text
italic <- function(x) paste0('<i>', x, '</i>')

xftbl <- xtable(as.data.frame(summary(example1)), 
  caption = "xtable formatted output of freqlist data frame", align="|r|r|r|r|c|c|c|r|")

# change the column names
names(xftbl)[1:3] <- c("Arm", "Gender", "LASA QOL")

print(xftbl, sanitize.colnames.function = italic, include.rownames = FALSE, type = "html", comment = FALSE)
```

| *Arm* | *Gender* | *LASA QOL* | *Freq* | *Cumulative Freq* | *%* | *Cumulative Percent* |
|---:|---:|---:|:--:|:--:|:--:|---:|
| A: IFL | Male | 0 | 29 | 29 | 1.93 | 1.93 |
|  |  | 1 | 214 | 243 | 14.28 | 16.21 |
|  |  |  | 34 | 277 | 2.27 | 18.48 |
|  | Female | 0 | 12 | 289 | 0.80 | 19.28 |
|  |  | 1 | 118 | 407 | 7.87 | 27.15 |
|  |  |  | 21 | 428 | 1.40 | 28.55 |
| F: FOLFOX | Male | 0 | 31 | 459 | 2.07 | 30.62 |
|  |  | 1 | 285 | 744 | 19.01 | 49.63 |
|  |  |  | 95 | 839 | 6.34 | 55.97 |
|  | Female | 0 | 21 | 860 | 1.40 | 57.37 |
|  |  | 1 | 198 | 1058 | 13.21 | 70.58 |
|  |  |  | 61 | 1119 | 4.07 | 74.65 |
| G: IROX | Male | 0 | 17 | 1136 | 1.13 | 75.78 |
|  |  | 1 | 187 | 1323 | 12.47 | 88.26 |
|  |  |  | 24 | 1347 | 1.60 | 89.86 |
|  | Female | 0 | 14 | 1361 | 0.93 | 90.79 |
|  |  | 1 | 121 | 1482 | 8.07 | 98.87 |
|  |  |  | 17 | 1499 | 1.13 | 100.00 |

xtable formatted output of freqlist data frame {.table border="1"}

### Use `freqlist` in bookdown

Since the backbone of
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
is [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html), tables
still render well in bookdown. However, `print.summary.freqlist()`
doesn’t use the `caption=` argument of `kable()`, so some tables may not
have a properly numbered caption. To fix this, use the method described
[on the bookdown site](https://pkg.yihui.org/bookdown/tables.html) to
give the table a tag/ID.

``` r

summary(freqlist(~ sex + age, data = mockstudy), title="(\\#tab:mytableby) Caption here")
```

## Appendix: Notes regarding table options in R

### NAs

There are several widely used options for basic tables in R. The
[`table()`](https://rdrr.io/r/base/table.html) function in base R is
probably the most common; by default it excludes NA values. You can
change NA handling in
[`base::table()`](https://rdrr.io/r/base/table.html) using the `useNA=`
or `exclude=` arguments.

``` r

# base table default removes NAs
tab.d1 <- base::table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA="ifany")
tab.d1
```

    , , mdquality.s = 0

               sex
    arm         Male Female
      A: IFL      29     12
      F: FOLFOX   31     21
      G: IROX     17     14

    , , mdquality.s = 1

               sex
    arm         Male Female
      A: IFL     214    118
      F: FOLFOX  285    198
      G: IROX    187    121

    , , mdquality.s = NA

               sex
    arm         Male Female
      A: IFL      34     21
      F: FOLFOX   95     61
      G: IROX     24     17

[`xtabs()`](https://rdrr.io/r/stats/xtabs.html) is similar to
[`table()`](https://rdrr.io/r/base/table.html), but uses a formula-based
syntax. However, NAs must be explicitly added to each factor using the
[`addNA()`](https://rdrr.io/r/base/factor.html) function or using the
argument `addNA = TRUE`.

``` r

# without specifying addNA
tab.d2 <- xtabs(formula = ~ arm + sex + mdquality.s, data = mockstudy)
tab.d2
```

    , , mdquality.s = 0

               sex
    arm         Male Female
      A: IFL      29     12
      F: FOLFOX   31     21
      G: IROX     17     14

    , , mdquality.s = 1

               sex
    arm         Male Female
      A: IFL     214    118
      F: FOLFOX  285    198
      G: IROX    187    121

``` r

# now with addNA
tab.d3 <- xtabs(~ arm + sex + addNA(mdquality.s), data = mockstudy)
tab.d3
```

    , , addNA(mdquality.s) = 0

               sex
    arm         Male Female
      A: IFL      29     12
      F: FOLFOX   31     21
      G: IROX     17     14

    , , addNA(mdquality.s) = 1

               sex
    arm         Male Female
      A: IFL     214    118
      F: FOLFOX  285    198
      G: IROX    187    121

    , , addNA(mdquality.s) = NA

               sex
    arm         Male Female
      A: IFL      34     21
      F: FOLFOX   95     61
      G: IROX     24     17

Since the formula method of
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
uses [`xtabs()`](https://rdrr.io/r/stats/xtabs.html), NAs should be
treated in the same way.
[`includeNA()`](https://mayoverse.github.io/arsenal/reference/NA.operations.md)
can also be helpful here for setting explicit NA values.

### Table dimname names (dnn)

Supplying a data.frame to the
[`table()`](https://rdrr.io/r/base/table.html) function without giving
columns individually will create a contingency table using all variables
in the data.frame.

However, if the columns of a data.frame or matrix are supplied
separately (i.e., as vectors), column names will not be preserved.

``` r

# providing variables separately (as vectors) drops column names
table(mockstudy$arm, mockstudy$sex, mockstudy$mdquality.s)
```

    , ,  = 0

               
                Male Female
      A: IFL      29     12
      F: FOLFOX   31     21
      G: IROX     17     14

    , ,  = 1

               
                Male Female
      A: IFL     214    118
      F: FOLFOX  285    198
      G: IROX    187    121

If desired, you can use the `dnn=` argument to pass variable names.

``` r

# add the column name labels back using dnn option in base::table
table(mockstudy$arm, mockstudy$sex, mockstudy$mdquality.s, dnn=c("Arm", "Sex", "QOL"))
```

    , , QOL = 0

               Sex
    Arm         Male Female
      A: IFL      29     12
      F: FOLFOX   31     21
      G: IROX     17     14

    , , QOL = 1

               Sex
    Arm         Male Female
      A: IFL     214    118
      F: FOLFOX  285    198
      G: IROX    187    121

You can also name the arguments to
[`table()`](https://rdrr.io/r/base/table.html):

``` r

table(Arm = mockstudy$arm, Sex = mockstudy$sex, QOL = mockstudy$mdquality.s)
```

    , , QOL = 0

               Sex
    Arm         Male Female
      A: IFL      29     12
      F: FOLFOX   31     21
      G: IROX     17     14

    , , QOL = 1

               Sex
    Arm         Male Female
      A: IFL     214    118
      F: FOLFOX  285    198
      G: IROX    187    121

If using
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md),
you can provide the labels directly to
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
or to [`summary()`](https://rdrr.io/r/base/summary.html) using
`labelTranslations=`.
