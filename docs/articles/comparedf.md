# The comparedf function

## Introduction

The
[`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md)
function can be used to determine and report differences between two
data.frames. It was written in the spirit of replacing `PROC COMPARE`
from SAS.

``` r

library(arsenal)
```

Why “comparedf”? We originally called this function
`compare.data.frame()`, using
[`testthat::compare()`](https://testthat.r-lib.org/reference/compare.html)
as our S3 generic, but that ended up getting us in trouble because of
conflicting object structures. Why this didn’t occur to us at the time
remains a mystery. To replace it, we brainstormed several ideas
([`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md),
`dfcompare()`, `collate()`, `comparison()`) but settled on the former
for three reasons:

1.  There were no other objects with that generic or class (see
    [`testthat::compare()`](https://testthat.r-lib.org/reference/compare.html)
    and `compare::compare()`).

2.  It is mnemonically easy to remember (we “compare data.frames”, not
    “data.frames compare”).

3.  It tab auto-completes from the original “compare”.

## Basic examples

We first build two similar data.frames to compare.

``` r

df1 <- data.frame(id = paste0("person", 1:3),
                  a = c("a", "b", "c"),
                  b = c(1, 3, 4),
                  c = c("f", "e", "d"),
                  row.names = paste0("rn", 1:3),
                  stringsAsFactors = FALSE)
df2 <- data.frame(id = paste0("person", 3:1),
                  a = c("c", "b", "a"),
                  b = c(1, 3, 4),
                  d = paste0("rn", 1:3),
                  row.names = paste0("rn", c(1,3,2)),
                  stringsAsFactors = FALSE)
```

To compare these datasets, simply pass them to the
[`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md)
function:

``` r

comparedf(df1, df2)
```

    Compare Object

    Function Call: 
    comparedf(x = df1, y = df2)

    Shared: 3 non-by variables and 3 observations.
    Not shared: 2 variables and 0 observations.

    Differences found in 2/3 variables compared.
    0 variables compared have non-identical attributes.

Use [`summary()`](https://rdrr.io/r/base/summary.html) to get a more
detailed summary

``` r

summary(comparedf(df1, df2))
```

| version | arg | ncol | nrow |
|:--------|:----|-----:|-----:|
| x       | df1 |    4 |    3 |
| y       | df2 |    4 |    3 |

Summary of data.frames {.table}

| statistic                                                   | value |
|:------------------------------------------------------------|------:|
| Number of by-variables                                      |     0 |
| Number of non-by variables in common                        |     3 |
| Number of variables compared                                |     3 |
| Number of variables in x but not y                          |     1 |
| Number of variables in y but not x                          |     1 |
| Number of variables compared with some values unequal       |     2 |
| Number of variables compared with all values equal          |     1 |
| Number of observations in common                            |     3 |
| Number of observations in x but not y                       |     0 |
| Number of observations in y but not x                       |     0 |
| Number of observations with some compared variables unequal |     2 |
| Number of observations with all compared variables equal    |     1 |
| Number of values unequal                                    |     4 |

Summary of overall comparison {.table}

| version | variable | position | class     |
|:--------|:---------|---------:|:----------|
| x       | c        |        4 | character |
| y       | d        |        4 | character |

Variables not shared {.table}

|                                 |
|:--------------------------------|
| No other variables not compared |

Other variables not compared {.table}

|                            |
|:---------------------------|
| No observations not shared |

Observations not shared {.table}

| var.x | var.y |   n | NAs |
|:------|:------|----:|----:|
| id    | id    |   2 |   0 |
| a     | a     |   2 |   0 |
| b     | b     |   0 |   0 |

Differences detected by variable {.table}

| var.x | var.y | ..row.names.. | values.x | values.y | row.x | row.y |
|:------|:------|--------------:|:---------|:---------|------:|------:|
| id    | id    |             1 | person1  | person3  |     1 |     1 |
| id    | id    |             3 | person3  | person1  |     3 |     3 |
| a     | a     |             1 | a        | c        |     1 |     1 |
| a     | a     |             3 | c        | a        |     3 |     3 |

Differences detected {.table}

|                             |
|:----------------------------|
| No non-identical attributes |

Non-identical attributes {.table}

By default, the datasets are compared row-by-row. To change this, use
the `by=` or `by.x=` and `by.y=` arguments:

``` r

summary(comparedf(df1, df2, by = "id"))
```

| version | arg | ncol | nrow |
|:--------|:----|-----:|-----:|
| x       | df1 |    4 |    3 |
| y       | df2 |    4 |    3 |

Summary of data.frames {.table}

| statistic                                                   | value |
|:------------------------------------------------------------|------:|
| Number of by-variables                                      |     1 |
| Number of non-by variables in common                        |     2 |
| Number of variables compared                                |     2 |
| Number of variables in x but not y                          |     1 |
| Number of variables in y but not x                          |     1 |
| Number of variables compared with some values unequal       |     1 |
| Number of variables compared with all values equal          |     1 |
| Number of observations in common                            |     3 |
| Number of observations in x but not y                       |     0 |
| Number of observations in y but not x                       |     0 |
| Number of observations with some compared variables unequal |     2 |
| Number of observations with all compared variables equal    |     1 |
| Number of values unequal                                    |     2 |

Summary of overall comparison {.table}

| version | variable | position | class     |
|:--------|:---------|---------:|:----------|
| x       | c        |        4 | character |
| y       | d        |        4 | character |

Variables not shared {.table}

|                                 |
|:--------------------------------|
| No other variables not compared |

Other variables not compared {.table}

|                            |
|:---------------------------|
| No observations not shared |

Observations not shared {.table}

| var.x | var.y |   n | NAs |
|:------|:------|----:|----:|
| a     | a     |   0 |   0 |
| b     | b     |   2 |   0 |

Differences detected by variable {.table}

| var.x | var.y | id      | values.x | values.y | row.x | row.y |
|:------|:------|:--------|:---------|:---------|------:|------:|
| b     | b     | person1 | 1        | 4        |     1 |     3 |
| b     | b     | person3 | 4        | 1        |     3 |     1 |

Differences detected {.table}

|                             |
|:----------------------------|
| No non-identical attributes |

Non-identical attributes {.table}

## A larger example

Let’s muck up the `mockstudy` data.

``` r

data(mockstudy)
mockstudy2 <- muck_up_mockstudy()
```

We’ve changed row order, so let’s compare by the case ID:

``` r

summary(comparedf(mockstudy, mockstudy2, by = "case"))
```

| version | arg        | ncol | nrow |
|:--------|:-----------|-----:|-----:|
| x       | mockstudy  |   14 | 1499 |
| y       | mockstudy2 |   13 | 1495 |

Summary of data.frames {.table}

| statistic                                                   | value |
|:------------------------------------------------------------|------:|
| Number of by-variables                                      |     1 |
| Number of non-by variables in common                        |     9 |
| Number of variables compared                                |     7 |
| Number of variables in x but not y                          |     4 |
| Number of variables in y but not x                          |     3 |
| Number of variables compared with some values unequal       |     3 |
| Number of variables compared with all values equal          |     4 |
| Number of observations in common                            |  1495 |
| Number of observations in x but not y                       |     4 |
| Number of observations in y but not x                       |     0 |
| Number of observations with some compared variables unequal |  1495 |
| Number of observations with all compared variables equal    |     0 |
| Number of values unequal                                    |  1762 |

Summary of overall comparison {.table}

| version | variable | position | class     |
|:--------|:---------|---------:|:----------|
| x       | age      |        2 | integer   |
| x       | arm      |        3 | character |
| x       | fu.time  |        6 | integer   |
| x       | fu.stat  |        7 | integer   |
| y       | fu_time  |       11 | integer   |
| y       | fu stat  |       12 | integer   |
| y       | Arm      |       13 | character |

Variables not shared {.table}

| var.x | pos.x | class.x   | var.y | pos.y | class.y |
|:------|------:|:----------|:------|------:|:--------|
| race  |     5 | character | race  |     3 | factor  |
| ast   |    12 | integer   | ast   |     8 | numeric |

Other variables not compared {.table}

| version |   case | observation |
|:--------|-------:|------------:|
| x       |  88989 |           9 |
| x       |  90158 |           8 |
| x       |  99508 |           7 |
| x       | 112263 |           5 |

Observations not shared {.table}

| var.x       | var.y       |    n | NAs |
|:------------|:------------|-----:|----:|
| sex         | sex         | 1495 |   0 |
| ps          | ps          |    1 |   1 |
| hgb         | hgb         |  266 | 266 |
| bmi         | bmi         |    0 |   0 |
| alk.phos    | alk.phos    |    0 |   0 |
| mdquality.s | mdquality.s |    0 |   0 |
| age.ord     | age.ord     |    0 |   0 |

Differences detected by variable {.table}

| var.x | var.y |  case | values.x | values.y | row.x | row.y |
|:------|:------|------:|:---------|:---------|------:|------:|
| sex   | sex   | 76170 | Male     | Male     |    26 |    20 |
| sex   | sex   | 76240 | Male     | Male     |    27 |    21 |
| sex   | sex   | 76431 | Female   | Female   |    28 |    22 |
| sex   | sex   | 76712 | Male     | Male     |    29 |    23 |
| sex   | sex   | 76780 | Female   | Female   |    30 |    24 |
| sex   | sex   | 77066 | Female   | Female   |    31 |    25 |
| sex   | sex   | 77316 | Male     | Male     |    32 |    26 |
| sex   | sex   | 77355 | Male     | Male     |    33 |    27 |
| sex   | sex   | 77591 | Male     | Male     |    34 |    28 |
| sex   | sex   | 77851 | Male     | Male     |    35 |    29 |
| ps    | ps    | 86205 | 0        | NA       |     6 |     3 |
| hgb   | hgb   | 88714 | NA       | -9       |   192 |   186 |
| hgb   | hgb   | 88955 | NA       | -9       |   204 |   198 |
| hgb   | hgb   | 89549 | NA       | -9       |   229 |   223 |
| hgb   | hgb   | 89563 | NA       | -9       |   231 |   225 |
| hgb   | hgb   | 89584 | NA       | -9       |   237 |   231 |
| hgb   | hgb   | 89591 | NA       | -9       |   238 |   232 |
| hgb   | hgb   | 89595 | NA       | -9       |   239 |   233 |
| hgb   | hgb   | 89647 | NA       | -9       |   243 |   237 |
| hgb   | hgb   | 89665 | NA       | -9       |   244 |   238 |
| hgb   | hgb   | 89827 | NA       | -9       |   255 |   249 |

Differences detected (1741 not shown) {.table}

| var.x | var.y | name   |
|:------|:------|:-------|
| sex   | sex   | label  |
| sex   | sex   | levels |
| race  | race  | class  |
| race  | race  | label  |
| race  | race  | levels |
| bmi   | bmi   | label  |

Non-identical attributes {.table}

## Column name comparison options

It is possible to change which column names are considered “the same
variable”.

### Ignoring case

For example, to ignore case in variable names (so that `Arm` and `arm`
are considered the same), pass `tol.vars = "case"`.

You can do this using
[`comparedf.control()`](https://mayoverse.github.io/arsenal/reference/comparedf.control.md)

``` r

summary(comparedf(mockstudy, mockstudy2, by = "case", control = comparedf.control(tol.vars = "case")))
```

or pass it through the `...` arguments.

``` r

summary(comparedf(mockstudy, mockstudy2, by = "case", tol.vars = "case"))
```

| version | arg        | ncol | nrow |
|:--------|:-----------|-----:|-----:|
| x       | mockstudy  |   14 | 1499 |
| y       | mockstudy2 |   13 | 1495 |

Summary of data.frames {.table}

| statistic                                                   | value |
|:------------------------------------------------------------|------:|
| Number of by-variables                                      |     1 |
| Number of non-by variables in common                        |    10 |
| Number of variables compared                                |     8 |
| Number of variables in x but not y                          |     3 |
| Number of variables in y but not x                          |     2 |
| Number of variables compared with some values unequal       |     3 |
| Number of variables compared with all values equal          |     5 |
| Number of observations in common                            |  1495 |
| Number of observations in x but not y                       |     4 |
| Number of observations in y but not x                       |     0 |
| Number of observations with some compared variables unequal |  1495 |
| Number of observations with all compared variables equal    |     0 |
| Number of values unequal                                    |  1762 |

Summary of overall comparison {.table}

| version | variable | position | class   |
|:--------|:---------|---------:|:--------|
| x       | age      |        2 | integer |
| x       | fu.time  |        6 | integer |
| x       | fu.stat  |        7 | integer |
| y       | fu_time  |       11 | integer |
| y       | fu stat  |       12 | integer |

Variables not shared {.table}

| var.x | pos.x | class.x   | var.y | pos.y | class.y |
|:------|------:|:----------|:------|------:|:--------|
| race  |     5 | character | race  |     3 | factor  |
| ast   |    12 | integer   | ast   |     8 | numeric |

Other variables not compared {.table}

| version |   case | observation |
|:--------|-------:|------------:|
| x       |  88989 |           9 |
| x       |  90158 |           8 |
| x       |  99508 |           7 |
| x       | 112263 |           5 |

Observations not shared {.table}

| var.x       | var.y       |    n | NAs |
|:------------|:------------|-----:|----:|
| arm         | Arm         |    0 |   0 |
| sex         | sex         | 1495 |   0 |
| ps          | ps          |    1 |   1 |
| hgb         | hgb         |  266 | 266 |
| bmi         | bmi         |    0 |   0 |
| alk.phos    | alk.phos    |    0 |   0 |
| mdquality.s | mdquality.s |    0 |   0 |
| age.ord     | age.ord     |    0 |   0 |

Differences detected by variable {.table}

| var.x | var.y |  case | values.x | values.y | row.x | row.y |
|:------|:------|------:|:---------|:---------|------:|------:|
| sex   | sex   | 76170 | Male     | Male     |    26 |    20 |
| sex   | sex   | 76240 | Male     | Male     |    27 |    21 |
| sex   | sex   | 76431 | Female   | Female   |    28 |    22 |
| sex   | sex   | 76712 | Male     | Male     |    29 |    23 |
| sex   | sex   | 76780 | Female   | Female   |    30 |    24 |
| sex   | sex   | 77066 | Female   | Female   |    31 |    25 |
| sex   | sex   | 77316 | Male     | Male     |    32 |    26 |
| sex   | sex   | 77355 | Male     | Male     |    33 |    27 |
| sex   | sex   | 77591 | Male     | Male     |    34 |    28 |
| sex   | sex   | 77851 | Male     | Male     |    35 |    29 |
| ps    | ps    | 86205 | 0        | NA       |     6 |     3 |
| hgb   | hgb   | 88714 | NA       | -9       |   192 |   186 |
| hgb   | hgb   | 88955 | NA       | -9       |   204 |   198 |
| hgb   | hgb   | 89549 | NA       | -9       |   229 |   223 |
| hgb   | hgb   | 89563 | NA       | -9       |   231 |   225 |
| hgb   | hgb   | 89584 | NA       | -9       |   237 |   231 |
| hgb   | hgb   | 89591 | NA       | -9       |   238 |   232 |
| hgb   | hgb   | 89595 | NA       | -9       |   239 |   233 |
| hgb   | hgb   | 89647 | NA       | -9       |   243 |   237 |
| hgb   | hgb   | 89665 | NA       | -9       |   244 |   238 |
| hgb   | hgb   | 89827 | NA       | -9       |   255 |   249 |

Differences detected (1741 not shown) {.table}

| var.x | var.y | name   |
|:------|:------|:-------|
| arm   | Arm   | label  |
| sex   | sex   | label  |
| sex   | sex   | levels |
| race  | race  | class  |
| race  | race  | label  |
| race  | race  | levels |
| bmi   | bmi   | label  |

Non-identical attributes {.table}

### Treating dots and underscores the same (equivalence classes)

It is possible to treat certain characters or sets of characters as the
same by passing a character vector of equivalence classes to the
`tol.vars=` argument.

In short, each string in the vector is split into single characters, and
the resulting set of characters is replaced by the first character in
the string. For example, passing `c("._")` would replace all underscores
with dots in the column names of both datasets. Similarly, passing
`c("aA", "BbCc")` would replace all instances of `"A"` with `"a"` and
all instances of `"b"`, `"C"`, or `"c"` with `"B"`. This is one way to
ignore case for certain letters. Otherwise, it’s possible to combine the
equivalence classes with ignoring case, by passing (e.g.)
`c("._", "case")`.

Passing a single character as an element this vector will replace that
character with the empty string. For example, passing c(” “,”.”) would
remove all spaces and dots from the column names.

For mockstudy, let’s treat dots, underscores, and spaces as the same,
and ignore case:

``` r

summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c("._ ", "case") # dots=underscores=spaces, ignore case
))
```

| version | arg        | ncol | nrow |
|:--------|:-----------|-----:|-----:|
| x       | mockstudy  |   14 | 1499 |
| y       | mockstudy2 |   13 | 1495 |

Summary of data.frames {.table}

| statistic                                                   | value |
|:------------------------------------------------------------|------:|
| Number of by-variables                                      |     1 |
| Number of non-by variables in common                        |    12 |
| Number of variables compared                                |    10 |
| Number of variables in x but not y                          |     1 |
| Number of variables in y but not x                          |     0 |
| Number of variables compared with some values unequal       |     3 |
| Number of variables compared with all values equal          |     7 |
| Number of observations in common                            |  1495 |
| Number of observations in x but not y                       |     4 |
| Number of observations in y but not x                       |     0 |
| Number of observations with some compared variables unequal |  1495 |
| Number of observations with all compared variables equal    |     0 |
| Number of values unequal                                    |  1762 |

Summary of overall comparison {.table}

| version | variable | position | class   |
|:--------|:---------|---------:|:--------|
| x       | age      |        2 | integer |

Variables not shared {.table}

| var.x | pos.x | class.x   | var.y | pos.y | class.y |
|:------|------:|:----------|:------|------:|:--------|
| race  |     5 | character | race  |     3 | factor  |
| ast   |    12 | integer   | ast   |     8 | numeric |

Other variables not compared {.table}

| version |   case | observation |
|:--------|-------:|------------:|
| x       |  88989 |           9 |
| x       |  90158 |           8 |
| x       |  99508 |           7 |
| x       | 112263 |           5 |

Observations not shared {.table}

| var.x       | var.y       |    n | NAs |
|:------------|:------------|-----:|----:|
| arm         | Arm         |    0 |   0 |
| sex         | sex         | 1495 |   0 |
| fu.time     | fu_time     |    0 |   0 |
| fu.stat     | fu stat     |    0 |   0 |
| ps          | ps          |    1 |   1 |
| hgb         | hgb         |  266 | 266 |
| bmi         | bmi         |    0 |   0 |
| alk.phos    | alk.phos    |    0 |   0 |
| mdquality.s | mdquality.s |    0 |   0 |
| age.ord     | age.ord     |    0 |   0 |

Differences detected by variable {.table}

| var.x | var.y |  case | values.x | values.y | row.x | row.y |
|:------|:------|------:|:---------|:---------|------:|------:|
| sex   | sex   | 76170 | Male     | Male     |    26 |    20 |
| sex   | sex   | 76240 | Male     | Male     |    27 |    21 |
| sex   | sex   | 76431 | Female   | Female   |    28 |    22 |
| sex   | sex   | 76712 | Male     | Male     |    29 |    23 |
| sex   | sex   | 76780 | Female   | Female   |    30 |    24 |
| sex   | sex   | 77066 | Female   | Female   |    31 |    25 |
| sex   | sex   | 77316 | Male     | Male     |    32 |    26 |
| sex   | sex   | 77355 | Male     | Male     |    33 |    27 |
| sex   | sex   | 77591 | Male     | Male     |    34 |    28 |
| sex   | sex   | 77851 | Male     | Male     |    35 |    29 |
| ps    | ps    | 86205 | 0        | NA       |     6 |     3 |
| hgb   | hgb   | 88714 | NA       | -9       |   192 |   186 |
| hgb   | hgb   | 88955 | NA       | -9       |   204 |   198 |
| hgb   | hgb   | 89549 | NA       | -9       |   229 |   223 |
| hgb   | hgb   | 89563 | NA       | -9       |   231 |   225 |
| hgb   | hgb   | 89584 | NA       | -9       |   237 |   231 |
| hgb   | hgb   | 89591 | NA       | -9       |   238 |   232 |
| hgb   | hgb   | 89595 | NA       | -9       |   239 |   233 |
| hgb   | hgb   | 89647 | NA       | -9       |   243 |   237 |
| hgb   | hgb   | 89665 | NA       | -9       |   244 |   238 |
| hgb   | hgb   | 89827 | NA       | -9       |   255 |   249 |

Differences detected (1741 not shown) {.table}

| var.x | var.y | name   |
|:------|:------|:-------|
| arm   | Arm   | label  |
| sex   | sex   | label  |
| sex   | sex   | levels |
| race  | race  | class  |
| race  | race  | label  |
| race  | race  | levels |
| bmi   | bmi   | label  |

Non-identical attributes {.table}

### Manually specifying columns to match together

If you pass a named vector to the `tol.vars=` argument,
[`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md)
will line up the names of that vector to the column names of `x` and the
values of that vector to the column names of `y`. In this way, you can
manually specify which non-identically-named columns to compare.

For mockstudy, let’s specify our variables manually in this way:

``` r

summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c(arm = "Arm", fu.stat = "fu stat", fu.time = "fu_time")
))
```

| version | arg        | ncol | nrow |
|:--------|:-----------|-----:|-----:|
| x       | mockstudy  |   14 | 1499 |
| y       | mockstudy2 |   13 | 1495 |

Summary of data.frames {.table}

| statistic                                                   | value |
|:------------------------------------------------------------|------:|
| Number of by-variables                                      |     1 |
| Number of non-by variables in common                        |    12 |
| Number of variables compared                                |    10 |
| Number of variables in x but not y                          |     1 |
| Number of variables in y but not x                          |     0 |
| Number of variables compared with some values unequal       |     3 |
| Number of variables compared with all values equal          |     7 |
| Number of observations in common                            |  1495 |
| Number of observations in x but not y                       |     4 |
| Number of observations in y but not x                       |     0 |
| Number of observations with some compared variables unequal |  1495 |
| Number of observations with all compared variables equal    |     0 |
| Number of values unequal                                    |  1762 |

Summary of overall comparison {.table}

| version | variable | position | class   |
|:--------|:---------|---------:|:--------|
| x       | age      |        2 | integer |

Variables not shared {.table}

| var.x | pos.x | class.x   | var.y | pos.y | class.y |
|:------|------:|:----------|:------|------:|:--------|
| race  |     5 | character | race  |     3 | factor  |
| ast   |    12 | integer   | ast   |     8 | numeric |

Other variables not compared {.table}

| version |   case | observation |
|:--------|-------:|------------:|
| x       |  88989 |           9 |
| x       |  90158 |           8 |
| x       |  99508 |           7 |
| x       | 112263 |           5 |

Observations not shared {.table}

| var.x       | var.y       |    n | NAs |
|:------------|:------------|-----:|----:|
| arm         | Arm         |    0 |   0 |
| sex         | sex         | 1495 |   0 |
| fu.time     | fu_time     |    0 |   0 |
| fu.stat     | fu stat     |    0 |   0 |
| ps          | ps          |    1 |   1 |
| hgb         | hgb         |  266 | 266 |
| bmi         | bmi         |    0 |   0 |
| alk.phos    | alk.phos    |    0 |   0 |
| mdquality.s | mdquality.s |    0 |   0 |
| age.ord     | age.ord     |    0 |   0 |

Differences detected by variable {.table}

| var.x | var.y |  case | values.x | values.y | row.x | row.y |
|:------|:------|------:|:---------|:---------|------:|------:|
| sex   | sex   | 76170 | Male     | Male     |    26 |    20 |
| sex   | sex   | 76240 | Male     | Male     |    27 |    21 |
| sex   | sex   | 76431 | Female   | Female   |    28 |    22 |
| sex   | sex   | 76712 | Male     | Male     |    29 |    23 |
| sex   | sex   | 76780 | Female   | Female   |    30 |    24 |
| sex   | sex   | 77066 | Female   | Female   |    31 |    25 |
| sex   | sex   | 77316 | Male     | Male     |    32 |    26 |
| sex   | sex   | 77355 | Male     | Male     |    33 |    27 |
| sex   | sex   | 77591 | Male     | Male     |    34 |    28 |
| sex   | sex   | 77851 | Male     | Male     |    35 |    29 |
| ps    | ps    | 86205 | 0        | NA       |     6 |     3 |
| hgb   | hgb   | 88714 | NA       | -9       |   192 |   186 |
| hgb   | hgb   | 88955 | NA       | -9       |   204 |   198 |
| hgb   | hgb   | 89549 | NA       | -9       |   229 |   223 |
| hgb   | hgb   | 89563 | NA       | -9       |   231 |   225 |
| hgb   | hgb   | 89584 | NA       | -9       |   237 |   231 |
| hgb   | hgb   | 89591 | NA       | -9       |   238 |   232 |
| hgb   | hgb   | 89595 | NA       | -9       |   239 |   233 |
| hgb   | hgb   | 89647 | NA       | -9       |   243 |   237 |
| hgb   | hgb   | 89665 | NA       | -9       |   244 |   238 |
| hgb   | hgb   | 89827 | NA       | -9       |   255 |   249 |

Differences detected (1741 not shown) {.table}

| var.x | var.y | name   |
|:------|:------|:-------|
| arm   | Arm   | label  |
| sex   | sex   | label  |
| sex   | sex   | levels |
| race  | race  | class  |
| race  | race  | label  |
| race  | race  | levels |
| bmi   | bmi   | label  |

Non-identical attributes {.table}

## Column comparison options

### Logical tolerance

Use the `tol.logical=` argument to change how logicals are compared. By
default, they’re expected to be equal to each other.

### Numeric tolerance

To allow numeric differences of a certain tolerance, use the `tol.num=`
and `tol.num.val=` options. `tol.num.val=` determines the maximum
(unsigned) difference tolerated if `tol.num="absolute"` (default), and
determines the maximum (unsigned) percent difference tolerated if
`tol.num="percent"`.

Also note the option `int.as.num=`, which determines whether integers
and numerics should be compared despite their class difference. If
`TRUE`, the integers are coerced to numeric. Note that `mockstudy$ast`
is integer, while `mockstudy2$ast` is numeric:

``` r

summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c("._ ", "case"), # dots=underscores=spaces, ignore case
                int.as.num = TRUE            # compare integers and numerics
))
```

| version | arg        | ncol | nrow |
|:--------|:-----------|-----:|-----:|
| x       | mockstudy  |   14 | 1499 |
| y       | mockstudy2 |   13 | 1495 |

Summary of data.frames {.table}

| statistic                                                   | value |
|:------------------------------------------------------------|------:|
| Number of by-variables                                      |     1 |
| Number of non-by variables in common                        |    12 |
| Number of variables compared                                |    11 |
| Number of variables in x but not y                          |     1 |
| Number of variables in y but not x                          |     0 |
| Number of variables compared with some values unequal       |     4 |
| Number of variables compared with all values equal          |     7 |
| Number of observations in common                            |  1495 |
| Number of observations in x but not y                       |     4 |
| Number of observations in y but not x                       |     0 |
| Number of observations with some compared variables unequal |  1495 |
| Number of observations with all compared variables equal    |     0 |
| Number of values unequal                                    |  1765 |

Summary of overall comparison {.table}

| version | variable | position | class   |
|:--------|:---------|---------:|:--------|
| x       | age      |        2 | integer |

Variables not shared {.table}

| var.x | pos.x | class.x   | var.y | pos.y | class.y |
|:------|------:|:----------|:------|------:|:--------|
| race  |     5 | character | race  |     3 | factor  |

Other variables not compared {.table}

| version |   case | observation |
|:--------|-------:|------------:|
| x       |  88989 |           9 |
| x       |  90158 |           8 |
| x       |  99508 |           7 |
| x       | 112263 |           5 |

Observations not shared {.table}

| var.x       | var.y       |    n | NAs |
|:------------|:------------|-----:|----:|
| arm         | Arm         |    0 |   0 |
| sex         | sex         | 1495 |   0 |
| fu.time     | fu_time     |    0 |   0 |
| fu.stat     | fu stat     |    0 |   0 |
| ps          | ps          |    1 |   1 |
| hgb         | hgb         |  266 | 266 |
| bmi         | bmi         |    0 |   0 |
| alk.phos    | alk.phos    |    0 |   0 |
| ast         | ast         |    3 |   0 |
| mdquality.s | mdquality.s |    0 |   0 |
| age.ord     | age.ord     |    0 |   0 |

Differences detected by variable {.table}

| var.x | var.y |   case | values.x | values.y | row.x | row.y |
|:------|:------|-------:|:---------|:---------|------:|------:|
| sex   | sex   |  76170 | Male     | Male     |    26 |    20 |
| sex   | sex   |  76240 | Male     | Male     |    27 |    21 |
| sex   | sex   |  76431 | Female   | Female   |    28 |    22 |
| sex   | sex   |  76712 | Male     | Male     |    29 |    23 |
| sex   | sex   |  76780 | Female   | Female   |    30 |    24 |
| sex   | sex   |  77066 | Female   | Female   |    31 |    25 |
| sex   | sex   |  77316 | Male     | Male     |    32 |    26 |
| sex   | sex   |  77355 | Male     | Male     |    33 |    27 |
| sex   | sex   |  77591 | Male     | Male     |    34 |    28 |
| sex   | sex   |  77851 | Male     | Male     |    35 |    29 |
| ps    | ps    |  86205 | 0        | NA       |     6 |     3 |
| hgb   | hgb   |  88714 | NA       | -9       |   192 |   186 |
| hgb   | hgb   |  88955 | NA       | -9       |   204 |   198 |
| hgb   | hgb   |  89549 | NA       | -9       |   229 |   223 |
| hgb   | hgb   |  89563 | NA       | -9       |   231 |   225 |
| hgb   | hgb   |  89584 | NA       | -9       |   237 |   231 |
| hgb   | hgb   |  89591 | NA       | -9       |   238 |   232 |
| hgb   | hgb   |  89595 | NA       | -9       |   239 |   233 |
| hgb   | hgb   |  89647 | NA       | -9       |   243 |   237 |
| hgb   | hgb   |  89665 | NA       | -9       |   244 |   238 |
| hgb   | hgb   |  89827 | NA       | -9       |   255 |   249 |
| ast   | ast   |  86205 | 27       | 36       |     6 |     3 |
| ast   | ast   | 105271 | 100      | 36       |     3 |     2 |
| ast   | ast   | 110754 | 35       | 36       |     1 |     1 |

Differences detected (1741 not shown) {.table}

| var.x | var.y | name   |
|:------|:------|:-------|
| arm   | Arm   | label  |
| sex   | sex   | label  |
| sex   | sex   | levels |
| race  | race  | class  |
| race  | race  | label  |
| race  | race  | levels |
| bmi   | bmi   | label  |

Non-identical attributes {.table}

Suppose a tolerance of up to 10 is allowed for `ast`:

``` r

summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c("._ ", "case"), # dots=underscores=spaces, ignore case
                int.as.num = TRUE,           # compare integers and numerics
                tol.num.val = 10             # allow absolute differences <= 10
))
```

| version | arg        | ncol | nrow |
|:--------|:-----------|-----:|-----:|
| x       | mockstudy  |   14 | 1499 |
| y       | mockstudy2 |   13 | 1495 |

Summary of data.frames {.table}

| statistic                                                   | value |
|:------------------------------------------------------------|------:|
| Number of by-variables                                      |     1 |
| Number of non-by variables in common                        |    12 |
| Number of variables compared                                |    11 |
| Number of variables in x but not y                          |     1 |
| Number of variables in y but not x                          |     0 |
| Number of variables compared with some values unequal       |     4 |
| Number of variables compared with all values equal          |     7 |
| Number of observations in common                            |  1495 |
| Number of observations in x but not y                       |     4 |
| Number of observations in y but not x                       |     0 |
| Number of observations with some compared variables unequal |  1495 |
| Number of observations with all compared variables equal    |     0 |
| Number of values unequal                                    |  1763 |

Summary of overall comparison {.table}

| version | variable | position | class   |
|:--------|:---------|---------:|:--------|
| x       | age      |        2 | integer |

Variables not shared {.table}

| var.x | pos.x | class.x   | var.y | pos.y | class.y |
|:------|------:|:----------|:------|------:|:--------|
| race  |     5 | character | race  |     3 | factor  |

Other variables not compared {.table}

| version |   case | observation |
|:--------|-------:|------------:|
| x       |  88989 |           9 |
| x       |  90158 |           8 |
| x       |  99508 |           7 |
| x       | 112263 |           5 |

Observations not shared {.table}

| var.x       | var.y       |    n | NAs |
|:------------|:------------|-----:|----:|
| arm         | Arm         |    0 |   0 |
| sex         | sex         | 1495 |   0 |
| fu.time     | fu_time     |    0 |   0 |
| fu.stat     | fu stat     |    0 |   0 |
| ps          | ps          |    1 |   1 |
| hgb         | hgb         |  266 | 266 |
| bmi         | bmi         |    0 |   0 |
| alk.phos    | alk.phos    |    0 |   0 |
| ast         | ast         |    1 |   0 |
| mdquality.s | mdquality.s |    0 |   0 |
| age.ord     | age.ord     |    0 |   0 |

Differences detected by variable {.table}

| var.x | var.y |   case | values.x | values.y | row.x | row.y |
|:------|:------|-------:|:---------|:---------|------:|------:|
| sex   | sex   |  76170 | Male     | Male     |    26 |    20 |
| sex   | sex   |  76240 | Male     | Male     |    27 |    21 |
| sex   | sex   |  76431 | Female   | Female   |    28 |    22 |
| sex   | sex   |  76712 | Male     | Male     |    29 |    23 |
| sex   | sex   |  76780 | Female   | Female   |    30 |    24 |
| sex   | sex   |  77066 | Female   | Female   |    31 |    25 |
| sex   | sex   |  77316 | Male     | Male     |    32 |    26 |
| sex   | sex   |  77355 | Male     | Male     |    33 |    27 |
| sex   | sex   |  77591 | Male     | Male     |    34 |    28 |
| sex   | sex   |  77851 | Male     | Male     |    35 |    29 |
| ps    | ps    |  86205 | 0        | NA       |     6 |     3 |
| hgb   | hgb   |  88714 | NA       | -9       |   192 |   186 |
| hgb   | hgb   |  88955 | NA       | -9       |   204 |   198 |
| hgb   | hgb   |  89549 | NA       | -9       |   229 |   223 |
| hgb   | hgb   |  89563 | NA       | -9       |   231 |   225 |
| hgb   | hgb   |  89584 | NA       | -9       |   237 |   231 |
| hgb   | hgb   |  89591 | NA       | -9       |   238 |   232 |
| hgb   | hgb   |  89595 | NA       | -9       |   239 |   233 |
| hgb   | hgb   |  89647 | NA       | -9       |   243 |   237 |
| hgb   | hgb   |  89665 | NA       | -9       |   244 |   238 |
| hgb   | hgb   |  89827 | NA       | -9       |   255 |   249 |
| ast   | ast   | 105271 | 100      | 36       |     3 |     2 |

Differences detected (1741 not shown) {.table}

| var.x | var.y | name   |
|:------|:------|:-------|
| arm   | Arm   | label  |
| sex   | sex   | label  |
| sex   | sex   | levels |
| race  | race  | class  |
| race  | race  | label  |
| race  | race  | levels |
| bmi   | bmi   | label  |

Non-identical attributes {.table}

### Factor tolerance

By default, factors are compared to each other based on both the labels
and the underlying numeric levels. Set `tol.factor="levels"` to match
only the numeric levels, or set `tol.factor="labels"` to match only the
labels.

``` r

summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c("._ ", "case"), # dots=underscores=spaces, ignore case
                int.as.num = TRUE,           # compare integers and numerics
                tol.num.val = 10,            # allow absolute differences <= 10
                tol.factor = "labels"        # match only factor labels
))
```

| version | arg        | ncol | nrow |
|:--------|:-----------|-----:|-----:|
| x       | mockstudy  |   14 | 1499 |
| y       | mockstudy2 |   13 | 1495 |

Summary of data.frames {.table}

| statistic                                                   | value |
|:------------------------------------------------------------|------:|
| Number of by-variables                                      |     1 |
| Number of non-by variables in common                        |    12 |
| Number of variables compared                                |    11 |
| Number of variables in x but not y                          |     1 |
| Number of variables in y but not x                          |     0 |
| Number of variables compared with some values unequal       |     3 |
| Number of variables compared with all values equal          |     8 |
| Number of observations in common                            |  1495 |
| Number of observations in x but not y                       |     4 |
| Number of observations in y but not x                       |     0 |
| Number of observations with some compared variables unequal |   268 |
| Number of observations with all compared variables equal    |  1227 |
| Number of values unequal                                    |   268 |

Summary of overall comparison {.table}

| version | variable | position | class   |
|:--------|:---------|---------:|:--------|
| x       | age      |        2 | integer |

Variables not shared {.table}

| var.x | pos.x | class.x   | var.y | pos.y | class.y |
|:------|------:|:----------|:------|------:|:--------|
| race  |     5 | character | race  |     3 | factor  |

Other variables not compared {.table}

| version |   case | observation |
|:--------|-------:|------------:|
| x       |  88989 |           9 |
| x       |  90158 |           8 |
| x       |  99508 |           7 |
| x       | 112263 |           5 |

Observations not shared {.table}

| var.x       | var.y       |   n | NAs |
|:------------|:------------|----:|----:|
| arm         | Arm         |   0 |   0 |
| sex         | sex         |   0 |   0 |
| fu.time     | fu_time     |   0 |   0 |
| fu.stat     | fu stat     |   0 |   0 |
| ps          | ps          |   1 |   1 |
| hgb         | hgb         | 266 | 266 |
| bmi         | bmi         |   0 |   0 |
| alk.phos    | alk.phos    |   0 |   0 |
| ast         | ast         |   1 |   0 |
| mdquality.s | mdquality.s |   0 |   0 |
| age.ord     | age.ord     |   0 |   0 |

Differences detected by variable {.table}

| var.x | var.y |   case | values.x | values.y | row.x | row.y |
|:------|:------|-------:|:---------|:---------|------:|------:|
| ps    | ps    |  86205 | 0        | NA       |     6 |     3 |
| hgb   | hgb   |  88714 | NA       | -9       |   192 |   186 |
| hgb   | hgb   |  88955 | NA       | -9       |   204 |   198 |
| hgb   | hgb   |  89549 | NA       | -9       |   229 |   223 |
| hgb   | hgb   |  89563 | NA       | -9       |   231 |   225 |
| hgb   | hgb   |  89584 | NA       | -9       |   237 |   231 |
| hgb   | hgb   |  89591 | NA       | -9       |   238 |   232 |
| hgb   | hgb   |  89595 | NA       | -9       |   239 |   233 |
| hgb   | hgb   |  89647 | NA       | -9       |   243 |   237 |
| hgb   | hgb   |  89665 | NA       | -9       |   244 |   238 |
| hgb   | hgb   |  89827 | NA       | -9       |   255 |   249 |
| ast   | ast   | 105271 | 100      | 36       |     3 |     2 |

Differences detected (256 not shown) {.table}

| var.x | var.y | name   |
|:------|:------|:-------|
| arm   | Arm   | label  |
| sex   | sex   | label  |
| sex   | sex   | levels |
| race  | race  | class  |
| race  | race  | label  |
| race  | race  | levels |
| bmi   | bmi   | label  |

Non-identical attributes {.table}

Also note the option `factor.as.char=`, which determines whether factors
and characters should be compared despite their class difference. If
`TRUE`, the factors are coerced to characters. Note that
`mockstudy$race` is a character, while `mockstudy2$race` is a factor:

``` r

summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c("._ ", "case"), # dots=underscores=spaces, ignore case
                int.as.num = TRUE,           # compare integers and numerics
                tol.num.val = 10,            # allow absolute differences <= 10
                tol.factor = "labels",       # match only factor labels
                factor.as.char = TRUE        # compare factors and characters
))
```

| version | arg        | ncol | nrow |
|:--------|:-----------|-----:|-----:|
| x       | mockstudy  |   14 | 1499 |
| y       | mockstudy2 |   13 | 1495 |

Summary of data.frames {.table}

| statistic                                                   | value |
|:------------------------------------------------------------|------:|
| Number of by-variables                                      |     1 |
| Number of non-by variables in common                        |    12 |
| Number of variables compared                                |    12 |
| Number of variables in x but not y                          |     1 |
| Number of variables in y but not x                          |     0 |
| Number of variables compared with some values unequal       |     4 |
| Number of variables compared with all values equal          |     8 |
| Number of observations in common                            |  1495 |
| Number of observations in x but not y                       |     4 |
| Number of observations in y but not x                       |     0 |
| Number of observations with some compared variables unequal |  1339 |
| Number of observations with all compared variables equal    |   156 |
| Number of values unequal                                    |  1553 |

Summary of overall comparison {.table}

| version | variable | position | class   |
|:--------|:---------|---------:|:--------|
| x       | age      |        2 | integer |

Variables not shared {.table}

|                                 |
|:--------------------------------|
| No other variables not compared |

Other variables not compared {.table}

| version |   case | observation |
|:--------|-------:|------------:|
| x       |  88989 |           9 |
| x       |  90158 |           8 |
| x       |  99508 |           7 |
| x       | 112263 |           5 |

Observations not shared {.table}

| var.x       | var.y       |    n | NAs |
|:------------|:------------|-----:|----:|
| arm         | Arm         |    0 |   0 |
| sex         | sex         |    0 |   0 |
| race        | race        | 1285 |   0 |
| fu.time     | fu_time     |    0 |   0 |
| fu.stat     | fu stat     |    0 |   0 |
| ps          | ps          |    1 |   1 |
| hgb         | hgb         |  266 | 266 |
| bmi         | bmi         |    0 |   0 |
| alk.phos    | alk.phos    |    0 |   0 |
| ast         | ast         |    1 |   0 |
| mdquality.s | mdquality.s |    0 |   0 |
| age.ord     | age.ord     |    0 |   0 |

Differences detected by variable {.table}

| var.x | var.y |   case | values.x  | values.y  | row.x | row.y |
|:------|:------|-------:|:----------|:----------|------:|------:|
| race  | race  |  76170 | Caucasian | caucasian |    26 |    20 |
| race  | race  |  76240 | Caucasian | caucasian |    27 |    21 |
| race  | race  |  76431 | Caucasian | caucasian |    28 |    22 |
| race  | race  |  76712 | Caucasian | caucasian |    29 |    23 |
| race  | race  |  76780 | Caucasian | caucasian |    30 |    24 |
| race  | race  |  77066 | Caucasian | caucasian |    31 |    25 |
| race  | race  |  77316 | Caucasian | caucasian |    32 |    26 |
| race  | race  |  77591 | Caucasian | caucasian |    34 |    28 |
| race  | race  |  77851 | Caucasian | caucasian |    35 |    29 |
| race  | race  |  77956 | Caucasian | caucasian |    36 |    30 |
| ps    | ps    |  86205 | 0         | NA        |     6 |     3 |
| hgb   | hgb   |  88714 | NA        | -9        |   192 |   186 |
| hgb   | hgb   |  88955 | NA        | -9        |   204 |   198 |
| hgb   | hgb   |  89549 | NA        | -9        |   229 |   223 |
| hgb   | hgb   |  89563 | NA        | -9        |   231 |   225 |
| hgb   | hgb   |  89584 | NA        | -9        |   237 |   231 |
| hgb   | hgb   |  89591 | NA        | -9        |   238 |   232 |
| hgb   | hgb   |  89595 | NA        | -9        |   239 |   233 |
| hgb   | hgb   |  89647 | NA        | -9        |   243 |   237 |
| hgb   | hgb   |  89665 | NA        | -9        |   244 |   238 |
| hgb   | hgb   |  89827 | NA        | -9        |   255 |   249 |
| ast   | ast   | 105271 | 100       | 36        |     3 |     2 |

Differences detected (1531 not shown) {.table}

| var.x | var.y | name   |
|:------|:------|:-------|
| arm   | Arm   | label  |
| sex   | sex   | label  |
| sex   | sex   | levels |
| race  | race  | class  |
| race  | race  | label  |
| race  | race  | levels |
| bmi   | bmi   | label  |

Non-identical attributes {.table}

### Character tolerance

Use the `tol.char=` argument to change how character variables are
compared. By default, they are compared as-is, but they can be compared
after ignoring case or trimming whitespace or both.

``` r

summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c("._ ", "case"), # dots=underscores=spaces, ignore case
                int.as.num = TRUE,           # compare integers and numerics
                tol.num.val = 10,            # allow absolute differences <= 10
                tol.factor = "labels",       # match only factor labels
                factor.as.char = TRUE,       # compare factors and characters
                tol.char = "case"            # ignore case in character vectors
))
```

| version | arg        | ncol | nrow |
|:--------|:-----------|-----:|-----:|
| x       | mockstudy  |   14 | 1499 |
| y       | mockstudy2 |   13 | 1495 |

Summary of data.frames {.table}

| statistic                                                   | value |
|:------------------------------------------------------------|------:|
| Number of by-variables                                      |     1 |
| Number of non-by variables in common                        |    12 |
| Number of variables compared                                |    12 |
| Number of variables in x but not y                          |     1 |
| Number of variables in y but not x                          |     0 |
| Number of variables compared with some values unequal       |     3 |
| Number of variables compared with all values equal          |     9 |
| Number of observations in common                            |  1495 |
| Number of observations in x but not y                       |     4 |
| Number of observations in y but not x                       |     0 |
| Number of observations with some compared variables unequal |   268 |
| Number of observations with all compared variables equal    |  1227 |
| Number of values unequal                                    |   268 |

Summary of overall comparison {.table}

| version | variable | position | class   |
|:--------|:---------|---------:|:--------|
| x       | age      |        2 | integer |

Variables not shared {.table}

|                                 |
|:--------------------------------|
| No other variables not compared |

Other variables not compared {.table}

| version |   case | observation |
|:--------|-------:|------------:|
| x       |  88989 |           9 |
| x       |  90158 |           8 |
| x       |  99508 |           7 |
| x       | 112263 |           5 |

Observations not shared {.table}

| var.x       | var.y       |   n | NAs |
|:------------|:------------|----:|----:|
| arm         | Arm         |   0 |   0 |
| sex         | sex         |   0 |   0 |
| race        | race        |   0 |   0 |
| fu.time     | fu_time     |   0 |   0 |
| fu.stat     | fu stat     |   0 |   0 |
| ps          | ps          |   1 |   1 |
| hgb         | hgb         | 266 | 266 |
| bmi         | bmi         |   0 |   0 |
| alk.phos    | alk.phos    |   0 |   0 |
| ast         | ast         |   1 |   0 |
| mdquality.s | mdquality.s |   0 |   0 |
| age.ord     | age.ord     |   0 |   0 |

Differences detected by variable {.table}

| var.x | var.y |   case | values.x | values.y | row.x | row.y |
|:------|:------|-------:|:---------|:---------|------:|------:|
| ps    | ps    |  86205 | 0        | NA       |     6 |     3 |
| hgb   | hgb   |  88714 | NA       | -9       |   192 |   186 |
| hgb   | hgb   |  88955 | NA       | -9       |   204 |   198 |
| hgb   | hgb   |  89549 | NA       | -9       |   229 |   223 |
| hgb   | hgb   |  89563 | NA       | -9       |   231 |   225 |
| hgb   | hgb   |  89584 | NA       | -9       |   237 |   231 |
| hgb   | hgb   |  89591 | NA       | -9       |   238 |   232 |
| hgb   | hgb   |  89595 | NA       | -9       |   239 |   233 |
| hgb   | hgb   |  89647 | NA       | -9       |   243 |   237 |
| hgb   | hgb   |  89665 | NA       | -9       |   244 |   238 |
| hgb   | hgb   |  89827 | NA       | -9       |   255 |   249 |
| ast   | ast   | 105271 | 100      | 36       |     3 |     2 |

Differences detected (256 not shown) {.table}

| var.x | var.y | name   |
|:------|:------|:-------|
| arm   | Arm   | label  |
| sex   | sex   | label  |
| sex   | sex   | levels |
| race  | race  | class  |
| race  | race  | label  |
| race  | race  | levels |
| bmi   | bmi   | label  |

Non-identical attributes {.table}

### Date tolerance

Use the `tol.date=` argument to change how dates are compared. By
default, they’re expected to be equal to each other.

### Other data type tolerances

Use the `tol.other=` argument to change how other objects are compared.
By default, they’re expected to be
[`identical()`](https://rdrr.io/r/base/identical.html).

### Specifying tolerances for each variable

You can also provide a list of tolerance functions to
[`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md):

``` r

comparedf.control(tol.char = list(
  "none",      # the default
  x1 = "case", # be case-insensitive for the variable "x1"
  x2 = function(x, y) tol.NA(x, y, x != y | y == "NA") # a custom-defined tolerance
))
```

### User-defined tolerance functions

#### Details

The
[`comparedf.control()`](https://mayoverse.github.io/arsenal/reference/comparedf.control.md)
function accepts functions for any of the tolerance arguments in
addition to the short-hand character strings. This allows the user to
create custom tolerance functions to suit his/her needs.

Any custom tolerance function must accept two vectors as arguments and
return a logical vector of the same length. The `TRUE`s in the results
should correspond to elements which are deemed “different”. Note that
the numeric and date tolerance functions should also include a third
argument for tolerance size (even if it’s not used).

CAUTION: the results should not include NAs, since the logical vector is
used to subset the input data.frames. The
[`tol.NA()`](https://mayoverse.github.io/arsenal/reference/comparedf.tolerances.md)
function is useful for considering any NAs in the two vectors (but not
both) as differences, in addition to other criteria.

The
[`tol.NA()`](https://mayoverse.github.io/arsenal/reference/comparedf.tolerances.md)
function is used in all default tolerance functions to help handle NAs.

#### Example 1

Suppose we want to ignore any dates which are later in the second
dataset than the first. We define a custom tolerance function.

``` r

my.tol <- function(x, y, tol)
{
  tol.NA(x, y, x > y)
}

date.df1 <- data.frame(dt = as.Date(c("2017-09-07", "2017-08-08", "2017-07-09", NA)))
date.df2 <- data.frame(dt = as.Date(c("2017-10-01", "2017-08-08", "2017-07-10", "2017-01-01")))
n.diffs(comparedf(date.df1, date.df2)) # default finds any differences
```

    [1] 3

``` r

n.diffs(comparedf(date.df1, date.df2, tol.date = my.tol)) # our function identifies only the NA as different...
```

    [1] 1

``` r

n.diffs(comparedf(date.df2, date.df1, tol.date = my.tol)) # ... until we change the argument order
```

    [1] 3

#### Example 2

(Continuing our mockstudy example)

Suppose we’re okay with NAs getting replaced by -9.

``` r

tol.minus9 <- function(x, y, tol)
{
  idx1 <- is.na(x) & !is.na(y) & y == -9
  idx2 <- tol.num.absolute(x, y, tol) # find other absolute differences
  return(!idx1 & idx2)
}

summary(comparedf(mockstudy, mockstudy2, by = "case",
                tol.vars = c("._ ", "case"), # dots=underscores=spaces, ignore case
                int.as.num = TRUE,           # compare integers and numerics
                tol.num.val = 10,            # allow absolute differences <= 10
                tol.factor = "labels",       # match only factor labels
                factor.as.char = TRUE,       # compare factors and characters
                tol.char = "case",           # ignore case in character vectors
                tol.num = tol.minus9         # ignore NA -> -9 changes
))
```

| version | arg        | ncol | nrow |
|:--------|:-----------|-----:|-----:|
| x       | mockstudy  |   14 | 1499 |
| y       | mockstudy2 |   13 | 1495 |

Summary of data.frames {.table}

| statistic                                                   | value |
|:------------------------------------------------------------|------:|
| Number of by-variables                                      |     1 |
| Number of non-by variables in common                        |    12 |
| Number of variables compared                                |    12 |
| Number of variables in x but not y                          |     1 |
| Number of variables in y but not x                          |     0 |
| Number of variables compared with some values unequal       |     2 |
| Number of variables compared with all values equal          |    10 |
| Number of observations in common                            |  1495 |
| Number of observations in x but not y                       |     4 |
| Number of observations in y but not x                       |     0 |
| Number of observations with some compared variables unequal |     2 |
| Number of observations with all compared variables equal    |  1493 |
| Number of values unequal                                    |     2 |

Summary of overall comparison {.table}

| version | variable | position | class   |
|:--------|:---------|---------:|:--------|
| x       | age      |        2 | integer |

Variables not shared {.table}

|                                 |
|:--------------------------------|
| No other variables not compared |

Other variables not compared {.table}

| version |   case | observation |
|:--------|-------:|------------:|
| x       |  88989 |           9 |
| x       |  90158 |           8 |
| x       |  99508 |           7 |
| x       | 112263 |           5 |

Observations not shared {.table}

| var.x       | var.y       |   n | NAs |
|:------------|:------------|----:|----:|
| arm         | Arm         |   0 |   0 |
| sex         | sex         |   0 |   0 |
| race        | race        |   0 |   0 |
| fu.time     | fu_time     |   0 |   0 |
| fu.stat     | fu stat     |   0 |   0 |
| ps          | ps          |   1 |   1 |
| hgb         | hgb         |   0 |   0 |
| bmi         | bmi         |   0 |   0 |
| alk.phos    | alk.phos    |   0 |   0 |
| ast         | ast         |   1 |   0 |
| mdquality.s | mdquality.s |   0 |   0 |
| age.ord     | age.ord     |   0 |   0 |

Differences detected by variable {.table}

| var.x | var.y |   case | values.x | values.y | row.x | row.y |
|:------|:------|-------:|:---------|:---------|------:|------:|
| ps    | ps    |  86205 | 0        | NA       |     6 |     3 |
| ast   | ast   | 105271 | 100      | 36       |     3 |     2 |

Differences detected {.table}

| var.x | var.y | name   |
|:------|:------|:-------|
| arm   | Arm   | label  |
| sex   | sex   | label  |
| sex   | sex   | levels |
| race  | race  | class  |
| race  | race  | label  |
| race  | race  | levels |
| bmi   | bmi   | label  |

Non-identical attributes {.table}

## Extract Differences

Differences can be easily extracted using the
[`diffs()`](https://mayoverse.github.io/arsenal/reference/diffs.md)
function. If you only want to determine how many differences were found,
use the
[`n.diffs()`](https://mayoverse.github.io/arsenal/reference/diffs.md)
function.

``` r

cmp <- comparedf(mockstudy, mockstudy2, by = "case", tol.vars = c("._ ", "case"), int.as.num = TRUE)
n.diffs(cmp)
```

    [1] 1765

``` r

head(diffs(cmp))
```

      var.x var.y  case values.x values.y row.x row.y
    1   sex   sex 76170     Male     Male    26    20
    2   sex   sex 76240     Male     Male    27    21
    3   sex   sex 76431   Female   Female    28    22
    4   sex   sex 76712     Male     Male    29    23
    5   sex   sex 76780   Female   Female    30    24
    6   sex   sex 77066   Female   Female    31    25

Differences can also be summarized by variable.

``` r

diffs(cmp, by.var = TRUE)
```

             var.x       var.y    n NAs
    1          arm         Arm    0   0
    2          sex         sex 1495   0
    3      fu.time     fu_time    0   0
    4      fu.stat     fu stat    0   0
    5           ps          ps    1   1
    6          hgb         hgb  266 266
    7          bmi         bmi    0   0
    8     alk.phos    alk.phos    0   0
    9          ast         ast    3   0
    10 mdquality.s mdquality.s    0   0
    11     age.ord     age.ord    0   0

To report differences from only a few variables, one can pass a list of
variable names to
[`diffs()`](https://mayoverse.github.io/arsenal/reference/diffs.md).

``` r

diffs(cmp, vars = c("ps", "ast"), by.var = TRUE)
```

      var.x var.y n NAs
    5    ps    ps 1   1
    9   ast   ast 3   0

``` r

diffs(cmp, vars = c("ps", "ast"))
```

         var.x var.y   case values.x values.y row.x row.y
    1496    ps    ps  86205        0       NA     6     3
    1763   ast   ast  86205       27       36     6     3
    1764   ast   ast 105271      100       36     3     2
    1765   ast   ast 110754       35       36     1     1

## Appendix

### Stucture of the Object

(This section is just as much for my use as for yours!)

``` r

obj <- comparedf(mockstudy, mockstudy2, by = "case")
```

There are two main objects in the `"comparedf"` object, each with its
own print method.

The `frame.summary` contains:

- the substituted-deparsed arguments

- information about the number of columns and rows in each dataset

- the by-variables for each dataset (which may not be the same)

- the attributes for each dataset (which get counted in the print
  method)

- a data.frame of by-variables and row numbers of observations not
  shared between datasets

- the number of shared observations

``` r

print(obj$frame.summary)
```

      version        arg ncol nrow   by        attrs       unique n.shared
    1       x  mockstudy   14 1499 case 3 attributes 4 unique obs     1495
    2       y mockstudy2   13 1495 case 3 attributes 0 unique obs     1495

The `vars.summary` contains:

- variable name, column number, and class vector (with possibly more
  than one element) for each x and y. These are all `NA` if there isn’t
  a match in both datasets.

- values, a list-column of the text string `"by-variable"` for the
  by-variables, `NULL` for columns that aren’t compared, or a data.frame
  containing:

  - The by-variables for differences found

  - The values which are different for x and y

  - The row numbers for differences found

- attrs, a list-column of `NULL` if there are no attributes, or a
  data.frame containing:

  - The name of the attributes

  - The attributes for x and y, set to `NA` if non-existant

  - The actual attributes (if `show.attr=TRUE`).

``` r

print(obj$vars.summary)
```

             var.x pos.x         class.x       var.y pos.y         class.y           values        attrs
    1         case     1         integer        case     1         integer      by-variable 0 attributes
    2          sex     4          factor         sex     2          factor 1495 differences 2 attributes
    3         race     5       character        race     3          factor     Not compared 3 attributes
    4           ps     8         integer          ps     4         integer    1 differences 0 attributes
    5          hgb     9         numeric         hgb     5         numeric  266 differences 0 attributes
    6          bmi    10         numeric         bmi     6         numeric    0 differences 1 attributes
    7     alk.phos    11         integer    alk.phos     7         integer    0 differences 0 attributes
    8          ast    12         integer         ast     8         numeric     Not compared 0 attributes
    9  mdquality.s    13         integer mdquality.s     9         integer    0 differences 0 attributes
    10     age.ord    14 ordered, factor     age.ord    10 ordered, factor    0 differences 0 attributes
    11         age     2         integer        <NA>    NA              NA     Not compared 0 attributes
    12         arm     3       character        <NA>    NA              NA     Not compared 0 attributes
    13     fu.time     6         integer        <NA>    NA              NA     Not compared 0 attributes
    14     fu.stat     7         integer        <NA>    NA              NA     Not compared 0 attributes
    15        <NA>    NA              NA     fu_time    11         integer     Not compared 0 attributes
    16        <NA>    NA              NA     fu stat    12         integer     Not compared 0 attributes
    17        <NA>    NA              NA         Arm    13       character     Not compared 0 attributes
