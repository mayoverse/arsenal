# Helper functions for freqlist

A set of helper functions for
[`freqlist`](https://mayoverse.github.io/arsenal/reference/freqlist.md).

## Usage

``` r
is.freqlist(x)

is.summary.freqlist(x)

# S3 method for class 'summary.freqlist'
head(x, n = 6L, ...)

# S3 method for class 'summary.freqlist'
tail(x, n = 6L, ...)

# S3 method for class 'freqlist'
sort(x, decreasing = FALSE, ...)
```

## Arguments

- x:

  A `freqlist` object.

- n:

  A single integer. See [`head`](https://rdrr.io/r/utils/head.html) or
  [`tail`](https://rdrr.io/r/utils/head.html) for more details

- ...:

  Other arguments.

- decreasing:

  Should the sort be increasing or decreasing?

## Details

Note that [`sort()`](https://rdrr.io/r/base/sort.html) has to
recalculate cumulative statistics. Note also that the reordering of rows
will also affect which labels are duplicates; you may also want to
consider using `dupLabels=TRUE` in
[`freq.control()`](https://mayoverse.github.io/arsenal/reference/freq.control.md).

## See also

[`merge.freqlist`](https://mayoverse.github.io/arsenal/reference/arsenal_table.md),
[`arsenal_table`](https://mayoverse.github.io/arsenal/reference/arsenal_table.md),
[`sort`](https://rdrr.io/r/base/sort.html),
[`freqlist`](https://mayoverse.github.io/arsenal/reference/freqlist.md),
[`summary.freqlist`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md),
[`freq.control`](https://mayoverse.github.io/arsenal/reference/freq.control.md),
