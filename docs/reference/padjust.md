# Adjust P-values for Multiple Comparisons

Adjust P-values for Multiple Comparisons

## Usage

``` r
padjust(p, method, n, ...)

# Default S3 method
padjust(p, method, n, ...)

# S3 method for class 'tableby'
padjust(p, method, n, suffix = " (adjusted for multiple comparisons)", ...)

# S3 method for class 'summary.tableby'
padjust(p, method, n, suffix = " (adjusted for multiple comparisons)", ...)
```

## Arguments

- p:

  An object.

- method:

  correction method, a
  [`character`](https://rdrr.io/r/base/character.html) string. Can be
  abbreviated.

- n:

  number of comparisons, must be at least `length(p)`; only set this (to
  non-default) when you know what you are doing!

- ...:

  Other arguments.

- suffix:

  A suffix to add to the footnotes indicating that the tests were
  adjusted.

## See also

[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html),
[`modpval.tableby`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md),
[`tests.tableby`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md)
