# Internal `tableby` functions

A collection of functions that may help users create custom functions
that are formatted correctly.

## Usage

``` r
tbfmt(x, digits = NULL, digits.count = NULL, digits.pct = NULL, ...)

as.tbstat(
  x,
  oldClass = NULL,
  fmt = "{y}",
  which.count = 0L,
  which.pct = 0L,
  ...
)

as.tbstat_multirow(x)
```

## Arguments

- x:

  Usually a vector.

- digits, digits.pct, digits.count:

  Digits specifications

- ...:

  arguments to pass to `as.tbstat`.

- oldClass:

  class(es) to add to the resulting object.

- fmt:

  A [`glue`](https://glue.tidyverse.org/reference/glue.html) string,
  where the object is exposed as the variable `x`, and a
  default-formatted version (using `tbfmt`) exposed as the variable `y`.
  `digits`, `digits.count`, and `digits.pct` are also exposed.

- which.count:

  Which statistics are counts? The default is 0, indicating that none
  are.

- which.pct:

  Which statistics are percents? The default is 0, indicating that none
  are.

## Details

The vignette has an example on how to use these.

`as.tbstat` defines a tableby statistic with its appropriate formatting.

`tbfmt` applies some default formatting.

`as.tbstat_multirow` marks an object (usually a list) for multiple-row
printing.
