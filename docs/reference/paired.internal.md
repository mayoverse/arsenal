# Helper functions for paired

A set of helper functions for
[`paired`](https://mayoverse.github.io/arsenal/reference/paired.md).

## Usage

``` r
na.paired(missings = c("in.both", "fill", "asis"))
```

## Arguments

- missings:

  A character string denoting which action to take. See "Details",
  below.

## Value

`na.paired` returns a function used to subset data.frames in
[`paired`](https://mayoverse.github.io/arsenal/reference/paired.md).

## Details

All methods subset out any NA time points or IDs. `"in.both"` (the
default) subsets the data.frame to individuals who appear at both time
points. `"fill"` adds explicit missings for the people missing second
time points. `"asis"` does nothing to add or remove missings.

## See also

[tableby.internal](https://mayoverse.github.io/arsenal/reference/tableby.internal.md)
