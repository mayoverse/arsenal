# Helper functions for modelsum

A set of helper functions for
[`modelsum`](https://mayoverse.github.io/arsenal/reference/modelsum.md).

## Usage

``` r
is.modelsum(x)

is.summary.modelsum(x)

na.modelsum(object, ...)
```

## Arguments

- x:

  A `modelsum` object.

- object:

  A `data.frame` resulting from evaluating a `modelsum` formula.

- ...:

  Other arguments, or a vector of indices for extracting.

## Value

`na.modelsum` returns a subsetted version of `object` (with attributes).

## See also

[`arsenal_table`](https://mayoverse.github.io/arsenal/reference/arsenal_table.md)
