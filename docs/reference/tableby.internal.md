# Helper functions for tableby

A set of helper functions for
[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md).

## Usage

``` r
is.tableby(x)

is.summary.tableby(x)

modpval.tableby(x, pdata, use.pname = FALSE)

tests(x)

# S3 method for class 'tableby'
tests(x)

na.tableby(lhs = TRUE)

# S3 method for class 'tableby'
xtfrm(x)

# S3 method for class 'tableby'
sort(x, ...)

# S3 method for class 'tableby'
Ops(e1, e2)

# S3 method for class 'tableby'
head(x, n = 6L, ...)

# S3 method for class 'tableby'
tail(x, n = 6L, ...)
```

## Arguments

- x:

  A `tableby` object.

- pdata:

  A named data.frame where the first column is the by-variable names,
  the (optional) second is the strata value, the next is the x variable
  names, the next is p-values (or some test stat), and the (optional)
  next column is the method name.

- use.pname:

  Logical, denoting whether the column name in `pdata` corresponding to
  the p-values should be used in the output of the object.

- lhs:

  Logical, denoting whether to remove `NA`s from the first column of the
  data.frame (the "left-hand side")

- ...:

  Other arguments.

- e1, e2:

  [`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  objects, or numbers to compare them to.

- n:

  A single integer. See [`head`](https://rdrr.io/r/utils/head.html) or
  [`tail`](https://rdrr.io/r/utils/head.html) for more details

## Value

`na.tableby` returns a subsetted version of `object` (with attributes).
`Ops.tableby` returns a logical vector. `xtfrm.tableby` returns the
p-values (which are ordered by
[`order`](https://rdrr.io/r/base/order.html) to
[`sort`](https://rdrr.io/r/base/sort.html)).

## Details

Logical comparisons are implemented for `Ops.tableby`.

## See also

[`arsenal_table`](https://mayoverse.github.io/arsenal/reference/arsenal_table.md),
[`sort`](https://rdrr.io/r/base/sort.html),
[`head`](https://rdrr.io/r/utils/head.html),
[`tail`](https://rdrr.io/r/utils/head.html),
[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md),
[`summary.tableby`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md),
[`tableby.control`](https://mayoverse.github.io/arsenal/reference/tableby.control.md)
