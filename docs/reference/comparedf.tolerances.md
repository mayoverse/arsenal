# `comparedf` tolerances

Internal functions defining tolerances for the
[`comparedf.control`](https://mayoverse.github.io/arsenal/reference/comparedf.control.md)
function. To create your own tolerance definitions, see the vignette.

## Usage

``` r
tol.NA(x, y, idx)

tol.num.absolute(x, y, tol)

tol.num.percent(x, y, tol)

tol.num.pct(x, y, tol)

tol.factor.none(x, y)

tol.factor.levels(x, y)

tol.factor.labels(x, y)

tol.char.both(x, y)

tol.char.case(x, y)

tol.char.trim(x, y)

tol.char.none(x, y)

tol.date.absolute(x, y, tol)

tol.logical.none(x, y)

tol.other.none(x, y)
```

## Arguments

- x, y:

  vectors of the appropriate lengths and types.

- idx:

  A logical vector of appropriate length.

- tol:

  A numeric tolerance

## Value

A logical vector of length equal to that of `x` and `y`, where `TRUE`
denotes a difference between `x` and `y`, and `FALSE` denotes no
difference between `x` and `y`.

## Details

`tol.NA` takes as differences between two vectors any elements which are
NA in one but not the other, or which are non-NA in both and `TRUE` in
`idx`. It is useful for handling NAs in custom tolerance functions.

## See also

[`comparedf.control`](https://mayoverse.github.io/arsenal/reference/comparedf.control.md),
[`comparedf`](https://mayoverse.github.io/arsenal/reference/comparedf.md)

## Author

Ethan Heinzen
