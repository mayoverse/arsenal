# Some functions to handle NAs

`allNA` tests if all elements are NA, and `includeNA` sets the `NA`s in
a character vector or factor to an explicit label.

## Usage

``` r
allNA(x)

includeNA(x, label, ...)

# S3 method for class 'factor'
includeNA(x, label = "(Missing)", first = FALSE, ...)

# Default S3 method
includeNA(x, label = "(Missing)", ...)
```

## Arguments

- x:

  An object

- label:

  A character string denoting the label to set `NA`s to.

- ...:

  Other arguments (not in use at this time).

- first:

  Logical; should the new label be the first level?

## See also

[`is.na`](https://rdrr.io/r/base/NA.html),
[`anyNA`](https://rdrr.io/r/base/NA.html)

## Author

Ethan Heinzen
