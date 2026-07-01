# as.data.frame.tableby

Coerce a
[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md)
object to a `data.frame`.

## Usage

``` r
# S3 method for class 'tableby'
as.data.frame(x, ..., labelTranslations = NULL, list.ok = FALSE)
```

## Arguments

- x:

  A
  [`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  object.

- ...:

  Arguments to pass to
  [`tableby.control`](https://mayoverse.github.io/arsenal/reference/tableby.control.md).

- labelTranslations:

  A named list (or vector) where the name is the label in the output to
  be replaced in the pretty rendering by the character string value for
  the named element of the list, e.g.,
  `list(age = "Age(Years)", meansd = "Mean(SD)")`.

- list.ok:

  If the object has multiple by-variables, is it okay to return a list
  of data.frames instead of a single data.frame? If `FALSE` but there
  are multiple by-variables, a warning is issued.

## Value

A `data.frame`.

## See also

[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md),
[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md)

## Author

Ethan Heinzen, based on code originally by Greg Dougherty
