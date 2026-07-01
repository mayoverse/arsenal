# The summary method for a `comparedf` object

Print a more detailed output of the
[`comparedf`](https://mayoverse.github.io/arsenal/reference/comparedf.md)
object.

## Usage

``` r
# S3 method for class 'comparedf'
summary(object, ..., show.attrs = FALSE)

# S3 method for class 'summary.comparedf'
print(x, ..., format = "pandoc")
```

## Arguments

- object:

  An object of class `"comparedf"`, as made by the
  [`comparedf`](https://mayoverse.github.io/arsenal/reference/comparedf.md)
  S3 method.

- ...:

  Other arguments passed to
  [`comparedf.control`](https://mayoverse.github.io/arsenal/reference/comparedf.control.md).
  In `print`, these are passed to
  [`kable`](https://rdrr.io/pkg/knitr/man/kable.html).

- show.attrs:

  Logical, denoting whether to show the actual attributes which are
  different. For (e.g.) factors with lots of levels, this can make the
  tables quite wide, so this feature is `FALSE` by default.

- x:

  An object returned by the `summary.comparedf` function.

- format:

  Passed to [`kable`](https://rdrr.io/pkg/knitr/man/kable.html): the
  format for the table. The default here is "pandoc". To use the default
  in `kable`, pass `NULL`.

## Value

An object of class `"summary.comparedf"` is returned.

## See also

[`comparedf`](https://mayoverse.github.io/arsenal/reference/comparedf.md),
[`comparedf.control`](https://mayoverse.github.io/arsenal/reference/comparedf.control.md)
