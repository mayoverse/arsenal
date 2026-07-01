# `arsenal` tables with common structure

`arsenal` tables with common structure

## Usage

``` r
has_strata(x)

# S3 method for class 'arsenal_table'
x[i, j, ...]

# S3 method for class 'tableby'
x[i, j, ...]

# S3 method for class 'arsenal_table'
labels(object, ...)

# S3 method for class 'arsenal_table'
labels(x) <- value

# S3 method for class 'arsenal_table'
print(x, ...)

# S3 method for class 'arsenal_table'
merge(x, y, all = FALSE, all.x = all, all.y = all, ...)

# S3 method for class 'freqlist'
merge(x, y, all = TRUE, ...)

# S3 method for class 'summary.arsenal_table'
print(
  x,
  ...,
  format = if (!is.null(x$text) && x$text %in% c("html", "latex")) x$text else
    "markdown",
  escape = x$text %nin% c("html", "latex"),
  width = NULL,
  min.split = NULL
)
```

## Arguments

- x, y, object:

  An object of class `"arsenal_table"`

- i, j:

  A vector to index `x` with: either names of variables, a numeric
  vector, or a logical vector of appropriate length. `i` indexes the
  x-variables, and `j` indexes the by-variables.

- ...:

  Other arguments (only used in `print.summary.arsenal_table`)

- value:

  A list of new labels.

- all, all.x, all.y:

  Logicals, denoting which terms to keep if not all are in common.

- format:

  Passed to [`kable`](https://rdrr.io/pkg/knitr/man/kable.html): the
  format for the table. The default here is "markdown". To use the
  default in `kable`, pass `NULL`. If `x$text` specifies LaTeX or HTML
  formatting, that format is used in the table.

- escape:

  Passed to [`kable`](https://rdrr.io/pkg/knitr/man/kable.html): should
  special characters be escaped when printed?

- width, min.split:

  Passed to
  [`smart.split`](https://mayoverse.github.io/arsenal/reference/internal.functions.md)
  for formatting of the "term" column.

## See also

[`merge`](https://rdrr.io/r/base/merge.html),
[`labels`](https://mayoverse.github.io/arsenal/reference/labels.md)
