# Summarize a `modelsum` object.

Format the information in `object` as a table using Pandoc coding or
plain text, and cat it to stdout.

## Usage

``` r
# S3 method for class 'modelsum'
summary(
  object,
  ...,
  labelTranslations = NULL,
  text = FALSE,
  title = NULL,
  term.name = "",
  adjustment.names = FALSE
)

# S3 method for class 'summary.modelsum'
as.data.frame(
  x,
  ...,
  text = x$text,
  term.name = x$term.name,
  adjustment.names = x$adjustment.names,
  width = NULL,
  min.split = NULL,
  list.ok = FALSE
)
```

## Arguments

- object:

  A
  [`modelsum`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
  object.

- ...:

  For `summary.modelsum`, other arguments passed to
  [`as.data.frame.modelsum`](https://mayoverse.github.io/arsenal/reference/as.data.frame.modelsum.md).
  For `as.data.frame.summary.modelsum`, "width" and "min.split" are
  passed to
  [`smart.split`](https://mayoverse.github.io/arsenal/reference/internal.functions.md).
  For `print`ing the summary object, these are passed to both
  `as.data.frame.summary.modelsum` and
  [`kable`](https://rdrr.io/pkg/knitr/man/kable.html).

- labelTranslations:

  A named list (or vector) where the name is the label in the output to
  be replaced in the pretty rendering by the character string value for
  the named element of the list, e.g.,
  `list(age = "Age(Years)", meansd = "Mean(SD)")`.

- text:

  An argument denoting how to print the summary to the screen. Default
  is `FALSE` (show markdown output). `TRUE` and `NULL` output a
  text-only version, with the latter avoiding all formatting. `"html"`
  uses the HTML tag `<strong>` instead of the markdown formatting, and
  `"latex"` uses the LaTeX command `\textbf`.

- title:

  Title/caption for the table, defaulting to `NULL` (no title). Passed
  to [`kable`](https://rdrr.io/pkg/knitr/man/kable.html). Can be length
  \> 1 if the more than one table is being printed.

- term.name:

  A character vector denoting the column name for the "terms" column. It
  should be the same length as the number of tables or less (it will get
  recycled if needed). The special value `TRUE` will use the
  y-variable's label for each table.

- adjustment.names:

  Logical, denoting whether the names of the adjustment models should be
  printed.

- x:

  An object of class `"summary.modelsum"`.

- width, min.split:

  Passed to
  [`smart.split`](https://mayoverse.github.io/arsenal/reference/internal.functions.md)
  for formatting of the "term" column.

- list.ok:

  If the object has multiple by-variables, is it okay to return a list
  of data.frames instead of a single data.frame? If `FALSE` but there
  are multiple by-variables, a warning is issued.

## Value

An object of class `"summary.modelsum"`

## See also

[`modelsum`](https://mayoverse.github.io/arsenal/reference/modelsum.md),
[`as.data.frame.modelsum`](https://mayoverse.github.io/arsenal/reference/as.data.frame.modelsum.md)

## Author

Ethan Heinzen, based on code originally by Greg Dougherty
