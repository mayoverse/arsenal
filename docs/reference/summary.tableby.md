# The summary method for a `tableby` object

The summary method for a
[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md)
object, which is a pretty rendering of a
[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md)
object into a publication-quality results table in R Markdown, and can
render well in text-only.

## Usage

``` r
# S3 method for class 'tableby'
summary(
  object,
  ...,
  labelTranslations = NULL,
  text = FALSE,
  title = NULL,
  pfootnote = FALSE,
  term.name = ""
)

# S3 method for class 'summary.tableby'
as.data.frame(
  x,
  ...,
  text = x$text,
  pfootnote = x$pfootnote,
  term.name = x$term.name,
  width = NULL,
  min.split = NULL,
  list.ok = FALSE
)
```

## Arguments

- object:

  An object of class `"tableby"`, made by the
  [`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  function.

- ...:

  For `summary.tableby`, other arguments passed to
  [`as.data.frame.tableby`](https://mayoverse.github.io/arsenal/reference/as.data.frame.tableby.md).
  For `print`ing the summary object, these are passed to both
  `as.data.frame.summary.tableby` and
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

- pfootnote:

  Logical, denoting whether to put footnotes describing the tests used
  to generate the p-values. Alternatively, "html" to surround the
  outputted footnotes with `<li>`.

- term.name:

  A character vector denoting the column name for the "terms" column. It
  should be the same length as the number of tables or less (it will get
  recycled if needed). The special value `TRUE` will use the
  y-variable's label for each table.

- x:

  An object of class `"summary.tableby"`.

- width, min.split:

  Passed to
  [`smart.split`](https://mayoverse.github.io/arsenal/reference/internal.functions.md)
  for formatting of the "term" column.

- list.ok:

  If the object has multiple by-variables, is it okay to return a list
  of data.frames instead of a single data.frame? If `FALSE` but there
  are multiple by-variables, a warning is issued.

## Value

An object of class `summary.tableby`

## See also

[`tableby.control`](https://mayoverse.github.io/arsenal/reference/tableby.control.md),
[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md)

## Author

Ethan Heinzen, based on code by Gregory Dougherty, Jason Sinnwell, Beth
Atkinson, adapted from SAS Macros written by Paul Novotny and Ryan
Lennon

## Examples

``` r

set.seed(100)
## make 3+ categories for response
nsubj <- 90
mdat <- data.frame(Response=sample(c(1,2,3),nsubj, replace=TRUE),
                   Sex=sample(c("Male", "Female"), nsubj,replace=TRUE),
                   Age=round(rnorm(nsubj,mean=40, sd=5)),
                   HtIn=round(rnorm(nsubj,mean=65,sd=5)))

## allow default summaries on RHS variables
out <- tableby(Response ~ Sex + Age + HtIn, data=mdat)
summary(out, text=TRUE)
#> 
#> 
#> |             |    1 (N=25)     |    2 (N=31)     |    3 (N=34)     |  Total (N=90)   | p value|
#> |:------------|:---------------:|:---------------:|:---------------:|:---------------:|-------:|
#> |Sex          |                 |                 |                 |                 |   0.232|
#> |-  Female    |   17 (68.0%)    |   14 (45.2%)    |   19 (55.9%)    |   50 (55.6%)    |        |
#> |-  Male      |    8 (32.0%)    |   17 (54.8%)    |   15 (44.1%)    |   40 (44.4%)    |        |
#> |Age          |                 |                 |                 |                 |   0.547|
#> |-  Mean (SD) | 40.200 (4.021)  | 40.161 (3.796)  | 39.265 (3.671)  | 39.833 (3.796)  |        |
#> |-  Range     | 29.000 - 48.000 | 33.000 - 51.000 | 30.000 - 48.000 | 29.000 - 51.000 |        |
#> |HtIn         |                 |                 |                 |                 |   0.093|
#> |-  Mean (SD) | 63.360 (5.322)  | 66.516 (4.878)  | 65.000 (5.684)  | 65.067 (5.402)  |        |
#> |-  Range     | 52.000 - 78.000 | 57.000 - 78.000 | 50.000 - 79.000 | 50.000 - 79.000 |        |
#> 
labels(out)
#>   Response        Sex        Age       HtIn 
#> "Response"      "Sex"      "Age"     "HtIn" 
labels(out) <- c(Age="Age (years)", HtIn="Height (inches)")
summary(out, stats.labels=c(meansd="Mean-SD", q1q3 = "Q1-Q3"), text=TRUE)
#> 
#> 
#> |                |    1 (N=25)     |    2 (N=31)     |    3 (N=34)     |  Total (N=90)   | p value|
#> |:---------------|:---------------:|:---------------:|:---------------:|:---------------:|-------:|
#> |Sex             |                 |                 |                 |                 |   0.232|
#> |-  Female       |   17 (68.0%)    |   14 (45.2%)    |   19 (55.9%)    |   50 (55.6%)    |        |
#> |-  Male         |    8 (32.0%)    |   17 (54.8%)    |   15 (44.1%)    |   40 (44.4%)    |        |
#> |Age (years)     |                 |                 |                 |                 |   0.547|
#> |-  Mean-SD      | 40.200 (4.021)  | 40.161 (3.796)  | 39.265 (3.671)  | 39.833 (3.796)  |        |
#> |-  Range        | 29.000 - 48.000 | 33.000 - 51.000 | 30.000 - 48.000 | 29.000 - 51.000 |        |
#> |Height (inches) |                 |                 |                 |                 |   0.093|
#> |-  Mean-SD      | 63.360 (5.322)  | 66.516 (4.878)  | 65.000 (5.684)  | 65.067 (5.402)  |        |
#> |-  Range        | 52.000 - 78.000 | 57.000 - 78.000 | 50.000 - 79.000 | 50.000 - 79.000 |        |
#> 
```
