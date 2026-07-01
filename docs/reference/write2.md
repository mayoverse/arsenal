# write2

Functions to output tables to a single document. (Also the S3 backbone
behind the `write2*` functions.)

## Usage

``` r
write2(object, file, ..., output_format)

# S3 method for class 'arsenal_table'
write2(object, file, ..., output_format = NULL)

# S3 method for class 'summary.arsenal_table'
write2(object, file, ..., output_format = NULL)

# S3 method for class 'comparedf'
write2(object, file, ..., output_format = NULL)

# S3 method for class 'summary.comparedf'
write2(object, file, ..., output_format = NULL)

# S3 method for class 'verbatim'
write2(object, file, ..., output_format = NULL)

# S3 method for class 'yaml'
write2(object, file, ..., output_format = NULL)

# S3 method for class 'code.chunk'
write2(object, file, ..., output_format = NULL)

# S3 method for class 'knitr_kable'
write2(object, file, ..., output_format = NULL)

# S3 method for class 'xtable'
write2(object, file, ..., output_format = NULL)

# S3 method for class 'character'
write2(object, file, ..., output_format = NULL)

# S3 method for class 'list'
write2(
  object,
  file,
  ...,
  append. = FALSE,
  render. = TRUE,
  keep.rmd = !render.,
  output_format = NULL
)

# Default S3 method
write2(
  object,
  file,
  FUN = NULL,
  ...,
  append. = FALSE,
  render. = TRUE,
  keep.rmd = !render.,
  output_format = NULL
)
```

## Arguments

- object:

  An object.

- file:

  A single character string denoting the filename for the output
  document.

- ...:

  Additional arguments to be passed to `FUN`,
  [`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html),
  etc. One popular option is to use `quiet = TRUE` to suppress the
  command line output.

- output_format:

  One of the following:

  1.  An output format object, e.g.
      `rmarkdown::`[`html_document`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html)`(...)`.

  2.  A character string denoting such a format function, e.g.
      `"html_document"`. In this case, the `"..."` are NOT passed.

  3.  The format function itself, e.g.
      [`rmarkdown::html_document`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html).
      In this case, the `"..."` arguments are passed.

  4.  One of `"html"`, `"pdf"`, and `"word"`, shortcuts implemented
      here. In this case, the `"..."` arguments are passed.

  5.  `NULL`, in which the output is HTML by default.

  See
  `rmarkdown::`[`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
  for details.

- append.:

  Logical, denoting whether (if a temporary `.Rmd` file of the same name
  already exists) to append on. Used mostly for `write2.list`.

- render.:

  Logical, denoting whether to render the temporary `.Rmd` file. Used
  mostly for `write2.list`.

- keep.rmd:

  Logical, denoting whether to keep the intermediate `.Rmd` file. Used
  mostly for `write2.list`.

- FUN:

  The summary-like or print-like function to use to generate the
  markdown content. Can be passed as a function or a character string.
  It's expected that `FUN(object, ...)` looks "good" when put directly
  in a `.Rmd` file.

## Value

`object` is returned invisibly, and `file` is written.

## Details

`write2` is an S3 method. The default prints the object (using
[`print`](https://rdrr.io/r/base/print.html)) inside a section
surrounded by three back ticks. See
[`verbatim`](https://mayoverse.github.io/arsenal/reference/write2.internal.md)
for details.

There are methods implemented for
[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md),
[`modelsum`](https://mayoverse.github.io/arsenal/reference/modelsum.md),
and
[`freqlist`](https://mayoverse.github.io/arsenal/reference/freqlist.md),
all of which use the `summary` function. There are also methods
compatible with [`kable`](https://rdrr.io/pkg/knitr/man/kable.html),
[`xtable`](https://rdrr.io/pkg/xtable/man/xtable.html), and
[`pander_return`](https://rdrr.io/pkg/pander/man/pander_return.html).
Another option is to coerce an object using
[`verbatim()`](https://mayoverse.github.io/arsenal/reference/write2.internal.md)
to print out the results monospaced (as if they were in the terminal).
To output multiple tables into a document, simply make a list of them
and call the same function as before. Finally, to output code chunks to
be evaluated, use
[`code.chunk`](https://mayoverse.github.io/arsenal/reference/write2.internal.md).

For more information, see
[`vignette("write2")`](https://mayoverse.github.io/arsenal/articles/write2.md).

## See also

[`write2word`](https://mayoverse.github.io/arsenal/reference/write2specific.md),
[`write2pdf`](https://mayoverse.github.io/arsenal/reference/write2specific.md),
[`write2html`](https://mayoverse.github.io/arsenal/reference/write2specific.md),
[`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html),
[`word_document`](https://pkgs.rstudio.com/rmarkdown/reference/word_document.html),
[`html_document`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html),
[`pdf_document`](https://pkgs.rstudio.com/rmarkdown/reference/pdf_document.html),
[`rtf_document`](https://pkgs.rstudio.com/rmarkdown/reference/rtf_document.html),
[`md_document`](https://pkgs.rstudio.com/rmarkdown/reference/md_document.html),
[`odt_document`](https://pkgs.rstudio.com/rmarkdown/reference/odt_document.html)

## Author

Ethan Heinzen, adapted from code from Krista Goergen

## Examples

``` r
if (FALSE) { # \dontrun{
data(mockstudy)
# tableby example
tab1 <- tableby(arm ~ sex + age, data=mockstudy)
write2(tab1, tempfile(fileext = ".rtf"),
  toc = TRUE, # passed to rmarkdown::rtf_document, though in this case it's not practical
  quiet = TRUE, # passed to rmarkdown::render
  title = "My cool new title", # passed to summary.tableby
  output_format = rmarkdown::rtf_document)

write2html(list(
  "# Header 1", # a header
  code.chunk(a <- 1, b <- 2, a + b), # a code chunk
  verbatim("hi there") # verbatim output
),
  tempfile(fileext = ".html"),
  quite = TRUE)
} # }
```
