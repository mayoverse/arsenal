# write2word, write2html, write2pdf

Functions to output tables to a single Word, HTML, or PDF document.

## Usage

``` r
write2word(object, file, ...)

write2pdf(object, file, ...)

write2html(object, file, ...)
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

## Value

`object` is returned invisibly, and `file` is written.

## Details

To generate the appropriate file type, the `write2*` functions use one
of
[`rmarkdown::word_document`](https://pkgs.rstudio.com/rmarkdown/reference/word_document.html),
[`rmarkdown::html_document`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html),
and
[`rmarkdown::pdf_document`](https://pkgs.rstudio.com/rmarkdown/reference/pdf_document.html)
to get the job done. `"..."` arguments are passed to these functions,
too.

## See also

[`write2`](https://mayoverse.github.io/arsenal/reference/write2.md)

## Author

Ethan Heinzen, adapted from code from Krista Goergen

## Examples

``` r
if (FALSE) { # \dontrun{
data(mockstudy)
# tableby example
tab1 <- tableby(arm ~ sex + age, data=mockstudy)
write2html(tab1, "~/trash.html")

# freqlist example
tab.ex <- table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA = "ifany")
noby <- freqlist(tab.ex, na.options = "include")
write2pdf(noby, "~/trash2.pdf")

# A more complicated example
write2word(tab1, "~/trash.doc",
  keep.md = TRUE,
  reference_docx = mystyles.docx, # passed to rmarkdown::word_document
  quiet = TRUE, # passed to rmarkdown::render
  title = "My cool new title") # passed to summary.tableby
} # }
```
