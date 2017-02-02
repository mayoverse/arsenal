#' write2word, write2html, write2pdf
#' 
#' Functions to output tables to a single Word, HTML, or PDF document.
#' 
#' @inheritParams write2
#' @return \code{object} is returned invisibly, and \code{file} is written.
#' @details
#'  To generate the appropriate file type, the \code{write2*} functions use one of \code{rmarkdown::word_document}, \code{rmarkdown::html_document},
#'    and \code{rmarkdown::pdf_document} to get the job done. \code{"..."} arguments are passed to these functions, too.
#' @seealso \code{\link{write2}}
#' @examples
#' \dontrun{
#' data(mockstudy)
#' # tableby example
#' tab1 <- tableby(arm ~ sex + age, data=mockstudy)
#' write2html(tab1, "~/trash.html")
#'
#' # freqlist example
#' tab.ex <- table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA = "ifany")
#' noby <- freqlist(tab.ex, na.options = "include")
#' write2pdf(noby, "~/trash2.pdf")
#' 
#' # A more complicated example
#' write2word(tab1, "~/trash.doc",
#'   keep.md = TRUE,
#'   reference_docx = mystyles.docx, # passed to rmarkdown::word_document
#'   quiet = TRUE, # passed to rmarkdown::render
#'   title = "My cool new title") # passed to summary.tableby
#' }
#' @author Ethan Heinzen, adapted from code from Krista Goergen
#' @name write2specific
NULL
#> NULL

#' @rdname write2specific
#' @export
write2word <- function(object, file, ...)
{
  write2(object, file, ..., output_format = "word")
}

#' @rdname write2specific
#' @export
write2pdf <- function(object, file, ...)
{
  write2(object, file, ..., output_format = "pdf")
}

#' @rdname write2specific
#' @export
write2html <- function(object, file, ...)
{
  write2(object, file, ..., output_format = "html")
}
