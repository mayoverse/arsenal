#' write2word, write2html, write2pdf
#' 
#' Functions to generate a word, html, or pdf document containing a single table.
#' 
#' @param object An object whose \code{summary} output looks "good" when using \code{results='asis'} in markdown.
#' @param file A single character string denoting the filename for the output document.
#' @param ... Additional arguments to be passed to \code{summary}, \code{rmarkdown::render}, etc.
#'   One popular option is to use \code{quiet = TRUE} to suppress the command line output.
#' @param keep.md Logical, denoting whether to keep the intermediate \code{.md} file.
#' @return \code{object} is returned invisibly, and \code{file} is written.
#' @details This is (kind of) an S3 method (the real S3 method is \code{write2}),and the default (used for tableby, modelsum, freqlist, etc.) assumes
#'   that there is a \code{summary} method implemented.
#'   
#'   To generate the appropriate file type, the default uses one of \code{rmarkdown::word_document}, \code{rmarkdown::html_document},
#'    and \code{rmarkdown::pdf_document} to get the job done. "..." arguments are passed to these functions, too.
#' @seealso \code{\link[rmarkdown]{render}}, \code{\link[rmarkdown]{word_document}}, \code{\link[rmarkdown]{html_document}}, \code{\link[rmarkdown]{pdf_document}}
#' @examples
#' \dontrun{
#' data(mockstudy)
#' # tableby example
#' tab1 <- tableby(arm ~ sex + age, data=mockstudy)
#' write2html(tab1, "~/ibm/trash.html")
#'
#' # freqlist example
#' tab.ex <- table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA = "ifany")
#' noby <- freqlist(tab.ex, na.options = "include")
#' write2pdf(noby, "~/ibm/trash2.pdf")
#' 
#' # A more complicated example
#' write2word(tab1, "~/ibm/trash.doc", keep.md = TRUE,
#'   reference_docx = mystyles.docx, # passed to rmarkdown::word_document
#'   quiet = TRUE, # passed to rmarkdown::render
#'   title = "My cool new title" # passed to summary.tableby
#' }
#' @author Ethan Heinzen, adapted from code from Krista Goergen
#' @name write2
NULL
#> NULL

write2 <- function(object, file, ..., keep.md, output_format)
{
  UseMethod("write2")
}

write2.default <- function(object, file, ..., keep.md = FALSE, output_format = c("html", "pdf", "word"))
{
  if(!is.character(file) || length(file) > 1) stop("'file' argument must be a single character string.")
  if(!is.logical(keep.md) || length(keep.md) > 1) stop("'keep.md' argument must be a single character string.")
  output_format <- match.arg(output_format)
  
  output_format <- if(output_format == "html") rmarkdown::html_document else if(output_format == "pdf") rmarkdown::pdf_document else rmarkdown::word_document
  dots <- list(...)
  capture.output(summary(object, ...), file = paste0(file, ".md"))
  
  output.args <- dots[names(dots) %in% names(formals(output_format))]
  
  render.args <- dots[names(dots) %in% names(formals(rmarkdown::render))]
  render.args$input <- paste0(file, ".md")
  render.args$output_format <- do.call(output_format, output.args)
  render.args$output_file <- file
  do.call(rmarkdown::render, render.args)
  
  if(!keep.md) system(paste0("rm -f ", file, ".md"))
  invisible(object)
}


#' @rdname write2
#' @export
write2word <- function(object, file, ..., keep.md = FALSE)
{
  write2(object, file, ..., keep.md = keep.md, output_format = "word")
}

#' @rdname write2
#' @export
write2pdf <- function(object, file, ..., keep.md = FALSE)
{
  write2(object, file, ..., keep.md = keep.md, output_format = "pdf")
}

#' @rdname write2
#' @export
write2html <- function(object, file, ..., keep.md = FALSE)
{
  write2(object, file, ..., keep.md = keep.md, output_format = "html")
}
