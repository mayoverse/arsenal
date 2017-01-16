#' write2word, write2html, write2pdf
#' 
#' Functions to generate a word, html, or pdf document containing a single table.
#' 
#' @param object An object whose \code{summary} output looks "good" when using \code{results='asis'} in markdown.
#' @param file A single character string denoting the filename for the output document.
#' @param ... Additional arguments to be passed to \code{summary}, \code{rmarkdown::render}, etc.
#'   One popular option is to use \code{quiet = TRUE} to suppress the command line output.
#' @param keep.md Logical, denoting whether to keep the intermediate \code{.md} file.
#' @param output_format One of the following:
#'  \enumerate{
#'    \item{An output format object, e.g. \code{rmarkdown::\link[rmarkdown]{html_document}(...)}.}
#'    \item{A character string denoting such a format function, e.g. \code{"html_document"}. In this case, the \code{"..."} are NOT passed.}
#'    \item{The format function itself, e.g. \code{rmarkdown::html_document}. In this case, the \code{"..."} arguments are passed.}
#'    \item{One of \code{"html"}, \code{"pdf"}, and \code{"word"}, shortcuts implemented here. In this case, the \code{"..."} arguments are passed.}
#'    \item{\code{NULL}, in which the output is html by default.}
#'  }
#'  See \code{rmarkdown::\link[rmarkdown]{render}} for details.
#' @return \code{object} is returned invisibly, and \code{file} is written.
#' @details \code{write2} is an S3 method, and the default
#'    (used for \code{\link{tableby}}, \code{\link{modelsum}}, \code{\link{freqlist}}, etc.) assumes
#'    that there is a \code{summary} method implemented which looks 'good' in markdown.
#'   
#'  To generate the appropriate file type, the \code{write2*} functions use one of \code{rmarkdown::word_document}, \code{rmarkdown::html_document},
#'    and \code{rmarkdown::pdf_document} to get the job done. \code{"..."} arguments are passed to these functions, too.
#'    
#' @seealso \code{\link[rmarkdown]{render}}, \code{\link[rmarkdown]{word_document}}, \code{\link[rmarkdown]{html_document}}, \code{\link[rmarkdown]{pdf_document}},
#'   \code{\link[rmarkdown]{rtf_document}}, \code{\link[rmarkdown]{md_document}}, \code{\link[rmarkdown]{odt_document}}
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
#' 
#' # Using write2()
#' write2(tab1, "~/trash.rtf",
#'   toc = TRUE, # passed to rmarkdown::rtf_document
#'   quiet = TRUE, # passed to rmarkdown::render, though in this case it's not practical
#'   title = "My cool new title", # passed to summary.tableby
#'   output_format = rmarkdown::rtf_document)
#' }
#' @author Ethan Heinzen, adapted from code from Krista Goergen
#' @name write2
NULL
#> NULL


#' @rdname write2
#' @export
write2 <- function(object, file, ..., keep.md, output_format)
{
  UseMethod("write2")
}

#' @rdname write2
#' @export
write2.default <- function(object, file, ..., keep.md = FALSE, output_format = NULL)
{
  if(missing(file)) stop("'file' argument can't be missing!")
  if(!is.character(file) || length(file) > 1) stop("'file' argument must be a single character string.")
  if(!is.logical(keep.md) || length(keep.md) > 1) stop("'keep.md' argument must be a single character string.")
  
  
  if(is.character(output_format) && length(output_format) > 1)
  {
    warning("At this point, write2() only supports one output type.")
    output_format <- output_format[1]
  }
  
  
  output_format <- if(is.null(output_format) || identical(output_format, "html"))
  {
    rmarkdown::html_document
  } else if(identical(output_format, "pdf"))
  {
    rmarkdown::pdf_document
  } else if(identical(output_format, "word"))
  {
    rmarkdown::word_document
  } else output_format

  dots <- list(...)
  utils::capture.output(summary(object, ...), file = paste0(file, ".md"))
  
  render.args <- dots[names(dots) %in% names(formals(rmarkdown::render))]
  render.args$input <- paste0(file, ".md")
  render.args$output_file <- file
  
  # if output_format is a function, evaluate it with the ... arguments
  # otherwise, just return the character string or list which rmarkdown::render() will handle
  render.args$output_format <- if(is.function(output_format))
  {
    do.call(output_format, dots[names(dots) %in% names(formals(output_format))])
  } else output_format
  
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
