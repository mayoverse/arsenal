#' write2
#' 
#' Functions to generate a single document containing a single table. (Also the S3 backbone behind the \code{write2*} functions.)
#' 
#' @param object An object.
#' @param file A single character string denoting the filename for the output document.
#' @param ... Additional arguments to be passed to \code{FUN}, \code{rmarkdown::render}, etc.
#'   One popular option is to use \code{quiet = TRUE} to suppress the command line output.
#' @param FUN The summary-like or print-like function to use to generate the markdown content. Can be passed as a function or a
#'   character string. It's expected that \code{FUN(object, ...)} looks "good" when using \code{results='asis'} in markdown.
#' @param keep.md Logical, denoting whether to keep the intermediate \code{.md} file.
#' @param output_format One of the following:
#'  \enumerate{
#'    \item{An output format object, e.g. \code{rmarkdown::\link[rmarkdown]{html_document}(...)}.}
#'    \item{A character string denoting such a format function, e.g. \code{"html_document"}. In this case, the \code{"..."} are NOT passed.}
#'    \item{The format function itself, e.g. \code{rmarkdown::html_document}. In this case, the \code{"..."} arguments are passed.}
#'    \item{One of \code{"html"}, \code{"pdf"}, and \code{"word"}, shortcuts implemented here. In this case, the \code{"..."} arguments are passed.}
#'    \item{\code{NULL}, in which the output is HTML by default.}
#'  }
#'  See \code{rmarkdown::\link[rmarkdown]{render}} for details.
#' @return \code{object} is returned invisibly, and \code{file} is written.
#' @details \code{write2} is an S3 method, and the default
#'    assumes that there is a \code{FUN} method implemented which looks 'good' in Rmarkdown.
#'    
#'    There are methods implemented for \code{\link{tableby}}, \code{\link{modelsum}}, and \code{\link{freqlist}}, all of which use the
#'    \code{summary} function. There are also methods compatible with \code{\link[knitr]{kable}}, \code{\link[xtable]{xtable}},
#'    and \code{\link[pander]{pander_return}}.
#' @seealso \code{\link{write2word}}, \code{\link{write2pdf}}, \code{\link{write2html}},
#'   \code{\link[rmarkdown]{render}}, \code{\link[rmarkdown]{word_document}}, \code{\link[rmarkdown]{html_document}}, \code{\link[rmarkdown]{pdf_document}},
#'   \code{\link[rmarkdown]{rtf_document}}, \code{\link[rmarkdown]{md_document}}, \code{\link[rmarkdown]{odt_document}}
#' @examples
#' \dontrun{
#' data(mockstudy)
#' # tableby example
#' tab1 <- tableby(arm ~ sex + age, data=mockstudy)
#' write2(tab1, "~/trash.rtf",
#'   toc = TRUE, # passed to rmarkdown::rtf_document, though in this case it's not practical
#'   quiet = TRUE, # passed to rmarkdown::render
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

############################ write2 for arsenal objects ############################

#' @rdname write2
#' @export
write2.tableby <- function(object, file, ..., keep.md = FALSE, output_format = NULL)
{
  write2.default(object = object, file = file, FUN = summary, ..., keep.md = keep.md, output_format = output_format)
}

#' @rdname write2
#' @export
write2.modelsum <- function(object, file, ..., keep.md = FALSE, output_format = NULL)
{
  write2.default(object = object, file = file, FUN = summary, ..., keep.md = keep.md, output_format = output_format)
}

#' @rdname write2
#' @export
write2.freqlist <- function(object, file, ..., keep.md = FALSE, output_format = NULL)
{
  write2.default(object = object, file = file, FUN = summary, ..., keep.md = keep.md, output_format = output_format)
}

############################ write2 for external objects ############################

#' @rdname write2
#' @export
write2.knitr_kable <- function(object, file, ..., keep.md = FALSE, output_format = NULL)
{
  write2.default(object = object, file = file, FUN = print, ..., keep.md = keep.md, output_format = output_format)
}

#' @rdname write2
#' @export
write2.xtable <- function(object, file, ..., keep.md = FALSE, output_format = NULL)
{
  write2.default(object = object, file = file, FUN = print, ..., keep.md = keep.md, output_format = output_format)
}

## this intended for use with pander_return()
#' @rdname write2
#' @export
write2.character <- function(object, file, ..., keep.md = FALSE, output_format = NULL)
{
  write2.default(object = object, file = file, FUN = cat, ..., sep = "\n", keep.md = keep.md, output_format = output_format)
}


#' @rdname write2
#' @export
write2.default <- function(object, file, FUN, ..., keep.md = FALSE, output_format = NULL)
{
  if(!is.character(file) || length(file) > 1) stop("'file' argument must be a single character string.")
  if(!is.logical(keep.md) || length(keep.md) > 1) stop("'keep.md' argument must be a single character string.")
  FUN <- match.fun(FUN)
  
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
  if(names(formals(FUN))[1] == "...") # this is when the FUN is, e.g., cat(). Any named arguments would still get cat'd, which we don't want
  {
    ARGS <- c(list(object), dots[names(dots) %in% names(formals(FUN))])
    utils::capture.output(do.call(FUN, ARGS), file = paste0(file, ".md"))
  } else
  {
    utils::capture.output(FUN(object, ...), file = paste0(file, ".md"))
  }
  
  render.args <- dots[names(dots) %in% names(formals(rmarkdown::render))]
  render.args$input <- paste0(file, ".md")
  render.args$output_file <- file
  
  # if output_format is a function, evaluate it with the ... arguments
  # otherwise, just return the character string or list which rmarkdown::render() will handle
  render.args$output_format <- if(is.function(output_format))
  {
    if("..." %in% names(formals(output_format)))
    {
      do.call(output_format, dots)
    } else do.call(output_format, dots[names(dots) %in% names(formals(output_format))])
  } else output_format
  
  do.call(rmarkdown::render, render.args)
  
  # This short-circuits if they want to keep the .md file. Otherwise, file.remove() returns a logical about successful file removal
  if(!keep.md && !file.remove(paste0(file, ".md"))) warning("Something went wrong removing the temporary .md file.")
  
  invisible(object)
}


#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################


#' write2word, write2html, write2pdf
#' 
#' Functions to generate a word, html, or pdf document containing a single table.
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
write2word <- function(object, file, ..., keep.md = FALSE)
{
  write2(object, file, ..., keep.md = keep.md, output_format = "word")
}

#' @rdname write2specific
#' @export
write2pdf <- function(object, file, ..., keep.md = FALSE)
{
  write2(object, file, ..., keep.md = keep.md, output_format = "pdf")
}

#' @rdname write2specific
#' @export
write2html <- function(object, file, ..., keep.md = FALSE)
{
  write2(object, file, ..., keep.md = keep.md, output_format = "html")
}
