#' write2
#'
#' Functions to output tables to a single document. (Also the S3 backbone behind the \code{write2*} functions.)
#'
#' @param object An object.
#' @param file A single character string denoting the filename for the output document.
#' @param ... Additional arguments to be passed to \code{FUN}, \code{rmarkdown::render}, etc.
#'   One popular option is to use \code{quiet = TRUE} to suppress the command line output.
#' @param FUN The summary-like or print-like function to use to generate the markdown content. Can be passed as a function or a
#'   character string. It's expected that \code{FUN(object, ...)} looks "good" when put directly in a \code{.md} file.
#' @param append. Logical, denoting whether (if a temporary \code{.md} file of the same name already exists)
#'   to append on. Used mostly for \code{write2.list}.
#' @param render. Logical, denoting whether to render the temporary \code{.md} file. Used mostly for \code{write2.list}.
#' @param keep.md Logical, denoting whether to keep the intermediate \code{.md} file. Used mostly for \code{write2.list}.
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
#' @details \code{write2} is an S3 method. The default prints the object (using \code{\link{print}})
#'    inside a section surrounded by three back ticks. See \code{\link{verbatim}} for details.
#'
#'    There are methods implemented for \code{\link{tableby}}, \code{\link{modelsum}}, and \code{\link{freqlist}}, all of which use the
#'    \code{summary} function. There are also methods compatible with \code{\link[knitr]{kable}}, \code{\link[xtable]{xtable}},
#'    and \code{\link[pander]{pander_return}}. Another option is to coerce an object using \code{\link{verbatim}()} to print out the
#'    results monospaced (as if they were in the terminal). To output multiple tables into a document, simply make a list of them
#'    and call the same function as before.
#'
#'    For more information, see \code{vignette("write2")}.
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
write2 <- function(object, file, ..., output_format)
{
  UseMethod("write2")
}

write2_using_summary <- function(object, file, ..., output_format = NULL)
{
  write2.default(object = object, file = file, FUN = summary, ..., output_format = output_format)
}

write2_using_print <- function(object, file, ..., output_format = NULL)
{
  write2.default(object = object, file = file, FUN = print, ..., output_format = output_format)
}

############################ write2 for arsenal objects ############################

#' @rdname write2
#' @export
write2.tableby <- write2_using_summary

#' @rdname write2
#' @export
write2.modelsum <- write2_using_summary

#' @rdname write2
#' @export
write2.freqlist <- write2_using_summary

#' @rdname write2
#' @export
write2.compare.data.frame <- write2_using_summary

#' @rdname write2
#' @export
write2.summary.compare.data.frame <- write2_using_print

#' @rdname write2
#' @export
write2.verbatim <- write2_using_print

#' @rdname write2
#' @export
write2.yaml <- write2_using_print

############################ write2 for external objects ############################

#' @rdname write2
#' @export
write2.knitr_kable <- write2_using_print

#' @rdname write2
#' @export
write2.xtable <- write2_using_print

#' @rdname write2
#' @export
## this intended for use with pander_return()
write2.character <- function(object, file, ..., output_format = NULL)
{
  write2.default(object = object, file = file, FUN = cat, ..., sep = "\n", output_format = output_format)
}

############################ write2 for lists of objects ############################

#' @rdname write2
#' @export
write2.list <- function(object, file, ..., append. = FALSE, render. = TRUE, keep.md = !render., output_format = NULL)
{
  if(!is.character(file) || length(file) > 1) stop("'file' argument must be a single character string.")
  if(!is.logical(append.) || length(append.) > 1) stop("'append.' argument must be a single logical value.")

  filename <- paste0(file, ".md")
  if(!append. || !file.exists(filename)) file.create(filename)

  # find any YAML specifications
  idx <- vapply(object, is.yaml, NA)
  if(sum(idx) > 0)
  {
    yamls <- Reduce(c, object[idx])
    object <- object[!idx]
    write2(yamls, file = file, ..., keep.md = TRUE, append = TRUE, render. = FALSE, output_format = output_format)
  }

  # separate the tables with a few blank lines, leading with the blank lines
  object2 <- c(object, as.list(rep("\n\n", times = length(object))))[order(c(seq_along(object), seq_along(object) - 0.5))]

  lapply(object2, write2, file = file, ..., keep.md = TRUE, append. = TRUE, render. = FALSE, output_format = output_format)

  write2("\n", file = file, ..., render. = render., append. = TRUE, keep.md = keep.md, output_format = output_format)

  invisible(object)
}

