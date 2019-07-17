#' Helper functions for \code{write2}
#'
#' Helper functions for \code{\link{write2}}.
#'
#' @param x An R object to coerce to class \code{"verbatim"} or such an object to print.
#' @param ... For \code{print.verbatim}, other arguments passed to \code{print}. For \code{code.chunk},
#'   either expressions or single character strings to paste into the code chunk. Not used for \code{print.code.chunk}.
#' @param chunk.opts A single character string giving the code chunk options. Make sure to specify the engine!
#' @details
#' The \code{"verbatim"} class is to tell \code{\link{write2}} to print the object inside
#'   a section surrounded by three back ticks. The results will look like it would in the terminal (monospaced).
#'
#' \code{code.chunk()} is to write explicit code chunks in the \code{.Rmd} file; it captures the call and writes it to the
#'   file, to execute upon knitting.
#' @name write2.internal
NULL
#> NULL

#' @rdname write2.internal
#' @export
print.verbatim <- function(x, ...)
{
  cat("```\n")

  # This line demands some explanation:
  #   its purpose is to make sure that the "verbatim" class doesn't confuse
  #   the downstream print methods. For example, character vectors would
  #   also print the "verbatim" class when run through the default, which
  #   isn't what I wanted. I want "verbatim" to be as invisible as possible.
  #   Note that this won't change what NextMethod() calls.
  class(x) <- class(x)[class(x) != "verbatim"]

  NextMethod("print")
  cat("\n```\n\n")
}

#' @rdname write2.internal
#' @export
verbatim <- function(x)
{
  class(x) <- c("verbatim", class(x)[class(x) != "verbatim"])
  x
}

#' @rdname write2.internal
#' @export
code.chunk <- function(..., chunk.opts = "r")
{
  if(!is.character(chunk.opts) || length(chunk.opts) != 1) stop("'chunk.opts' should be a single character string")
  Call <- match.call()
  Call[[1]] <- NULL
  Call$chunk.opts <- NULL
  if(length(Call) == 0) Call[[1]] <- ""
  set_attr(set_attr(Call, "chunk.opts", chunk.opts), "class", c("code.chunk", class(Call)))
}

#' @rdname write2.internal
#' @export
print.code.chunk <- function(x, ...)
{
  cat("```{", attr(x, "chunk.opts"), "}\n", sep = "")
  lapply(x, function(elt) if(is.character(elt)) cat(elt, "\n", sep = "") else print(elt))
  cat("```\n\n")
}

