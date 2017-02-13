#' Helper functions for \code{write2}
#' 
#' Helper functions for \code{\link{write2}}.
#' 
#' @param x An R object to coerce to class \code{"verbatim"} or such an object to print.
#' @param ... Other arguments passed to \code{print}.
#' @details
#' The \code{"verbatim"} class is to tell \code{\link{write2}} to print the object inside
#'   a section surrounded by three back ticks. The results will look like it would in the terminal (monospaced).
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
