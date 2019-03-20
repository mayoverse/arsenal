
#' Keep Labels
#'
#' Keep the \code{'label'} attribute on an R object when subsetting. \code{loosen.labels} allows the \code{'label'}
#'   attribute to be lost again.
#'
#' @param x An R object
#' @param i,value See \code{\link{[<-}}.
#' @param ... Other arguments (not in use at this time).
#' @return A copy of \code{x} with a "keep_labels" class appended on or removed. Note that for the \code{data.frame} method,
#'   only classes on the columns are changed; the \code{data.frame} won't have an extra class appended. This is different from previous
#'   versions of \code{arsenal}.
#' @author Ethan Heinzen
#' @seealso \code{\link{labels}}
#' @name keep.labels
NULL
#> NULL

#' @rdname keep.labels
#' @export
keep.labels <- function(x, ...)
{
  UseMethod("keep.labels")
}

#' @rdname keep.labels
#' @export
keep.labels.data.frame <- function(x, ...)
{
  x[] <- lapply(x, keep.labels)
  x
}

#' @rdname keep.labels
#' @export
keep.labels.default <- function(x, ...)
{
  class(x) <- c("keep_labels", class(x)[class(x) != "keep_labels"])
  x
}

#' @rdname keep.labels
#' @export
`[.keep_labels` <- function(x, ...)
{
  y <- NextMethod()
  keep.labels(set_attr(y, "label", attr(x, "label")))
}

#' @rdname keep.labels
#' @export
`[<-.keep_labels` <- function(x, i, value)
{
  x <- loosen.labels(x)
  out <- NextMethod()
  keep.labels(out)
}

#' @rdname keep.labels
#' @export
loosen.labels <- function(x, ...)
{
  UseMethod("loosen.labels")
}

#' @rdname keep.labels
#' @export
loosen.labels.data.frame <- function(x, ...)
{
  x[] <- lapply(x, loosen.labels)
  x
}

#' @rdname keep.labels
#' @export
loosen.labels.default <- function(x, ...)
{
  class(x) <- class(x)[class(x) != "keep_labels"]
  x
}
