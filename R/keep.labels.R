
#' Keep Labels
#'
#' Keep the \code{'label'} attribute on an R object when subsetting.
#'
#' @param x An R object
#' @param ... Other arguments (not in use at this time).
#' @return A copy of \code{x} with a "keep labels" class appended on.
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
  class(x) <- c("keep_labels_df", class(x)[class(x) != "keep_labels_df"])
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
