
#' Keep Labels
#'
#' Assign the \code{'label'} attribute on an R object, and keep it there when subsetting.
#'
#'
#'
#' @author Ethan Heinzen, based on an idea from Brendan Broderick
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
  for(i in seq_along(x))
  {
    x[[i]] <- keep.labels(x[[i]])
  }

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
  attr(y, "label") <- attr(x, "label")
  keep.labels(y)
}
