
#' Labels
#'
#' Assign the \code{'label'} attribute on an R object, and keep it there when subsetting.
#'
#'
#'
#' @author Ethan Heinzen, based on an idea from Brendan Broderick
#' @name labels
NULL
#> NULL

#' @rdname labels
#' @export
labels.data.frame <- function(x, ...)
{
  lapply(x, attr, which = "label", exact = TRUE)
}

#' @rdname labels
#' @export
labels.keep_labels <- function(x, ...)
{
  attr(x, "label")
}

#' @rdname labels
#' @export
'labels<-' <- function(x, value)
{
  UseMethod("labels<-")
}

#' @rdname labels
#' @export
`labels<-.keep_labels` <- function(x, value)
{
  attr(x, "label") <- value
  x
}

#' @rdname labels
#' @export
`labels<-.data.frame` <- function(x, value)
{
  if(is.null(value))
  {
    value <- rep(list(NULL), times = ncol(x))
  }

  if(is.null(names(x)) || is.null(names(value)))
  {
    if(length(x) != length(value) && length(x) > 0) stop("'x' and 'value' have unequal lengths, and don't have names")
    idx <- seq_along(x) # just map one-to-one
  } else
  {
    value <- value[names(value) %in% names(x)]
    idx <- match(names(value), names(x))
  }

  for(i in seq_along(idx))
  {
    attr(x[[idx[i]]], "label") <- value[[i]]
  }
  x
}
