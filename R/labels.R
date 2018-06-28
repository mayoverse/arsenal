
#' Labels
#'
#' Assign and extract the \code{'label'} attribute on an R object. \code{set_labels} is
#'   the same as \code{labels(x) <- value} but returns \code{x} for use in a pipe chain.
#'   \code{set_attr} is the same as \code{attr(x, which) <- value} but returns \code{x}
#'   for use in a pipe chain.
#'
#' @param x,object An R object.
#' @param value A vector or list containing labels to assign. Labels are assigned based on
#'   names, if available; otherwise, they're assigned in order. Can pass \code{NULL}
#'   to remove all labels.
#' @param which See \code{\link{attr<-}}
#' @param ... Other arguments (not in use at this time).
#' @return The labels of \code{object}, or \code{object} with new labels.
#' @details
#'   The \code{\link{data.frame}} methods put labels on and extract labels from
#'   the \emph{columns} of \code{object}.
#' @seealso \code{\link{keep.labels}}
#' @author Ethan Heinzen
#' @name labels
NULL
#> NULL

#' @rdname labels
#' @export
labels.data.frame <- function(object, ...)
{
  lapply(object, attr, which = "label", exact = TRUE)
}

#' @rdname labels
#' @export
labels.keep_labels <- function(object, ...)
{
  attr(object, "label", exact = TRUE)
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
  set_attr(x, "label", value)
}

#' @rdname labels
#' @export
`labels<-.default` <- function(x, value) # we can't overwrite labels.default(), but nothing is stopping us from doing this
{
  set_attr(x, "label", value)
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

#' @rdname labels
#' @export
set_labels <- function(x, value)
{
  labels(x) <- value
  x
}

#' @rdname labels
#' @export
set_attr <- function(x, which, value)
{
  attr(x, which) <- value
  x
}

