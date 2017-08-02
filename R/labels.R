
#' Labels
#'
#' Assign and extract the \code{'label'} attribute on an R object.
#'
#' @param x,object An R object.
#' @param value A vector or list containing labels to assign. Labels are assigned based on
#'   names, if available; otherwise, they're assigned in order. Can pass \code{NULL}
#'   to remove all labels.
#' @param ... Other arguments (not in use at this time).
#' @return The labels of \code{object}, or \code{object} with new labels.
#' @details
#'   The \code{\link{data.frame}} methods put labels on and extract labels from
#'   the \emph{columns} of \code{object}.
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
  attr(object, "label")
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
