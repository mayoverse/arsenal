#' as.data.frame.freqlist
#'
#' Convert \code{\link{freqlist}} object to a data.frame.
#'
#' @param x An object of class \code{"freqlist"}.
#' @param ... optional arguments included for S3 consistency
#' @return A data.frame corresponding to the \code{freqlist} object.
#' @export
as.data.frame.freqlist <- function(x, ...)
{
  return(x$freqlist)
}

#' Helper functions for freqlist
#'
#' A set of helper functions for \code{\link{freqlist}}.
#'
#' @param x,object A \code{freqlist} object.
#' @param value A list or vector of new labels.
#' @param ... Other arguments (not in use at this time, but included for S3 consistency)
#' @name freqlist.internal
NULL
#> NULL

#' @rdname freqlist.internal
#' @export
'labels<-.freqlist' <- function(x, value) {

  if(is.null(value))
  {
    x["labels"] <- list(NULL)
    return(x)
  }

  if(is.list(value)) value <- unlist(value)
  
  if((!is.character(value) || length(value) != ncol(x$freqlist) - 4))
  {
    stop("New labels must be 'NULL' or character vector of length ", ncol(x$freqlist) - 4, ".")
  }

  x$labels <- value

  ## return freqlist x with updated labels
  return(x)
}

#' @rdname freqlist.internal
#' @export
labels.freqlist <- function(object, ...) {
  return(object$labels)
}
