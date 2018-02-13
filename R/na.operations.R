
#' Some functions to handle NAs
#'
#' \code{allNA} tests if all elements are NA, and \code{includeNA} sets the
#'   \code{NA}s in a character vector or factor to an explicit label.
#'
#' @param x An object
#' @param label A character string denoting the label to set \code{NA}s to.
#'
#' @seealso \code{\link{is.na}}, \code{\link{anyNA}}
#' @author Ethan Heinzen
#' @name NA.operations
NULL
#> NULL

#' @rdname NA.operations
#' @export
allNA <- function(x) all(is.na(x))

#' @rdname NA.operations
#' @export
includeNA <- function(x, label, ...)
{
  UseMethod("includeNA")
}

#' @rdname NA.operations
#' @export
includeNA.factor <- function(x, label = "(Missing)", first = FALSE, ...)
{
  lvl <- levels(x)
  if(label %in% lvl)
  {
    warning('"', label, '" already appears in levels(x).')
  } else if(first)
  {
    levels(x) <- c(label, lvl)
    x[] <- levels(x)[as.integer(x) + 1L]

  } else levels(x) <- c(lvl, label) # don't use factor() here, in case you lose attributes
  x[is.na(x)] <- label
  x
}

#' @rdname NA.operations
#' @export
includeNA.character <- function(x, label = "(Missing)", ...)
{
  if(label %in% x) warning('"', label, '" already appears in x.')
  x[is.na(x)] <- label
  x
}

