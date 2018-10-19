
locate <- function(string, pattern) stringr::str_locate_all(string, pattern)[[1L]][, 1L]

smartsplit <- function(string, width, min.split)
{
  if(width < min.split) stop("Desired width < min.split?")
  if(nchar(string) <= width) return(string)

  pos <- locate(string, "[ \t\n_.;:,-]")
  splt <- if(length(pos) == 0 || !any(idx <- pos <= width & pos >= min.split)) width else max(pos[idx])

  c(stringr::str_sub(string, 1L, splt), smartsplit(stringr::str_sub(string, splt+1L), width = width, min.split = min.split))
}

#' Split a string into pieces intelligently
#'
#' @param string A character vector
#' @param width Either \code{Inf} or \code{NULL} to specify no splitting,
#'   or a positive integer giving the largest allowed string length.
#' @param min.split Either \code{-Inf} or \code{NULL} to specify no
#'   lower bound on the string length, or a positive integer giving the minimum string length.
#' @return A list of the same length as \code{string}, with each element being
#'   the "intelligently" split string.
#' @name internal.functions
NULL
#> NULL

#' @rdname internal.functions
#' @export
smart.split <- function(string, width = Inf, min.split = -Inf)
{
  if(is.null(width)) width <- Inf
  if(is.null(min.split)) min.split <- -Inf
  lapply(string, smartsplit, width = width, min.split = min.split)
}


insert_elt <- function(col, times, elt = "")
{
  f <- if(is.null(elt)) rep else function(x, i) c(x, rep(elt, times = i - 1L))
  unlist(Map(f, col, times), use.names = FALSE)
}


#' Adjust P-values for Multiple Comparisons
#'
#' @param p An object.
#' @inheritParams stats::p.adjust
#' @param suffix A suffix to add to the footnotes indicating that the tests were adjusted.
#' @param ... Other arguments.
#' @seealso \code{\link[stats]{p.adjust}}, \code{\link{modpval.tableby}}, \code{\link{tests.tableby}}
#' @name padjust
NULL
#> NULL

#' @rdname padjust
#' @export
padjust <- function(p, method, n) UseMethod("padjust")

#' @rdname padjust
#' @export
padjust.default <- function(p, method, n, ...)
{
  Call <- match.call()
  indx <- match(c("p", "method", "n"), names(Call), nomatch = 0)
  temp.call <- Call[c(1, indx)]
  temp.call[[1L]] <- quote(stats::p.adjust)

  eval(temp.call, parent.frame())
}
