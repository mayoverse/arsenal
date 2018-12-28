
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
#' @inheritParams base::replace
#' @return For \code{smart.split}, a list of the same length as \code{string}, with each element being
#'   the "intelligently" split string.
#'
#'   For \code{replace2}, a vector with the proper values replaced.
#' @seealso \code{\link[base]{replace}}
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

#' @rdname internal.functions
#' @export
replace2 <- function(x, list, values)
{
  x[[list]] <- values
  x
}

as_list_formula <- function(formula)
{
  if(is.list(formula)) return(formula)
  if(length(formula) == 2 || is.name(formula[[2]]) || !identical(formula[[2]][[1]], as.name("list")))
    return(list(formula)) # one-sided or LHS is single arg
  lapply(formula[[2]][-1], replace2, list = 2, x = formula)
}

# set all factors to characters
rbind_chr <- function(...)
{
  out <- rbind(...)
  if(is.data.frame(out))
  {
    idx <- vapply(out, is.factor, NA)
    out[idx] <- lapply(out[idx], as.character)
  }
  out
}
