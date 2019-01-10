## Purpose: internal functions (and methods) for tableby function
## Authors: Jason Sinnwell, Beth Atkinson
## Created: 9/4/2015

## Helper functions for modelsum:  merge, subset, and labels (work like names)

#' Helper functions for modelsum
#'
#' A set of helper functions for \code{\link{modelsum}}.
#'
#' @param object A \code{data.frame} resulting from evaluating a \code{modelsum} formula.
#' @param ... Other arguments, or a vector of indices for extracting.
#' @param x A \code{modelsum} object.
#' @inheritParams tableby.internal
#' @return \code{na.modelsum} returns a subsetted version of \code{object} (with attributes).
#' @seealso \code{\link{arsenal_table}}
#' @name modelsum.internal
NULL
#> NULL

starts_with <- function(x, prefix)
{
  substring(x, 1, nchar(prefix)) == prefix # this function is optimized in R >= 3.3 (startsWith)
}

join_formula <- function(x, y)
{
  x <- stats::formula(x)
  if(is.null(y)) return(x)
  y <- stats::formula(y)
  stopifnot(length(x) == 3 && length(y) == 2)
  x[[3]] <- call("+", x[[3]], y[[2]])
  x
}

#' @rdname modelsum.internal
#' @export
is.modelsum <- function(x) inherits(x, "modelsum")

#' @rdname modelsum.internal
#' @export
is.summary.modelsum <- function(x) inherits(x, "summary.modelsum")

#' @rdname modelsum.internal
#' @export
na.modelsum <- na_lhs_strata

##standardized beta function (for gaussian stat)
lm.beta  <- function (MOD) {
    b <- stats::coef(MOD)[-1]
    sx <- rep(NA,length(b))
    b.idx <- 1
    for(k in 2:ncol(MOD$model)) {
      ## skip factors and char variables,
      ## psplines consider doing sx, but need a second for loop for the ncol of those
      if(any(class(MOD$model[,k]) %in% c("character","factor", "pspline"))) {
        b.idx <- b.idx + ifelse(is.null(ncol(MOD$model[,k])), length(unique(MOD$model[,k]))-1, ncol(MOD$model[,k]))
        ## skip as many elements of beta as there are N.levels-1 of categorical variables
      } else {
        sx[b.idx] <- stats::sd(as.double(MOD$model[,k]),na.rm=TRUE)
        b.idx <- b.idx + 1
      }
    }
    sy <- stats::sd(as.double(MOD$model[,1]),na.rm=TRUE)
    beta <- c(NA,round(b * sx/sy,3))
    return(beta)
}
