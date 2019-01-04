
## allow stat functions to be passed as single arguments that are strings of function names
## Store this as attribute in the modeldf column, along with the actual name of the variable,
## rather than anova(age) showing up in the result (though anova(age) will be the column name in modeldf
## but we pull these attributes off later.
inline_tableby_stat_test <- function(x, ..., digits = NULL, digits.count = NULL, digits.pct = NULL, cat.simplify = NULL, numeric.simplify = NULL)
{
  attr(x, "name") <- deparse(substitute(x))
  attr(x, "stats") <- if(missing(...)) NULL else list(...)
  attr(x, "control.list") <- list(digits = digits, digits.count = digits.count, digits.pct = digits.pct,
                                  cat.simplify = cat.simplify, numeric.simplify = numeric.simplify)
  x
}

get_attr <- function(x, which, default)
{
  x <- attr(x, which, exact = TRUE)
  if(is.null(x)) default else x
}

#' @export
format.tbstat <- function(x, digits = NULL, ...)
{
  x <- x[] # to remove classes
  if(is.numeric(x)) x <- trimws(formatC(x, digits = digits, format = "f"))
  if(length(x) == 1) return(paste0(x))

  parens <- get_attr(x, "parens", c("", ""))
  sep <- get_attr(x, "sep", " ")
  sep2 <- get_attr(x, "sep2", " ")
  pct <- get_attr(x, "pct", "")
  if(length(x) == 2)
  {
    paste0(x[1], sep, parens[1], x[2], pct, parens[2])
  } else paste0(x[1], sep, parens[1], x[2], sep2, x[3], parens[2])
}

#' @export
format.tbstat_countpct <- function(x, digits.count = NULL, digits.pct = NULL, ...)
{
  att <- attributes(x)
  x <- if(length(x) == 2)
  {
    c(formatC(x[1], digits = digits.count, format = "f"), formatC(x[2], digits = digits.pct, format = "f"))
  } else formatC(x[1], digits = digits.count, format = "f")
  attributes(x) <- att
  NextMethod("format")
}

#' Internal \code{tableby} functions
#'
#' A collection of functions that may help users create custom functions that are formatted correctly.
#' @param x Usually a vector.
#' @param oldClass class(es) to add to the resulting object.
#' @param sep The separator between \code{x[1]} and the rest of the vector.
#' @param parens A length-2 vector denoting parentheses to use around \code{x[2]} and \code{x[3]}.
#' @param sep2 The separator between \code{x[2]} and \code{x[3]}.
#' @param pct The symbol to use after percents.
#' @param ... arguments to pass to \code{as.tbstat}.
#' @details
#'   \code{as.tbstat} defines a tableby statistic with its appropriate formatting.
#'
#'   \code{as.countpct} adds another class to \code{as.tbstat} to use different "digits" arguments. See \code{\link{tableby.control}}.
#'
#'   \code{as.tbstat_multirow} marks an object (usually a list) for multiple-row printing.
#' @name tableby.stats.internal
NULL
#> NULL

#' @rdname tableby.stats.internal
#' @export
as.tbstat <- function(x, oldClass = NULL, sep = NULL, parens = NULL, sep2 = NULL, pct = NULL)
{
  structure(x, class = c("tbstat", oldClass),
            sep = sep, parens = parens, sep2 = sep2, pct = pct)
}

#' @rdname tableby.stats.internal
#' @export
as.countpct <- function(x, ...)
{
  tmp <- as.tbstat(x, ...)
  class(tmp) <- c("tbstat_countpct", class(tmp))
  tmp
}

#' @rdname tableby.stats.internal
#' @export
as.tbstat_multirow <- function(x)
{
  class(x) <- c("tbstat_multirow", class(x))
  x
}

extract_tbstat <- function(x, ...)
{
  x <- NextMethod("[")
  class(x) <- class(x)[class(x) %nin% c("tbstat", "tbstat_countpct", "tbstat_multirow")]
  x
}

extract2_tbstat <- function(x, ...)
{
  x <- NextMethod("[[")
  class(x) <- class(x)[class(x) %nin% c("tbstat", "tbstat_countpct", "tbstat_multirow")]
  x
}

#' @export
`[.tbstat` <- extract_tbstat
#' @export
`[.tbstat_countpct` <- extract_tbstat
#' @export
`[.tbstat_multirow` <- extract_tbstat
#' @export
`[[.tbstat` <- extract2_tbstat
#' @export
`[[.tbstat_countpct` <- extract2_tbstat
#' @export
`[[.tbsta_multirowt` <- extract2_tbstat


## merge two tableby objects
## both must have same "by" variable and levels
## if some RHS variables have same names, keep both, the one in y add ".y"

#' Helper functions for tableby
#'
#' A set of helper functions for \code{\link{tableby}}.
#'
#' @param object A \code{data.frame} resulting from evaluating a \code{tableby} formula.
#' @param ... Other arguments, or a vector of indices for extracting.
#' @param x,y A \code{tableby} object.
#' @param i,j A vector to index \code{x} with: either names of variables, a numeric vector, or a logical vector of appropriate length.
#'   \code{i} indexes the x-variables, and \code{j} indexes the by-variables.
#' @param warn A logical, indicating whether to warn if some indices aren't found.
#' @param value A list of new labels.
#' @param pdata A named data.frame where the first column is the by-variable names, the (optional) second is the strata value, the next is
#'   the x variable names, the next is p-values (or some test stat), and the (optional) next column is the method name.
#' @param e1,e2 \code{\link{tableby}} objects, or numbers to compare them to.
#' @param all,all.x,all.y Logicals, denoting which terms to keep if not all are in common.
#' @param use.pname Logical, denoting whether the column name in \code{pdata} corresponding to the p-values should be used
#'   in the output of the object.
#' @param n A single integer. See \code{\link[utils]{head}} or \code{\link[utils]{tail}} for more details
#' @param lhs Logical, denoting whether to remove \code{NA}s from the first column of the data.frame (the "left-hand side")
#' @return \code{na.tableby} returns a subsetted version of \code{object} (with attributes). \code{Ops.tableby} returns
#'   a logical vector. \code{xtfrm.tableby} returns the p-values (which are ordered by \code{\link{order}} to \code{\link{sort}}).
#' @details
#' Logical comparisons are implemented for \code{Ops.tableby}.
#' @seealso \code{\link{merge}}, \code{\link{sort}}, \code{\link[utils]{head}}, \code{\link[utils]{tail}}, \code{\link{labels}}
#' @name tableby.internal
NULL
#> NULL

#' @rdname tableby.internal
#' @export
is.tableby <- function(x) inherits(x, "tableby")

#' @rdname tableby.internal
#' @export
is.summary.tableby <- function(x) inherits(x, "summary.tableby")

## pdata is a named data.frame where the first column is the x variable names matched by name,
## p-values (or some test stat) are numbers and the name is matched
## method name is the third column (optional)
## to the x variable in the tableby object (x)

#' @rdname tableby.internal
#' @export
modpval.tableby <- function(x, pdata, use.pname=FALSE) {
  ## set control$test to TRUE
  if(any(pdata[[1]] %in% names(x$tables))) {
    x$control$test <- TRUE

    ## change test results
    for(k in seq_len(nrow(pdata))) {
      yname <- pdata[[1]][k]

      strat <- if(x$hasStrata) pdata[[2]][k] else ""
      xname <- pdata[[2 + x$hasStrata]][k]
      p <- pdata[[3 + x$hasStrata]][k]
      method <- if(ncol(pdata) > 3 + x$hasStrata) pdata[[4 + x$hasStrata]][k] else "modified by user"

      if(xname %in% names(x$tables[[yname]]$x) && strat %in% x$tables[[yname]]$strata$values)
      {
        idx <- which(x$tables[[yname]]$strata$values == strat)
        stopifnot(length(idx) == 1)

        x$tables[[yname]]$tables[[idx]][[xname]]$test$p.value <- p
        x$tables[[yname]]$tables[[idx]][[xname]]$test$method <- method
      }
    }
    if(use.pname & nchar(names(pdata)[2])>0) {
      ## put different test column name in control
      x$control$test.pname <- names(pdata)[2]
    }
  } else warning("Couldn't match any by-variables to the first column of 'x'.")
  return(x)
}

## Get the labels from the tableby object's elements in the order they appear in the fomula/Call
## including the y and x variables
# labels <- function(x) {
#   UseMethod("labels")
# }

## retrieve variable labels (y, x-vec) from tableby object


## define generic function for tests, so tests(tbObj) will work

#' @rdname tableby.internal
#' @export
tests <- function(x) UseMethod("tests")

## retrieve the names of the tests performed per variable
#' @rdname tableby.internal
#' @export
tests.tableby <- function(x) {
  if(x$control$test) {
    df <- as.data.frame(x, list.ok = TRUE)
    testdf <- do.call(rbind_chr, lapply(df, function(i) i[c("group.label", if(x$hasStrata) names(i)[4], "variable", "p.value", "test")]))

    testdf <- unique(testdf)
    row.names(testdf) <- NULL
    names(testdf)[c(1, x$hasStrata + (2:4))] <- c("Group", "Variable",
                                                  if(!is.null(x$control$test.pname)) x$control$test.pname else "p.value", "Method")
  } else {
    cat("No tests run on tableby object\n")
    testdf <- NULL
  }
  testdf
}

## function to handle na.action for tableby formula, data.frame

#' @rdname tableby.internal
#' @export
na.tableby <- function(lhs = TRUE)
{
  if(is.data.frame(lhs)) stop("na.tableby now generates functions (and no longer accepts data.frames). ",
                              "Use 'na.tableby()' to generate the function that used to be 'na.tableby'.")
  if(lhs) return(na_lhs_strata)

  function(object, ...) {
    omit <- if("(strata)" %in% names(object)) is.na(object[["(strata)"]]) else rep(FALSE, nrow(object))

    xx <- object[!omit, , drop = FALSE]
    if(any(omit)) {
      temp <- stats::setNames(seq_along(omit)[omit], attr(object, "row.names")[omit])
      attr(temp, "class") <- "omit"
      attr(xx, "na.action") <- temp
    }
    xx
  }
}

#' @rdname tableby.internal
#' @export
xtfrm.tableby <- function(x)
{
  if(!x$control$test) stop("Can't extract p-values from a tableby object created with test=FALSE.")
  unlist(lapply(x$tables, function(lst) lapply(lst$tables, function(strat) lapply(strat, function(i) i$test$p.value))), use.names = FALSE)
}

#' @rdname tableby.internal
#' @export
sort.tableby <- function(x, ...)
{
  if(!x$control$test) stop("Can't sort a tableby object created with test=FALSE.")
  if(x$hasStrata || length(x$tables) > 1) stop("Can't sort a tableby object with strata or multiple by variables")
  NextMethod()
}

#' @rdname tableby.internal
#' @export
Ops.tableby <- function(e1, e2)
{
  ok <- switch(.Generic, `<` = , `>` = , `<=` = , `>=` = , `==` = , `!=` = TRUE, FALSE)
  if(!ok) stop("'", .Generic, "' is not meaningful for tableby objects")

  if(inherits(e1, "tableby")) e1 <- xtfrm(e1)
  if(inherits(e2, "tableby")) e2 <- xtfrm(e2)
  get(.Generic, mode = "function")(e1, e2)
}

#' @rdname tableby.internal
#' @export
head.tableby <- function(x, n = 6L, ...)
{
  stopifnot(length(n) == 1L)
  xlen <- unique(vapply(x$tables, function(obj) length(obj$x), NA_integer_))
  if(length(xlen) != 1) stop("length isn't defined for tableby objects with differing x-variables per by-variable")
  n <- if(n < 0L) max(xlen + n, 0L) else min(n, xlen)
  x[seq_len(n)]
}

#' @rdname tableby.internal
#' @export
tail.tableby <- function(x, n = 6L, ...)
{
  stopifnot(length(n) == 1L)
  xlen <- unique(vapply(x$tables, function(obj) length(obj$x), NA_integer_))
  if(length(xlen) != 1) stop("length isn't defined for tableby objects with differing x-variables per by-variable")
  n <- if(n < 0L) max(xlen + n, 0L) else min(n, xlen)
  x[seq.int(to = xlen, length.out = n)]
}
