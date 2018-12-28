
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
#' @param value A list of new labels.
#' @param pdata A named data.frame where the first column is the by-variable names, the (optional) second is the strata value, the next is
#'   the x variable names, the next is p-values (or some test stat), and the (optional) next column is the method name.
#' @param e1,e2 \code{\link{tableby}} objects, or numbers to compare them to.
#' @param use.pname Logical, denoting whether the column name in \code{pdata} corresponding to the p-values should be used
#'   in the output of the object.
#' @param n A single integer. See \code{\link[utils]{head}} or \code{\link[utils]{tail}} for more details
#' @param lhs Logical, denoting whether to remove \code{NA}s from the first column of the data.frame (the "left-hand side")
#' @return \code{na.tableby} returns a subsetted version of \code{object} (with attributes). \code{Ops.tableby} returns
#'   a logical vector. \code{xtfrm.tableby} returns the p-values (which are ordered by \code{\link{order}} to \code{\link{sort}}).
#' @details
#' Logical comparisons are implemented for \code{Ops.tableby}.
#'
#' \code{xtfrm.tableby} also allows the use of \code{\link{order}} and \code{\link{sort}}.
#'
#' \code{length.tableby} also allows for the use of \code{\link[utils]{head}} and \code{\link[utils]{tail}}.
#' @name tableby.internal
NULL
#> NULL

#' @rdname tableby.internal
#' @export
is.tableby <- function(x) inherits(x, "tableby")

#' @rdname tableby.internal
#' @export
is.summary.tableby <- function(x) inherits(x, "summary.tableby")

#' @rdname tableby.internal
#' @export
merge.tableby <- function(x, y, ...) {
  stop("still do this")
  if(names(x$y) != names(y$y)) {
    stop("tableby objects cannot be merged unless same 'by' variable name).\n")
  }
  if(!all(names(x$y[[1]]$stats) == names(y$y[[1]]$stats))){
    stop("tableby objects cannot be merged unless same 'by' variable categories.\n")
  }
  newobj <- x
  y$y[[1]]$label <- paste0(y$y[[1]]$label, ".2")
  newobj$y[[paste0(names(y$y)[[1]],".2")]] <- y$y[[1]]
  for(xname in names(y$x)) {
    thisname <- xname
    ## if name already present, add "2" to name and add on
    if(xname %in% names(newobj$x)) {
      thisname <- paste0(xname, ".2")
      y$x[[xname]]$label <- paste0(y$x[[xname]]$label, ".2")
    }
    newobj$x[[thisname]] <- y$x[[xname]]
  }

  ## add on call and control from y
  newobj$Call2 <- y$Call
  newobj$control2 <- y$control

  return(newobj)
}

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

#' @rdname tableby.internal
#' @export
labels.tableby <- function(object, ...) {

  get_lab <- function(x)
  {
    xLabs <- lapply(x$tables, function(strat) vapply(strat, function(obj) obj$label, NA_character_))
    out <- c(setNames(x$y$label, x$y$term), unlist(unname(xLabs), recursive = FALSE))
    out[!duplicated(out) | !duplicated(names(out))]
  }

  labs <- unlist(unname(lapply(object$tables, get_lab)), recursive = FALSE)
  labs[!duplicated(labs) | !duplicated(names(labs))]
}

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


## assign labels to tableby object

#' @rdname tableby.internal
#' @export
'labels<-.tableby' <- function(x, value) {
  ## if the value vector is named, then assign the labels to
  ## those names that match those in x and y
  if(is.list(value)) value <- unlist(value)
  if(is.null(value))
  {
    for(i in seq_along(x$tables))
    {
      for(j in seq_along(x$tables[[i]]$x)) x$tables[[i]]$x[[j]]$label <- x$tables[[i]]$x[[j]]$term
      x$tables[[i]]$y$label <- x$tables[[i]]$y$term
      x$tables[[i]]$strata$label <- x$tables[[i]]$strata$term
    }
  } else if(!is.null(names(value))) {
    for(L in seq_along(value))
    {
      for(i in seq_along(x$tables))
      {
        nm <- names(value)[L]
        val <- unname(value[L])
        if(nm %in% names(x$tables[[i]]$x)) x$tables[[i]]$x[[nm]]$label <- val
        if(nm == x$tables[[i]]$y$term) x$tables[[i]]$y$label <- val
        if(nm == x$tables[[i]]$strata$term) x$tables[[i]]$strata$label <- val
      }
    }
  } else stop("Unnamed label assignments are no longer supported")
  x
}

## subset a tableby object;
## syntax of usage: newtb <- tbObj[1:2]
## x here is the tableby object
## index is in '...', and allows only 1 vector of integer indices
## in future, maybe allow subsetting by names

#' @rdname tableby.internal
#' @export
"[.tableby" <- function(x, i, j) {
  if(missing(i) && missing(j)) return(x)
  newx <- x

  give_warn <- function(vec) warning(paste0("Some indices not found in tableby object: ", paste0(vec, collapse = ", ")), call. = FALSE)
  if(!missing(j))
  {
    if(is.character(j) && any(tmp <- j %nin% names(newx$tables)))
    {
      give_warn(j[tmp])
      j <- j[!tmp]
    } else if(is.numeric(j) && any(tmp <- j %nin% seq_along(newx$tables)))
    {
      give_warn(j[tmp])
      j <- j[!tmp]
    } else if(is.logical(j) && length(j) != length(newx$tables))
    {
      stop("Logical vector index not the right length")
    }
    if(length(j) == 0 || anyNA(j)) stop("Indices must have nonzero length and no NAs.")
    newx$tables <- newx$tables[j]
  }

  if(!missing(i))
  {
    newx$tables <- lapply(newx$tables, function(yList) {
      if(is.character(i) && any(tmp <- i %nin% names(yList$x)))
      {
        give_warn(i[tmp])
        i <- i[!tmp]
      } else if(is.numeric(i) && any(tmp <- i %nin% seq_along(yList$x)))
      {
        give_warn(i[tmp])
        i <- i[!tmp]
      } else if(is.logical(i) && length(i) != length(yList$x))
      {
        stop("Logical vector index not the right length")
      }
      if(length(i) == 0 || anyNA(i)) stop("Indices must have nonzero length and no NAs.")
      yList$x <- yList$x[i]
      yList$tables <- lapply(yList$tables, "[", i)
      yList
    })
  }
  newx
}


## function to handle na.action for tableby formula, data.frame

#' @rdname tableby.internal
#' @export
na.tableby <- function(lhs = TRUE)
{
  if(is.data.frame(lhs)) stop("na.tableby now generates functions (and no longer accepts data.frames). ",
                              "Use 'na.tableby()' to generate the function that used to be 'na.tableby'.")
  if(lhs)
  {
    function(object, ...) {
      omit <- is.na(object[[1]])
      if("(strata)" %in% names(object)) omit <- omit | is.na(object[["(strata)"]])

      xx <- object[!omit, , drop = FALSE]
      if(any(omit)) {
        temp <- stats::setNames(seq_along(omit)[omit], attr(object, "row.names")[omit])
        attr(temp, "class") <- "omit"
        attr(xx, "na.action") <- temp
      }
      xx
    }
  } else
  {
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
