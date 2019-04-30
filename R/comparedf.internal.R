tweakcolnames <- function(by.x, by.y, cn.x, cn.y, control)
{
  if(any(by.x %in% setdiff(cn.y, by.y)))
  {
    stop("A by-variable for x appears in non-by-variables for y. base::merge() will not work in this function.")
  }
  if(anyDuplicated(cn.x) || anyDuplicated(cn.y)) stop("Sorry, there are duplicate colnames.")

  cn.y[match(by.y, cn.y)] <- by <- by.x
  rm(by.x, by.y) # just to make sure we don't use it again

  rn <- "..row.names.." %in% by

  # don't replace the characters for "..row.names.."
  if(rn)
  {
    if(cn.x[length(cn.x)] != "..row.names.." || cn.y[length(cn.y)] != "..row.names..")
    {
      stop("Something went wrong with the row.names.")
    }
    cn.x <- cn.x[- length(cn.x)]
    cn.y <- cn.y[- length(cn.y)]
  }

  tv <- control$tol.vars
  if(!is.null(names(tv)))
  {
    for(i in seq_along(tv))
    {
      if(bad1 <- names(tv)[i] %nin% cn.x)
        warning("Variable tolerance '", names(tv)[i], "' not found in colnames of x")
      if(bad2 <- tv[i] %nin% cn.y)
        warning("Variable tolerance '", tv[i], "' not found in colnames of y")
      if(!bad1 && !bad2) cn.y[cn.y == tv[i]] <- names(tv)[i]
    }
    # no need to do anything with the by-variables, since we've already set those to x
  } else if("none" %nin% tv)
  {
    for(elt in strsplit(tv, "", fixed = TRUE))
    {
      if(length(elt) == 1)
      {
        cn.x <- gsub(elt, "", cn.x, fixed = TRUE)
        cn.y <- gsub(elt, "", cn.y, fixed = TRUE)
        if(!rn) by <- gsub(elt, "", by, fixed = TRUE)
      } else
      {
        for(elt2 in elt[-1])
        {
          # I know I could do some fancy regex work here, but for readibility, the for-loop is easiest.
          cn.x <- gsub(elt2, elt[1], cn.x, fixed = TRUE)
          cn.y <- gsub(elt2, elt[1], cn.y, fixed = TRUE)
          if(!rn) by <- gsub(elt2, elt[1], by, fixed = TRUE)
        }
      }
    }
    if(anyDuplicated(cn.x) || anyDuplicated(cn.y)) stop("'tol.vars' resulted in duplicate colnames.")
  }

  if(rn)
  {
    cn.x <- c(cn.x, "..row.names..")
    cn.y <- c(cn.y, "..row.names..")
  }

  return(list(by = by, cn.x = cn.x, cn.y = cn.y))
}

cleanup.null.na <- function(x) if(is.null(x) || allNA(x)) NA_character_ else x

compare_values <- function(i, v, df, byvars, contr)
{
  if(is.na(v$var.x[i]) || is.na(v$var.y[i])) return(NULL)
  if(v$tmp[i] %in% byvars) return("by-variable")

  var <- v$tmp[i]
  var.x <- df[[paste0(var, ".x")]]
  var.y <- df[[paste0(var, ".y")]]

  int.num <- function(vr) is.integer(vr) || is.numeric(vr)
  fac.chr <- function(vr) is.factor(vr)  || is.character(vr)

  if(!identical(v$class.x[i], v$class.y[i]) &&
     !(contr$int.as.num && int.num(var.x) && int.num(var.y)) &&
     !(contr$factor.as.char && fac.chr(var.x) && fac.chr(var.y))) return("Not compared")

  ## Technically, we can compare these as-is, but let's be explicit about it.
  if(!identical(v$class.x[i], v$class.y[i]) && contr$int.as.num && int.num(var.x) && int.num(var.y))
  {
    var.x <- as.numeric(var.x)
    var.y <- as.numeric(var.y)
  }

  if(!identical(v$class.x[i], v$class.y[i]) && contr$factor.as.char && fac.chr(var.x) && fac.chr(var.y))
  {
    var.x <- as.character(var.x)
    var.y <- as.character(var.y)
  }

  if(is.logical(var.x) && is.logical(var.y))
  {
    idx <- contr$tol.logical(var.x, var.y)
  } else if(is.numeric(var.x) && is.numeric(var.y)) # this covers integers, too
  {
    idx <- contr$tol.num(var.x, var.y, contr$tol.num.val)
  } else if(is.factor(var.x) && is.factor(var.y))
  {
    idx <- contr$tol.factor(var.x, var.y)

  } else if(is.character(var.x) && is.character(var.y))
  {
    idx <- contr$tol.char(var.x, var.y)
  } else if(is.Date(var.x) && is.Date(var.y))
  {
    idx <- contr$tol.date(var.x, var.y, contr$tol.date.val)
  } else
  {
    idx <- contr$tol.other(var.x, var.y)
  }

  out <- data.frame(values.x = I(var.x[idx]), # just in case list-column
                    values.y = I(var.y[idx]),
                    row.x = df[["..row.x.."]][idx],
                    row.y = df[["..row.y.."]][idx])

  return(cbind(df[idx, byvars, drop = FALSE], out))
}

compare_attrs <- function(i, v, x_, y_)
{
  if(is.na(v$var.x[i]) || is.na(v$var.y[i])) return(NULL)
  attr.x <- attributes(x_[[v$var.x[i]]])
  attr.y <- attributes(y_[[v$var.y[i]]])
  if(is.null(attr.x) && is.null(attr.y)) return(NULL)

  empty <- data.frame(name = character(0), attr = I(list()), stringsAsFactors = FALSE)

  out <- merge(if(!is.null(attr.x)) data.frame(name = names(attr.x), attr = I(attr.x), stringsAsFactors = FALSE) else empty,
               if(!is.null(attr.y)) data.frame(name = names(attr.y), attr = I(attr.y), stringsAsFactors = FALSE) else empty,
               by = "name", all = TRUE)
  out$attr.x <- lapply(out$attr.x, cleanup.null.na)
  out$attr.y <- lapply(out$attr.y, cleanup.null.na)

  if(nrow(out) > 0)
  {
    out <- out[order(out$name), , drop = FALSE]
    out <- out[vapply(seq_len(nrow(out)), function(i) !identical(out$attr.x[[i]], out$attr.y[[i]]), logical(1)), , drop = FALSE]
  }
  out
}


####################################################################################################
####################################################################################################
####################################################################################################

idx_var_sum <- function(object, which = c("vars.not.shared", "vars.compared", "vars.not.compared", "differences.found", "non.identical.attributes"))
{
  which <- match.arg(which, several.ok = FALSE)
  if(which == "vars.not.shared")
  {
    vapply(object$vars.summary$values, is.null, logical(1))
  } else if(which == "vars.not.compared")
  {
    vapply(object$vars.summary$values, identical, logical(1), y = "Not compared")
  } else if(which == "vars.compared")
  {
    vapply(object$vars.summary$values, function(elt) is.data.frame(elt), logical(1))
  } else if(which == "differences.found")
  {
    vapply(object$vars.summary$values, function(elt) is.data.frame(elt) && nrow(elt) > 0, logical(1))
  } else if(which == "non.identical.attributes")
  {
    vapply(object$vars.summary$attrs, function(elt) is.data.frame(elt) && nrow(elt) > 0, logical(1))
  }
}

#' Extract differences
#'
#' Extract differences, number of differences, or number of not-shared observations from a \code{comparedf} object.
#'
#' @param object An object of class \code{comparedf} or \code{summary.comparedf}.
#' @param vars A character vector of variable names to subset the results to.
#' @param ... Other arguments (not in use at this time).
#' @param by.var Logical: should the number of differences by variable be reported, or should
#'   all differences be reported (the default).
#' @author Ethan Heinzen
#' @seealso \code{\link{comparedf}} \code{\link{summary.comparedf}}
#' @name diffs
NULL
#> NULL


#' @rdname diffs
#' @export
n.diff.obs <- function(object, ...)
{
  UseMethod("n.diff.obs")
}

#' @rdname diffs
#' @export
n.diff.obs.comparedf <- function(object, ...)
{
  nrow(object$frame.summary$unique[[1]]) + nrow(object$frame.summary$unique[[2]])
}

#' @rdname diffs
#' @export
n.diff.obs.summary.comparedf <- function(object, ...)
{
  nrow(object$obs.table)
}

#' @rdname diffs
#' @export
n.diffs <- function(object, ...)
{
  UseMethod("n.diffs")
}

#' @rdname diffs
#' @export
n.diffs.comparedf <- function(object, ...)
{
  sum(vapply(object$vars.summary$values, function(elt) if(is.data.frame(elt)) nrow(elt) else 0, numeric(1)))
}

#' @rdname diffs
#' @export
n.diffs.summary.comparedf <- function(object, ...)
{
  nrow(object$diffs.table)
}

#' @rdname diffs
#' @export
diffs <- function(object, ...)
{
  UseMethod("diffs")
}

#' @rdname diffs
#' @export
diffs.comparedf <- function(object, vars = NULL, ..., by.var = FALSE)
{
  if(!is.logical(by.var) || length(by.var) != 1) stop("'by.var' must be a single logical value.")
  diffs <- as.data.frame(object$vars.summary[idx_var_sum(object, "vars.compared"), c("var.x", "var.y", "values")])
  diffs$n <- vapply(diffs$values, nrow, numeric(1))
  sumNA <- function(df) sum(is.na(df$values.x) | is.na(df$values.y))
  diffs$NAs <- vapply(diffs$values, sumNA, numeric(1))

  if(is.null(vars)) vars <- unique(c(diffs$var.x, diffs$var.y)) else if(!is.character(vars)) stop("'vars' should be NULL or a character vector.")

  rownames(diffs) <- NULL
  if(by.var) return(diffs[diffs$var.x %in% vars | diffs$var.y %in% vars, c("var.x", "var.y", "n", "NAs"), drop = FALSE])

  tolist <- function(df)
  {
    df$values.x <- I(as.list(df$values.x)) # need the I() for factors and dates to show up right
    df$values.y <- I(as.list(df$values.y))
    df
  }

  diffs1 <- diffs[diffs$n > 0, , drop = FALSE]
  if(nrow(diffs1) > 0)
  {
    diffs.table <- do.call(rbind, lapply(Map(cbind, var.x = diffs1$var.x, var.y = diffs1$var.y, diffs1$values,
                                             MoreArgs = list(stringsAsFactors = FALSE)), tolist))
  } else
  {
    diffs.table <- data.frame(var.x = character(0), var.y = character(0), stringsAsFactors = FALSE)
    diffs.table[object$frame.summary$by[[1]]] <- rep(list(character(0)), length(object$frame.summary$by[[1]]))
    diffs.table$values.y <- diffs.table$values.x <- I(list())
    diffs.table$row.y <- diffs.table$row.x <- integer(0)
  }

  rownames(diffs.table) <- NULL
  diffs.table[diffs.table$var.x %in% vars | diffs.table$var.y %in% vars, , drop = FALSE]
}

#' @rdname diffs
#' @export
diffs.summary.comparedf <- function(object, vars = NULL, ..., by.var = FALSE)
{
  tmp <- if(by.var) object$diffs.byvar.table else object$diffs.table

  if(is.null(vars)) vars <- unique(tmp$var.x, tmp$var.y) else if(!is.character(vars)) stop("'vars' should be NULL or a character vector.")

  tmp[tmp$var.x %in% vars | tmp$var.y %in% vars, , drop = FALSE]
}
