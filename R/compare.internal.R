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

  if("none" %nin% control$tol.vars)
  {
    for(elt in strsplit(control$tol.vars, "", fixed = TRUE))
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

cleanup.null.na <- function(x) if(is.null(x) || is.na(x)) NA_character_ else x

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
    idx <- unlist(Map(Negate(identical), var.x, var.y))
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
    out <- out[vapply(1:nrow(out), function(i) !identical(out$attr.x[[i]], out$attr.y[[i]]), logical(1)), , drop = FALSE]
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

#' Internal Comparison Functions
#'
#' Extract differences and number of differences from a compare object.
#'
#' @param object An object of class \code{compare.data.frame} or \code{summary.compare.data.frame}.
#' @param ... Other arguments (not in use at this time).
#' @param by.var Logical: should the number of differences by variable be reported, or should
#'   all differences be reported (the default).
#' @author Ethan Heinzen
#' @seealso \code{\link{compare.data.frame}} \code{\link{summary.compare.data.frame}}
#' @name comparison.internal
NULL
#> NULL


#' @rdname comparison.internal
#' @export
n.diffs <- function(object, ...)
{
  UseMethod("n.diffs")
}

#' @rdname comparison.internal
#' @export
n.diffs.compare.data.frame <- function(object, ...)
{
  sum(vapply(object$vars.summary$values, function(elt) if(is.data.frame(elt)) nrow(elt) else 0, numeric(1)))
}

#' @rdname comparison.internal
#' @export
n.diffs.summary.compare.data.frame <- function(object, ...)
{
  nrow(object$diffs.table)
}

#' @rdname comparison.internal
#' @export
diffs <- function(object, ...)
{
  UseMethod("diffs")
}

#' @rdname comparison.internal
#' @export
diffs.compare.data.frame <- function(object, ..., by.var = FALSE)
{
  if(!is.logical(by.var) || length(by.var) != 1) stop("'by.var' must be a single logical value.")
  diffs <- as.data.frame(object$vars.summary[idx_var_sum(object, "vars.compared"), c("var.x", "var.y", "values")])
  diffs$n <- vapply(diffs$values, nrow, numeric(1))
  sumNA <- function(df) sum(is.na(df$values.x) | is.na(df$values.y))
  diffs$NAs <- vapply(diffs$values, sumNA, numeric(1))

  if(by.var) return(diffs[, c("var.x", "var.y", "n", "NAs")])

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
  } else diffs.table <- cbind(var.x = character(0), var.y = character(0), diffs$values[[1]], stringsAsFactors = FALSE)

  diffs.table
}

#' @rdname comparison.internal
#' @export
diffs.summary.compare.data.frame <- function(object, ..., by.var = FALSE)
{
  if(by.var) object$diffs.byvar.table else object$diffs.table
}
