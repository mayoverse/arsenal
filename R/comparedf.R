
# original function mockup done by Andy Hanson; re-envisioned by EPH starting 3/20/17

#' Compare two data.frames and report differences
#'
#' Compare two data.frames and report any differences between them,
#'  much like SAS's \code{PROC COMPARE} procedure.
#'
#' @param x,y A data.frame to compare
#' @param by,by.x,by.y Which variables are IDs to merge the two data.frames?
#'   If set to \code{"row.names"}, merging will occur over the row.names.
#'   If set to \code{NULL} (default), merging will occur row-by-row.
#' @param control A list of control parameters from \code{\link{comparison.control}}.
#' @param ... Other arguments, passed to \code{\link{comparison.control}} when appropriate.
#' @examples
#'
#' df1 <- data.frame(id = paste0("person", 1:3), a = c("a", "b", "c"),
#'                   b = c(1, 3, 4), c = c("f", "e", "d"),
#'                   row.names = paste0("rn", 1:3), stringsAsFactors = FALSE)
#' df2 <- data.frame(id = paste0("person", 3:1), a = c("c", "b", "a"),
#'                   b = c(1, 3, 4), d = paste0("rn", 1:3),
#'                   row.names = paste0("rn", c(1,3,2)), stringsAsFactors = FALSE)
#' summary(compare(df1, df2))
#' summary(compare(df1, df2, by = "id"))
#' summary(compare(df1, df2, by = "row.names"))
#' @author Ethan Heinzen, adapted from code from Andrew Hanson
#' @seealso \code{\link{summary.compare.data.frame}}, \code{\link{n.diffs}}, \code{\link{n.diff.obs}}
#' @name compare.data.frame
NULL
#> NULL

#' @rdname compare.data.frame
#' @export
compare.data.frame <- function(x, y, by = NULL, by.x = by, by.y = by, control = NULL, ...) {

  control <- c(list(...), control)
  control <- do.call("comparison.control", control[!duplicated(names(control))])

  xname <- paste0(deparse(substitute(x)), collapse = "")
  yname <- paste0(deparse(substitute(y)), collapse = "")

  if(!is.data.frame(x) || !is.data.frame(y))
  {
    stop("Both 'x' and 'y' must be data.frames")
  }

  if(any(c("..row.x..", "..row.y..", "..row.names..") %in% c(colnames(x), colnames(y))))
  {
    stop("'..row.x..', '..row.y..', and '..row.names..' are reserved colnames in this function.")
  }

  if(is.null(by) && is.null(by.x) && is.null(by.y))
  {
    # user didn't supply any by-variables, so we'll merge by row
    by.x <- by.y <- "row.names"
    byrow <- TRUE
  } else if(is.null(by.x) || is.null(by.y))
  {
    stop("Either 'by' or both of 'by.x' and 'by.y' must be specified")
  } else byrow <- FALSE

  if(any(by.x %nin% c("row.names", colnames(x))) || any(by.y %nin% c("row.names", colnames(y))))
  {
    stop("One or more of 'by.x' doesn't match colnames(x) or 'by.y' doesn't match colnames(y).")
  }

  #### data frame summary ####

  frame.summary <- data.frame(
    version = c("x", "y"),
    arg = c(xname, yname),
    ncol = c(ncol(x), ncol(y)),
    nrow = c(nrow(x), nrow(y)), stringsAsFactors = FALSE
  )
  frame.summary$by <- list(by.x, by.y)
  frame.summary$attrs <- list(attributes(x), attributes(y))

  if("row.names" %in% by.x)
  {
    x[["..row.names.."]] <- if(byrow) seq_len(nrow(x)) else row.names(x)
    by.x <- "..row.names.."
  }
  if("row.names" %in% by.y)
  {
    y[["..row.names.."]] <- if(byrow) seq_len(nrow(y)) else row.names(y)
    by.y <- "..row.names.."
  }


  #### tweak the column names ####

  tcn <- tweakcolnames(by.x, by.y, colnames(x), colnames(y), control)
  rm(by.x, by.y) # just to make sure we don't try to use those anymore
  by <- tcn$by

  #### now merge the things together ####

  together <- merge(cbind(stats::setNames(x, tcn$cn.x), ..row.x.. = seq_len(nrow(x))),
                    cbind(stats::setNames(y, tcn$cn.y), ..row.y.. = seq_len(nrow(y))), by = by, all = TRUE)

  both <- together[!is.na(together[["..row.x.."]]) & !is.na(together[["..row.y.."]]), , drop = FALSE]

  #### get the unshared observations ####

  getunique <- function(whch, whch2, by. = by)
  {
    tmp <- together[is.na(together[[whch2]]), c(by., whch), drop = FALSE]
    colnames(tmp)[colnames(tmp) == whch] <- "observation"
    tmp
  }

  frame.summary$unique <- list(getunique("..row.x..", "..row.y.."), getunique("..row.y..", "..row.x.."))
  frame.summary$n.shared <- rep(nrow(both), times = 2)

  #### Make the main object in this function ####

  mkdf <- function(df, cn.new)
  {
    df2 <- data.frame(tmp = cn.new, var = colnames(df), pos = seq_along(df), stringsAsFactors = FALSE)
    df2$class <- unname(lapply(df, class))
    df2
  }

  vars.summary <- merge(mkdf(x, tcn$cn.x), mkdf(y, tcn$cn.y), by = "tmp", all = TRUE)
  ord <- order(is.na(vars.summary$var.x), is.na(vars.summary$var.y), vars.summary$pos.x, vars.summary$pos.y, na.last = TRUE)
  vars.summary <- vars.summary[ord, , drop = FALSE]
  row.names(vars.summary) <- NULL

  vars.summary$class.x <- lapply(vars.summary$class.x, cleanup.null.na)
  vars.summary$class.y <- lapply(vars.summary$class.y, cleanup.null.na)

  vars.summary$values <- lapply(seq_len(nrow(vars.summary)), compare_values, v = vars.summary, df = both, byvars = by, contr = control)
  vars.summary$attrs <- lapply(seq_len(nrow(vars.summary)), compare_attrs, v = vars.summary, x_ = x, y_ = y)
  vars.summary$tmp <- NULL

  structure(list(frame.summary = structure(frame.summary, class = c("compare.data.frame.frame.summary", "data.frame")),
                 vars.summary = structure(vars.summary, class = c("compare.data.frame.vars.summary", "data.frame")),
                 control = control, Call = match.call()), class = "compare.data.frame")
}

#' @rdname compare.data.frame
#' @export
print.compare.data.frame <- function(x, ...)
{
  cat("Compare Object\n\n")
  cat("Function Call: \n")
  print(x$Call)
  cat("\n")
  cat("Shared: ", sum(!idx_var_sum(x, "vars.not.shared")), " variables and ", x$frame.summary$n.shared[1], " observations.\n", sep = "")
  cat("Not shared: ", sum(idx_var_sum(x, "vars.not.shared")), " variables and ",
      n.diff.obs(x), " observations.\n", sep = "")
  cat("\n")
  cat("Differences found in ", sum(idx_var_sum(x, "differences.found")), "/", sum(idx_var_sum(x, "vars.compared")), " variables compared.\n", sep = "")
  cat(sum(idx_var_sum(x, "non.identical.attributes")), " variables compared have non-identical attributes.\n", sep = "")
  invisible(x)
}

#' @export
print.compare.data.frame.vars.summary <- function(x, ...)
{
  orig <- x
  f <- function(elt, txt1, txt2)
  {
    if(is.data.frame(elt)) paste0(nrow(elt), txt1) else if(is.null(elt)) txt2 else elt
  }

  x$values <- vapply(x$values, f, character(1), txt1 = " differences", txt2 = "Not compared")
  x$attrs <- vapply(x$attrs, f, character(1), txt1 = " attributes", txt2 = "0 attributes")
  NextMethod()
  invisible(orig)
}


#' @export
print.compare.data.frame.frame.summary <- function(x, ...)
{
  orig <- x
  f <- function(elt, txt1, txt2)
  {
    if(is.data.frame(elt)) paste0(nrow(elt), txt1) else if(is.list(elt)) paste0(length(elt), txt1) else if(is.null(elt)) txt2 else elt
  }

  x$attrs <- vapply(x$attrs, f, character(1), txt1 = " attributes", txt2 = "0 attributes")
  x$unique <- vapply(x$unique, f, character(1), txt1 = " unique obs", txt2 = "")
  NextMethod()
  invisible(orig)
}
