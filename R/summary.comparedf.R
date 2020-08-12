
#' The summary method for a \code{comparedf} object
#'
#' Print a more detailed output of the \code{\link{comparedf}} object.
#'
#' @param object An object of class \code{"comparedf"}, as made by the \code{\link{comparedf}} S3 method.
#' @param ... Other arguments passed to \code{\link{comparedf.control}}. In \code{print}, these are passed to \code{\link[knitr]{kable}}.
#' @param show.attrs Logical, denoting whether to show the actual attributes which are different. For (e.g.) factors with lots
#'   of levels, this can make the tables quite wide, so this feature is \code{FALSE} by default.
#' @param x An object returned by the \code{summary.comparedf} function.
#' @param format Passed to \code{\link[knitr]{kable}}: the format for the table. The default here is "pandoc".
#'   To use the default in \code{kable}, pass \code{NULL}.
#' @return An object of class \code{"summary.comparedf"} is returned.
#' @seealso \code{\link{comparedf}}, \code{\link{comparedf.control}}
#' @name summary.comparedf
NULL
#> NULL

#' @rdname summary.comparedf
#' @export
summary.comparedf <- function(object, ..., show.attrs = FALSE)
{
  control <- c(list(...), object$control)
  control <- do.call("comparedf.control", control[!duplicated(names(control))])

  #### start with summaries of the data.frames ####

  frame.summary <- as.data.frame(object$frame.summary[c("version", "arg", "ncol", "nrow")])

  #### after we've done all that, summaries of the overall comparison ####
  diffs.byvar <- diffs(object, by.var = TRUE)
  diffs.tab <- diffs(object)
  nobs.shared <- object$frame.summary$n.shared[1]
  nobs.uneq <- length(unique(diffs.tab$row.x))
  comparison.summary <- data.frame(
    statistic = c(
      "Number of by-variables",
      "Number of non-by variables in common", "Number of variables compared",
      "Number of variables in x but not y", "Number of variables in y but not x",
      "Number of variables compared with some values unequal", "Number of variables compared with all values equal",
      "Number of observations in common", "Number of observations in x but not y", "Number of observations in y but not x",
      "Number of observations with some compared variables unequal", "Number of observations with all compared variables equal",
      "Number of values unequal"
    ),
    value = c(
      (!attr(object$frame.summary$by, "byrow"))*sum(idx_var_sum(object, "by.variables")),
      sum(idx_var_sum(object, "nonby.vars.shared")), sum(idx_var_sum(object, "vars.compared")),
      sum(is.na(object$vars.summary$var.y)), sum(is.na(object$vars.summary$var.x)),
      sum(diffs.byvar$n > 0), sum(diffs.byvar$n == 0),
      nobs.shared, nrow(object$frame.summary$unique[[1]]), nrow(object$frame.summary$unique[[2]]),
      nobs.uneq, nobs.shared - nobs.uneq,
      n.diffs(object)
    ),
    stringsAsFactors = FALSE
  )

  #### start with differences in variables first ####
  get.vars.not.shared <- function(a, b)
  {
    var.diff.a <- object$vars.summary[is.na(object$vars.summary[[paste0("var.", b)]]), paste0(c("var.", "pos.", "class."), a)]
    var.diff.a2 <- cbind(version = rep(a, times = nrow(var.diff.a)), var.diff.a, stringsAsFactors = FALSE)
    colnames(var.diff.a2) <- c("version", "variable", "position", "class")
    var.diff.a2
  }

  vars.ns <- rbind(get.vars.not.shared("x", "y"), get.vars.not.shared("y", "x"))

  #### report variables not compared ####

  vars.nc <- as.data.frame(object$vars.summary[idx_var_sum(object, "vars.not.compared"),
                                               c("var.x", "pos.x", "class.x", "var.y", "pos.y", "class.y"), drop = FALSE])

  #### Now for the observations which aren't shared ####

  obs.ns <- diffs(object, what = "observations")

  #### Now for the actual differences ####

  # done in the structure statement

  #### Now for attributes ####

  attrs.tmp <- as.data.frame(object$vars.summary[idx_var_sum(object, "non.identical.attributes"), c("var.x", "var.y", "attrs"), drop = FALSE])
  attrs.diffs <- do.call(rbind, Map(cbind, var.x = attrs.tmp$var.x, var.y = attrs.tmp$var.y, attrs.tmp$attrs,
                                    MoreArgs = list(stringsAsFactors = FALSE)))
  if(is.null(attrs.diffs))
  {
    attrs.diffs <- data.frame(var.x = character(0), var.y = character(0), name = character(0), stringsAsFactors = FALSE)
  } else if(!show.attrs) attrs.diffs <- attrs.diffs[c("var.x", "var.y", "name")]

  structure(list(
    frame.summary.table = frame.summary,
    comparison.summary.table = comparison.summary,
    vars.ns.table = vars.ns, vars.nc.table = vars.nc, obs.table = obs.ns,
    diffs.byvar.table = diffs.byvar, diffs.table = diffs.tab,
    attrs.table = attrs.diffs,
    control = control
  ), class = "summary.comparedf")
}

#' @rdname summary.comparedf
#' @export
print.summary.comparedf <- function(x, ..., format = "pandoc")
{
  orig <- x
  sumdiffs <- sum(x$diffs.byvar.table$n)
  ctrl <- x$control
  ctrl$max.print.vars.ns <- ctrl$max.print.vars
  ctrl$max.print.vars.nc <- ctrl$max.print.vars

  if(is.null(ctrl$max.print.diffs.per.var) || is.na(ctrl$max.print.diffs.per.var)) ctrl$max.print.diffs.per.var <- sumdiffs
  if(nrow(x$diffs.table) > 0)
  {
    x$diffs.table <- do.call(rbind, by(x$diffs.table, factor(x$diffs.table$var.x, levels = unique(x$diffs.table$var.x)),
                                       utils::head, ctrl$max.print.diffs.per.var))

    # Need this for knitr to output list-cols of factors and dates correctly
    as_char <- function(x) if(is.factor(x) || is.Date(x)) x <- as.character(x) else x
    x$diffs.table$values.x <- lapply(x$diffs.table$values.x, as_char)
    x$diffs.table$values.y <- lapply(x$diffs.table$values.y, as_char)
  }

  for(v in c("frame.summary", "comparison.summary", "vars.ns", "vars.nc", "obs", "diffs.byvar", "diffs", "attrs"))
  {
    obj <- x[[paste0(v, ".table")]]
    nprint <- ctrl[[paste0("max.print.", v)]]

    # there is purposefully no max.print.frame.summary or max.print.comparison.summary
    if(is.null(nprint) || is.na(nprint)) nprint <- nrow(obj)

    caption <- switch(
      v,
      frame.summary = "Summary of data.frames",
      comparison.summary = "Summary of overall comparison",
      vars.ns = "Variables not shared",
      vars.nc = "Other variables not compared",
      obs = "Observations not shared",
      diffs.byvar = "Differences detected by variable",
      diffs = "Differences detected",
      attrs = "Non-identical attributes"
    )
    if(nrow(obj) > 0)
    {
      if(v == "diffs" && sumdiffs > min(nprint, nrow(obj)))
      {
        caption <- paste0(caption, " (", sumdiffs - min(nprint, nrow(obj)), " not shown)")
      } else if(nrow(obj) > nprint)
      {
        caption <- paste0(caption, " (", nrow(obj) - nprint, " not shown)")
      }
      print(knitr::kable(utils::head(obj, nprint), format = format, caption = caption, row.names = FALSE, ...))
    } else
    {
      nocaption <- paste0("No ", tolower(caption))
      print(knitr::kable(data.frame(x = nocaption), format = format, caption = caption, row.names = FALSE, col.names = "", ...))
    }
    cat("\n")
  }
  invisible(orig)
}

