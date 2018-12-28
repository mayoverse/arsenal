
#' The summary method for a \code{tableby} object
#'
#' The summary method for a \code{\link{tableby}} object, which is a pretty rendering of a \code{\link{tableby}}
#' object into a publication-quality results table in R Markdown, and can render well in text-only.
#'
#' @param object An object of class \code{"tableby"}, made by the \code{\link{tableby}} function.
#' @param x An object of class \code{"summary.tableby"}.
#' @param ... For \code{summary.tableby}, other arguments passed to \code{\link{as.data.frame.tableby}}.
#'   For \code{print}ing the summary object, these are passed to both \code{as.data.frame.summary.tableby} and
#'   \code{\link[knitr]{kable}}.
#' @param title	Title for the table, defaulting to \code{NULL} (no title)
#' @param labelTranslations  A named list (or vector) where the name is the label in the
#'        output to be replaced in the pretty rendering of tableby by the character string
#'        value for the named element of the list, e.g., \code{list(age = "Age(Years)", meansd = "Mean(SD)")}.
#' @param text An argument denoting how to print the summary to the screen.
#'		Default is \code{FALSE} (show markdown output). \code{TRUE} and \code{NULL} output a text-only version, with
#'		the latter avoiding all formatting.
#'		\code{"html"} uses the HTML tag \code{<strong>} instead of the markdown formatting, and \code{"latex"} uses
#'		the LaTeX command \code{\\textbf}.
#' @param pfootnote Logical, denoting whether to put footnotes describing the tests used to generate the p-values.
#' @param term.name A character string denoting the column name for the first column.
#' @param format Passed to \code{\link[knitr]{kable}}: the format for the table. The default here is "markdown".
#'   To use the default in \code{kable}, pass \code{NULL}. If \code{x$text} specifies LaTeX or HTML formatting,
#'   that format is used in the table.
#' @param escape Passed to \code{\link[knitr]{kable}}: should special characters be escaped when printed?
#' @param width,min.split Passed to \code{\link{smart.split}} for formatting of the "term" column.
#' @return An object of class \code{summary.tableby}
#' @seealso \code{\link{tableby.control}}, \code{\link{tableby}}
#' @author Ethan Heinzen, based on code by Gregory Dougherty, Jason Sinnwell, Beth Atkinson,
#'   adapted from SAS Macros written by Paul Novotny and Ryan Lennon
#' @examples
#'
#' set.seed(100)
#' ## make 3+ categories for response
#' nsubj <- 90
#' mdat <- data.frame(Response=sample(c(1,2,3),nsubj, replace=TRUE),
#'                    Sex=sample(c("Male", "Female"), nsubj,replace=TRUE),
#'                    Age=round(rnorm(nsubj,mean=40, sd=5)),
#'                    HtIn=round(rnorm(nsubj,mean=65,sd=5)))
#'
#' ## allow default summaries on RHS variables
#' out <- tableby(Response ~ Sex + Age + HtIn, data=mdat)
#' summary(out, text=TRUE)
#' labels(out)
#' labels(out) <- c(Age="Age (years)", HtIn="Height (inches)")
#' summary(out, stats.labels=c(meansd="Mean-SD", q1q3 = "Q1-Q3"), text=TRUE)
#'
#' @name summary.tableby
NULL
#> NULL

#' @rdname summary.tableby
#' @export
summary.tableby <- function(object, ..., labelTranslations = NULL, text = FALSE, title = NULL, pfootnote = FALSE, term.name = "")
{
  dat <- as.data.frame(object, which = seq_along(object$tables), ..., labelTranslations = labelTranslations, list.ok = TRUE)
  structure(list(
    object = set_attr(dat, "control", NULL),
    control = attr(dat, "control"),
    totals = lapply(object$tables, function(x) x$y$stats),
    hasStrata = object$hasStrata,
    text = text,
    title = title,
    pfootnote = pfootnote,
    term.name = term.name
  ), class = "summary.tableby")
}

as_data_frame_summary_tableby <- function(df, totals, control, hasStrata, text, pfootnote, term.name, width, min.split)
{
  df.orig <- df
  idx <- names(df)[names(df) %nin% c("group.term", "group.label", "strata.term", "strata.label", "variable",
                                     "term", "label", "variable.type", "test", "p.value")]
  dgt <- attr(df, "control.list")

  f <- function(j, whch) if(is.null(dgt[[j]]) || is.null(dgt[[j]][[whch]])) control[[whch]] else dgt[[j]][[whch]]

  digits <- vapply(df$variable, function(i) f(i, "digits"), numeric(1))
  digits.count <- vapply(df$variable, function(i) f(i, "digits.count"), numeric(1))
  digits.pct <- vapply(df$variable, function(i) f(i, "digits.pct"), numeric(1))

  df[idx] <- lapply(df[idx], function(col) unlist(Map(format, col, digits = digits, format = "f", # the format="f" is not used for tbstat objects
                                                      digits.count = digits.count, digits.pct = digits.pct)))

  df$p.value <- formatC(df$p.value, digits = control$digits.p, format = if(control$format.p) "f" else "g")

  if(control$format.p)
  {
    cutoff <- 10^(-control$digits.p)
    fmt <- paste0("< ", format(cutoff, digits = control$digits.p, format = "f"))

    df$p.value[df.orig$p.value < cutoff] <- fmt
  }
  df$p.value[grepl("^\\s*NA$", df$p.value)] <- ""

  tests.used <- NULL
  if(control$test && pfootnote)
  {
    tests.used <- unique(df$test[df$test != "No test"])
    df$p.value <- ifelse(df$p.value == "", "", paste0(df$p.value, "^", as.integer(factor(df[["test"]], levels = tests.used)), "^"))
    tests.used <- paste0(seq_along(tests.used), ". ", tests.used)
  }

  #### don't show the same statistics more than once ####
  dups <- if(hasStrata) unlist(by(df, df[[4]], function(x) duplicated(x$variable), simplify = FALSE), use.names = FALSE) else duplicated(df$variable)
  df$p.value[dups] <- ""
  if(hasStrata)
  {
    df[[4]] <- as.character(df[[4]])
    df[[4]][duplicated(df[[4]])] <- ""
  }

  #### get rid of unnecessary columns ####
  df$group.term <- NULL
  df$group.label <- NULL
  df$strata.term <- NULL

  df$variable <- NULL
  df$term <- NULL
  df$test <- NULL
  df$variable.type <- NULL
  if(!control$test) df$p.value <- NULL
  if(!control$total) df[["Total"]] <- NULL

  #### Format if necessary ####
  if(!is.null(width))
  {
    firstcol <- smart.split(df[[1L + hasStrata]], width = width, min.split = min.split)
    lens <- vapply(firstcol, length, NA_integer_)

    df <- do.call(cbind.data.frame, c(list(label = unlist(firstcol, use.names = FALSE)), lapply(df[-1L - hasStrata], insert_elt, times = lens)))
    if(hasStrata) df <- df[replace(seq_along(df), 1:2, 2:1)]
    row.names(df) <- NULL
    dups <- insert_elt(dups, times = lens, elt = NULL)
  }
  df$label <- trimws(df$label)

  if(!is.null(text))
  {
    df$label <- if(identical(text, "html"))
    {
      ifelse(dups, paste0("&nbsp;&nbsp;&nbsp;", df$label), paste0("<strong>", df$label, "</strong>"))
    } else if(identical(text, "latex"))
    {
      ifelse(dups, paste0("~~~", df$label), paste0("\\textbf{", df$label, "}"))
    } else if(text)
    {
      ifelse(dups, paste0("-  ", df$label), df$label)
    } else ifelse(dups, paste0("&nbsp;&nbsp;&nbsp;", df$label), paste0("**", ifelse(df$label == "", "&nbsp;", df$label), "**"))
  }

  #### tweak column names according to specifications ####
  cn <- stats::setNames(colnames(df), colnames(df))
  align <- paste0(c(if(hasStrata) "l", "l", rep("c", times = sum(cn != "p.value")-1), if("p.value" %in% cn) "r"), collapse = "")
  nm <- intersect(cn, names(totals))
  if(length(nm)) cn[nm] <- paste0(cn[nm], " (N=", totals[nm], ")")
  cn["label"] <- term.name
  if("p.value" %in% cn && is.null(control$test.pname)) cn["p.value"] <- "p value" else if("p.value" %in% cn) cn["p.value"] <- control$test.pname
  colnames(df) <- cn

  set_attr(set_attr(df, "tests", tests.used), "align", align)
}

#' @rdname summary.tableby
#' @export
as.data.frame.summary.tableby <- function(x, ..., text = x$text, pfootnote = x$pfootnote, term.name = x$term.name, width = NULL, min.split = NULL, list.ok = FALSE)
{
  out <- Map(as_data_frame_summary_tableby, x$object, x$totals,
             MoreArgs = list(hasStrata = x$hasStrata, control = x$control, text = text, pfootnote = pfootnote,
                             term.name = term.name, width = width, min.split = min.split))
  if(!list.ok)
  {
    if(length(out) == 1) out <- out[[1]] else warning("as.data.frame.summary.tableby is returning a list of data.frames")
  }
  out
}

#' @rdname summary.tableby
#' @export
print.summary.tableby <- function(x, ..., format = if(!is.null(x$text) && x$text %in% c("html", "latex")) x$text else "markdown",
                                  escape = x$text %nin% c("html", "latex"), width = NULL, min.split = NULL)
{
  df <- as.data.frame(x, ..., width = width, min.split = min.split, list.ok = TRUE)

  if(!is.null(x$title)) cat("\nTable: ", x$title, sep = "")
  for(i in seq_along(df))
  {
    print(knitr::kable(df[[i]], caption = NULL, align = attr(df[[i]], "align"), format = format, row.names = FALSE, escape = escape, ...))
    if(!is.null(attr(df[[i]], "tests"))) cat(paste0(attr(df[[i]], "tests"), "\n", collapse = ""))
  }
  cat("\n")

  invisible(x)
}
