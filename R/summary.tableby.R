
#' The summary method for a \code{tableby} object
#'
#' The summary method for a \code{\link{tableby}} object, which is a pretty rendering of a \code{\link{tableby}}
#' object into a publication-quality results table in R Markdown, and can render well in text-only.
#'
#' @param object An object of class \code{"tableby"}, made by the \code{\link{tableby}} function.
#' @param x An object of class \code{"summary.tableby"}.
#' @param ... For \code{summary.tableby}, other arguments passed to \code{\link{as.data.frame.tableby}}.
#'   For \code{as.data.frame.summary.tableby}, "width" and "min.split" are passed to \code{\link{smart.split}}.
#'   For \code{print}ing the summary object, these are passed to both \code{as.data.frame.summary.tableby} and
#'   \code{\link[knitr]{kable}}.
#' @param title Title that will appear on the top of the header in the pretty-table rendering
#'		of the tableby object
#' @param labelTranslations  A named list (or vector) where the name is the label in the
#'        output to be replaced in the pretty rendering of tableby by the character string
#'        value for the named element of the list, e.g., \code{list(age = "Age(Years)", meansd = "Mean(SD)")}.
#' @param text Logical, tell R to print the raw text version of the summary to the screen.
#'		Default is \code{FALSE}, but recommended to be \code{TRUE} for interactive R session development. For
#'		\code{as.data.frame}, this can be set to \code{NULL} to avoid changing the labels at all.
#' @param pfootnote Logical, denoting whether to put footnotes describing the tests used to generate the p-values.
#' @param format Passed to \code{\link[knitr]{kable}}: the format for the table. The default here is "markdown".
#'   To use the default in \code{kable}, pass \code{NULL}.
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
summary.tableby <- function(object, ..., labelTranslations = NULL, text = FALSE, title = NULL, pfootnote = FALSE)
{
  dat <- as.data.frame(object, ..., labelTranslations = labelTranslations)
  structure(list(
    object = set_attr(dat, "control", NULL),
    control = attr(dat, "control"),
    totals = object$y[[1]]$stats,
    text = text,
    title = title,
    pfootnote = pfootnote
  ), class = "summary.tableby")
}

#' @rdname summary.tableby
#' @export
as.data.frame.summary.tableby <- function(x, ..., text = x$text, pfootnote = x$pfootnote)
{
  df <- x$object

  idx <- colnames(df) %nin% c("variable", "term", "label", "variable.type", "test", "p.value")
  format_all <- function(x, ...) vapply(x, format, NA_character_, ...)
  df[idx] <- lapply(df[idx], format_all, digits = x$control$digits, format = "f", # the format="f" is not used for tbstat objects
                    digits.count = x$control$digits.count, digits.pct = x$control$digits.pct)
  df[["p.value"]] <- formatC(df[["p.value"]], digits = x$control$digits.p, format = if(x$control$format.p) "f" else "g")

  if(x$control$format.p)
  {
    cutoff <- 10^(-x$control$digits.p)
    fmt <- paste0("< ", format(cutoff, digits = x$control$digits.p, format = "f"))

    df[["p.value"]][x$object[["p.value"]] < cutoff] <- fmt
  }

  tests.used <- NULL
  if(x$control$test && pfootnote)
  {
    tests.used <- unique(df$test)
    df[["p.value"]] <- paste0(df[["p.value"]], "^", as.integer(factor(df[["test"]], levels = tests.used)), "^")
    tests.used <- paste0(seq_along(tests.used), ". ", tests.used)
  }

  #### don't show the same statistics more than once ####
  dups <- duplicated(df$variable)
  df[["p.value"]] <- replace(df[["p.value"]], dups, "")

  #### get rid of unnecessary columns ####
  df$variable <- NULL
  df$term <- NULL
  df$test <- NULL
  df$variable.type <- NULL
  if(!x$control$test) df$p.value <- NULL
  if(!x$control$total) df[["Total"]] <- NULL

  #### Format if necessary ####
  opts <- list(...)
  if(!is.null(width <- opts$width))
  {
    firstcol <- smart.split(df[[1L]], width = width, min.split = opts$min.split)
    lens <- vapply(firstcol, length, NA_integer_)

    df <- do.call(cbind.data.frame, c(list(label = unlist(firstcol, use.names = FALSE)), lapply(df[-1L], insert_elt, times = lens)))
    row.names(df) <- NULL
    dups <- insert_elt(dups, times = lens, elt = NULL)
  }
  df$label <- trimws(df$label)

  if(!is.null(text))
  {
    if(text)
    {
      df$label <- ifelse(dups, paste0("-  ", df$label), df$label)
    } else df$label <- ifelse(dups, paste0("&nbsp;&nbsp;&nbsp;", df$label), paste0("**", ifelse(df$label == "", "&nbsp;", df$label), "**"))
  }

  #### tweak column names according to specifications ####
  cn <- stats::setNames(colnames(df), colnames(df))
  align <- paste0(c("l", rep("c", times = sum(cn != "p.value")-1), if("p.value" %in% cn) "r"), collapse = "")
  nm <- intersect(cn, names(x$totals))
  if(length(nm)) cn[nm] <- paste0(cn[nm], " (N=", x$totals[nm], ")")
  cn["label"] <- ""
  if("p.value" %in% cn && is.null(x$control$test.pname)) cn["p.value"] <- "p value" else if("p.value" %in% cn) cn["p.value"] <- x$control$test.pname
  colnames(df) <- cn

  set_attr(set_attr(df, "tests", tests.used), "align", align)
}

#' @rdname summary.tableby
#' @export
print.summary.tableby <- function(x, ..., format = "markdown")
{
  df <- as.data.frame(x, ...)

  #### finally print it out ####
  if(!is.null(x$title)) cat("\nTable: ", x$title, sep = "")
  print(knitr::kable(df, caption = NULL, align = attr(df, "align"), format = format, ...))
  if(!is.null(attr(df, "tests"))) cat(paste0(attr(df, "tests"), "\n", collapse = ""))
  cat("\n")

  invisible(x)
}
