
defaultLabelTranslations <- function()
{
  list(Nmiss = "N-miss", Nmiss2 = "N-miss", meansd = "Mean (SD)", q1q3 = "Q1, Q3",
							range = "Range", Nevents = "Events", medsurv = "Median Survival")
}


#' The summary method for a \code{tableby} object
#'
#' The summary method for a \code{\link{tableby}} object, which is a pretty rendering of a \code{\link{tableby}}
#' object into a publication-quality results table in R-studio, and can render well in text-only.
#'
#' @param object An object of class \code{"tableby"}, made by the \code{\link{tableby}} function.
#' @param title Title that will appear on the top of the header in the pretty-table rendering
#'		of the tableby object
#' @param labelTranslations  A named list (or vector) where the name is the label in the
#'        output to be replaced in the pretty rendering of tableby by the character string
#'        value for the named element of the list, e.g., \code{list(age = "Age(Years)", meansd = "Mean(SD)")}.
#'	  This applies to both the statistic labels and the variables from the formula.
#' @param digits Digits to round for significant digits of numeric, non-integer values.
#'		If \code{digits.test} is not set, \code{digits} is used for that setting.
#' @param nsmall Minimum number of digits to the right of the decimal point to display
#'		for floating point numbers.  If \code{NA} (default), it uses the value from
#'		\code{object$control$nsmall}. Allowed non-\code{NA} values are \code{0 <= nsmall <= 20}.
#' @param nsmall.pct Minimum number of digits to the right of the decimal point to display
#'		for percent numbers.  If \code{NA} (default), it uses the value from \code{object$control$nsmall.pct}.
#' @param digits.test Significant digits by which to round for numeric test statistic p-values,
#'		if the test was performed.
#' @param text Logical, tell R to print the raw text version of the summary to the screen.
#'		Default is \code{FALSE}, but recommended to be \code{TRUE} for interactive R session development.
#' @param removeBlanks Logical, remove extra blanks in the pretty rendering of the table
#' @param labelSize Relative size difference between label column and other columns.
#'		Default is 1.2: label column ~20\% bigger than other columns.
#' @param test Logical, denoting whether the "p value" value should be printed.
#'		If \code{NA} (default), it uses the value from \code{object$control$test}.
#' @param test.pname Title for p-value (only matters if test is \code{TRUE}; default is "p value").
#' @param pfootnote Logical, denoting whether to add a footnote describing the test used to
#'		generate the p value. Default is \code{FALSE}.
#' @param total Logical, denoting whether to include the "total" value.
#'		If \code{NA} (default), it uses the value from \code{object$control$total}.
#' @param ... Other arguments (not in use at this time).
#' @details
#' For text-only, simply paste the summary stats together per variable, along with p-value and totals,
#' with group variable in the header.  For other formats, the paste is done into a pandoc-style markup
#' such that it can be translated into 3 formats: latex, html, rtf.  The decision of which of those it
#' is translated to is left for run-time for whatever format into which the report is being generated.
#'
#' For all interative development within R sessions, \code{text=TRUE} is recommended.
#'
#' @return Results are cat'ed to stdout, and returned invisibly as a data.frame of the \code{tableby}
#' @seealso \code{\link{tableby.control}}, \code{\link{tableby}}
#' @author Gregory Dougherty, Jason Sinnwell, Beth Atkinson, adapted from SAS Macros written by Paul Novotny and Ryan Lennon
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
#' summary(out, labelTranslations=c(meansd="Mean-SD"), text=TRUE)
#'
#' @export
summary.tableby <- function(object, ..., labelTranslations = NULL, text = FALSE, title = NULL)
{
  dat <- as.data.frame(object, ..., labelTranslations = labelTranslations)
  structure(list(
    object = set_attr(dat, "control", NULL),
    control = attr(dat, "control"),
    totals = object$y[[1]]$stats,
    text = text,
    title = title
  ), class = "summary.tableby")
}

print.summary.tableby <- function(x, ...)
{

  df <- x$object

  idx <- colnames(df) %nin% c("variable", "term", "label", "variable.type", "test", "p.value")
  format_all <- function(x, ...) vapply(x, format, NA_character_, ...)
  df[idx] <- lapply(df[idx], format_all, digits = x$control$digits,
                    digits.count = x$control$digits.count, digits.pct = x$control$digits.pct)
  df[["p.value"]] <- formatC(df[["p.value"]], digits = x$control$digits.p, format = if(x$control$format.p) "f" else "g")

  if(x$control$format.p)
  {
    cutoff <- 10^(-x$control$digits.p)
    fmt <- paste0("< ", format(cutoff, digits = x$control$digits.p, format = "f"))

    df[["p.value"]][x$object[["p.value"]] < cutoff] <- fmt
  }

  #### don't show the same statistics more than once ####
  df[["p.value"]] <- replace(df[["p.value"]], duplicated(df$variable), "")

  #### Format if necessary ####
  df$label <- trimws(df$label) # regardless of formatting
  if(x$text)
  {
    df$label <- ifelse(duplicated(df$variable), paste0("-  ", df$label), df$label)
  } else df$label <- ifelse(duplicated(df$variable), paste0("&nbsp;&nbsp;&nbsp;", df$label), paste0("**", df$label, "**"))

  #### get rid of unnecessary columns ####
  df$variable <- NULL
  df$term <- NULL
  df$test <- NULL
  df$variable.type <- NULL
  if(!x$control$test) df$p.value <- NULL
  if(!x$control$total) df[["Total"]] <- NULL

  #### tweak column names according to specifications ####
  cn <- stats::setNames(colnames(df), colnames(df))
  align <- paste0(c(rep("l", times = sum(cn != "p.value")), if("p.value" %in% cn) "r"), collapse = "")
  nm <- intersect(cn, names(x$totals))
  if(length(nm)) cn[nm] <- paste0(cn[nm], " (N=", x$totals[nm], ")")
  cn["label"] <- ""
  if("p.value" %in% cn && is.null(x$control$test.pname)) cn["p.value"] <- "p value" else if("p.value" %in% cn) cn["p.value"] <- x$control$test.pname

  #### finally print it out ####
  if(!is.null(x$title)) cat("\nTable: ", x$title, sep = "")
  print(knitr::kable(df, col.names = cn, caption = NULL, align = align))

  invisible(x)
}
