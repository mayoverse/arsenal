
#' The summary method for a \code{tableby} object
#'
#' The summary method for a \code{\link{tableby}} object, which is a pretty rendering of a \code{\link{tableby}}
#' object into a publication-quality results table in R Markdown, and can render well in text-only.
#'
#' @param object An object of class \code{"tableby"}, made by the \code{\link{tableby}} function.
#' @param ... Other arguments passed to \code{\link{as.data.frame.tableby}}.
#' @param title Title that will appear on the top of the header in the pretty-table rendering
#'		of the tableby object
#' @param labelTranslations  A named list (or vector) where the name is the label in the
#'        output to be replaced in the pretty rendering of tableby by the character string
#'        value for the named element of the list, e.g., \code{list(age = "Age(Years)", meansd = "Mean(SD)")}.
#' @param text Logical, tell R to print the raw text version of the summary to the screen.
#'		Default is \code{FALSE}, but recommended to be \code{TRUE} for interactive R session development.
#'
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

#' @export
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
