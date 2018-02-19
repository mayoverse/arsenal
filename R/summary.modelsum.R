## Purpose: summary method for modelsum object
## Author: Greg Dougherty, Jason Sinnwell and Beth Atkinson
## Updated: 9/29/2015


#' Summarize a \code{modelsum} object.
#'
#' Format the information in \code{object} as a table using Pandoc coding or plain text, and cat it to stdout.
#'
#' @param object A \code{\link{modelsum}} object.
#' @param ... Other arguments; in \code{summary.modelsum}, these are passed to \code{\link{as.data.frame.modelsum}},
#'   and in \code{print.summary.modelsum}, these are passed to \code{\link[knitr]{kable}}.
#' @param labelTranslations A named list (or vector) where the name is the label in the
#'   output to be replaced in the pretty rendering of modelsum by the character
#'   string value for the named element of the list, e.g.,
#'   \code{list(age = "Age(years)")}.
#' @param text Logical, denoting whether to print out the text version.
#' @param title	Title for the table, defaults to \code{NULL} (no title)
#' @param x An object of class \code{"summary.modelsum"}.
#' @param format Passed to \code{\link[knitr]{kable}}: the format for the table. The default here is "markdown".
#'   To use the default in \code{kable}, pass \code{NULL}.
#' @seealso \code{\link{modelsum}}, \code{\link{print.modelsum}}, \code{\link{as.data.frame.modelsum}}
#' @return An object of class \code{"summary.modelsum"}
#' @author Ethan Heinzen, based on code originally by Greg Dougherty
#' @name summary.modelsum
NULL
#> NULL

#' @rdname summary.modelsum
#' @export
summary.modelsum <- function(object, ..., labelTranslations = NULL, text = FALSE, title = NULL)
{
  object <- as.data.frame(object, ..., labelTranslations = labelTranslations)
  structure(list(
    object = set_attr(object, "control", NULL),
    control = attr(object, "control"),
    text = text,
    title = title
  ), class = "summary.modelsum")
}

#' @rdname summary.modelsum
#' @export
print.summary.modelsum <- function(x, ..., format = "markdown")
{

  #### format the digits and nsmall things ####
  # integers, one-per-model
  use.digits0 <- c("Nmiss", "N", "Nmiss2", "Nevents", "df.residual", "df.null")

  # non-integers, one-per-model
  use.digits1 <- c("logLik", "AIC", "BIC", "null.deviance", "deviance",
                   "statistic.F", "dispersion", "statistic.sc", "concordance", "std.error.concordance",
                   "adj.r.squared", "r.squared")

  # non-integers, many-per-model
  use.digits2 <- c("estimate", "CI.lower.estimate", "CI.upper.estimate", "std.error", "statistic", "standard.estimate")

  use.digits.ratio <- c("OR", "CI.lower.OR", "CI.upper.OR", "RR", "CI.lower.RR", "CI.upper.RR", "HR", "CI.lower.HR", "CI.upper.HR")
  use.digits.p <- c("p.value.sc", "p.value.log", "p.value.wald", "p.value.F") #"p.value"

  df <- x$object
  cn <- colnames(df)

  df[cn %in% c(use.digits1, use.digits2)] <- lapply(df[cn %in% c(use.digits1, use.digits2)], formatC, digits = x$control$digits, format = "f")
  df[cn %in% use.digits.ratio] <- lapply(df[cn %in% use.digits.ratio], formatC, digits = x$control$digits.ratio, format = "f")
  df[cn %in% c("p.value", use.digits.p)] <- lapply(df[cn %in% c("p.value", use.digits.p)], formatC, digits = x$control$digits.p,
                                                     format = if(x$control$format.p) "f" else "g")

  if(x$control$format.p)
  {
    cutoff <- 10^(-x$control$digits.p)
    fmt <- paste0("< ", format(cutoff, digits = x$control$digits.p, format = "f"))

    for(tst in c("p.value", use.digits.p))
    {
      if(tst %in% cn) df[[tst]][x$object[[tst]] < cutoff] <- fmt
    }
  }

  #### don't show the same statistics more than once ####
  df[cn %in% c(use.digits0, use.digits1)] <- lapply(df[cn %in% c(use.digits0, use.digits1, use.digits.p)],
                                                    replace, list = duplicated(df$model), values = "")

  #### Format if necessary ####
  df$label <- trimws(df$label) # regardless of formatting
  if(!x$text) df$label <- ifelse(df$term.type == "Intercept", df$label, paste0("**", df$label, "**"))

  #### get rid of unnecessary columns ####
  df$model <- NULL
  df$term <- NULL
  df$term.type <- NULL


  #### tweak column names according to specifications ####
  cn <- stats::setNames(colnames(df), colnames(df))
  if(length(x$control$stat.labels) > 0)
  {
    nm <- intersect(cn, names(x$control$stat.labels))
    if(length(nm)) cn[nm] <- unlist(x$control$stat.labels[nm])
  }
  cn["label"] <- ""


  #### finally print it out ####
  if(!is.null(x$title)) cat("\nTable: ", x$title, sep = "")
  print(knitr::kable(df, col.names = cn, caption = NULL, format = format, ...))
  cat("\n")

  invisible(x)
}

