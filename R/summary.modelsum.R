## Purpose: summary method for modelsum object
## Author: Greg Dougherty, Jason Sinnwell and Beth Atkinson
## Updated: 9/29/2015


#' Summarize a \code{modelsum} object.
#'
#' Format the information in \code{object} as a table using Pandoc coding or plain text, and cat it to stdout.
#'
#' @param object A \code{\link{modelsum}} object.
#' @param ... For \code{summary.modelsum}, other arguments passed to \code{\link{as.data.frame.modelsum}}.
#'   For \code{as.data.frame.summary.modelsum}, "width" and "min.split" are passed to \code{\link{smart.split}}.
#'   For \code{print}ing the summary object, these are passed to both \code{as.data.frame.summary.modelsum} and
#'   \code{\link[knitr]{kable}}.
#' @param labelTranslations A named list (or vector) where the name is the label in the
#'   output to be replaced in the pretty rendering of modelsum by the character
#'   string value for the named element of the list, e.g.,
#'   \code{list(age = "Age(years)")}.
#' @inheritParams summary.tableby
#' @param x An object of class \code{"summary.modelsum"}.
#' @seealso \code{\link{modelsum}}, \code{\link{print.modelsum}}, \code{\link{as.data.frame.modelsum}}
#' @return An object of class \code{"summary.modelsum"}
#' @author Ethan Heinzen, based on code originally by Greg Dougherty
#' @name summary.modelsum
NULL
#> NULL

#' @rdname summary.modelsum
#' @export
summary.modelsum <- function(object, ..., labelTranslations = NULL, text = FALSE, title = NULL, term.name = "")
{
  object <- as.data.frame(object, ..., labelTranslations = labelTranslations)
  structure(list(
    object = set_attr(object, "control", NULL),
    control = attr(object, "control"),
    text = text,
    title = title,
    term.name = term.name
  ), class = "summary.modelsum")
}

#' @rdname summary.modelsum
#' @export
as.data.frame.summary.modelsum <- function(x, ..., text = x$text, term.name = x$term.name)
{

  #### format the digits and nsmall things ####
  # integers, one-per-model
  use.digits0 <- c("Nmiss", "N", "Nmiss2", "Nevents", "df.residual", "df.null")

  # non-integers, one-per-model
  use.digits1 <- c("logLik", "AIC", "BIC", "null.deviance", "deviance",
                   "statistic.F", "dispersion", "statistic.sc", "concordance", "std.error.concordance",
                   "adj.r.squared", "r.squared", "edf")

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

  #### get rid of unnecessary columns ####
  df$model <- NULL
  df$term <- NULL
  term.type <- df$term.type
  df$term.type <- NULL

  #### Format if necessary ####
  opts <- list(...)
  if(!is.null(width <- opts$width))
  {
    firstcol <- smart.split(df[[1L]], width = width, min.split = opts$min.split)
    lens <- vapply(firstcol, length, NA_integer_)

    df <- do.call(cbind.data.frame, c(list(label = unlist(firstcol, use.names = FALSE)), lapply(df[-1L], insert_elt, times = lens)))
    row.names(df) <- NULL
    term.type <- insert_elt(term.type, times = lens, elt = NULL)
  }
  df$label <- trimws(df$label)

  if(!is.null(text))
  {
    if(identical(text, "html"))
    {
      df$label <- ifelse(term.type == "Intercept", df$label, paste0("<strong>", df$label, "</strong>"))
    } else if(identical(text, "latex"))
    {
      df$label <- ifelse(term.type == "Intercept", df$label, paste0("\\textbf{", df$label, "}"))
    } else if(!text)
    {
      df$label <- ifelse(term.type == "Intercept", df$label, paste0("**", ifelse(df$label == "", "&nbsp;", df$label), "**"))
    }
  }

  #### tweak column names according to specifications ####
  cn <- stats::setNames(colnames(df), colnames(df))
  if(length(x$control$stat.labels) > 0)
  {
    nm <- intersect(cn, names(x$control$stat.labels))
    if(length(nm)) cn[nm] <- unlist(x$control$stat.labels[nm])
  }
  cn["label"] <- term.name
  colnames(df) <- cn

  df
}

#' @rdname summary.modelsum
#' @export
print.summary.modelsum <- function(x, ..., format = if(!is.null(x$text) && x$text %in% c("html", "latex")) x$text else "markdown",
                                   escape = x$text %nin% c("html", "latex"))
{
  df <- as.data.frame(x, ...)

  #### finally print it out ####
  if(!is.null(x$title)) cat("\nTable: ", x$title, sep = "")
  print(knitr::kable(df, caption = NULL, format = format, row.names = FALSE, escape = escape, ...))
  cat("\n")

  invisible(x)
}


