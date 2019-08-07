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
#' @inheritParams summary.tableby
#' @param x An object of class \code{"summary.modelsum"}.
#' @param adjustment.names Logical, denoting whether the names of the adjustment models should be printed.
#' @seealso \code{\link{modelsum}}, \code{\link{as.data.frame.modelsum}}
#' @return An object of class \code{"summary.modelsum"}
#' @author Ethan Heinzen, based on code originally by Greg Dougherty
#' @name summary.modelsum
NULL
#> NULL

#' @rdname summary.modelsum
#' @export
summary.modelsum <- function(object, ..., labelTranslations = NULL, text = FALSE, title = NULL, term.name = "", adjustment.names = FALSE)
{
  dat <- as.data.frame(object, ..., labelTranslations = labelTranslations, list.ok = TRUE)
  structure(list(
    object = set_attr(dat, "control", NULL),
    control = attr(dat, "control"),
    hasStrata = has_strata(object),
    text = text,
    title = title,
    term.name = term.name,
    adjustment.names = adjustment.names
  ), class = c("summary.modelsum", "summary.arsenal_table"))
}

as_data_frame_summary_modelsum <- function(df, control, hasStrata, term.name, text, adjustment.names, width, min.split)
{
  df.orig <- df

  #### format the digits and nsmall things ####
  # integers, one-per-model
  use.digits0 <- c("Nmiss", "N", "Nmiss2", "Nevents", "df.residual", "df.null", "edf")

  # non-integers, one-per-model
  use.digits1 <- c("logLik", "AIC", "BIC", "null.deviance", "deviance",
                   "statistic.F", "dispersion", "statistic.sc", "concordance", "std.error.concordance",
                   "adj.r.squared", "r.squared", "theta", "SE.theta")

  # non-integers, many-per-model
  use.digits2 <- c("estimate", "CI.lower.estimate", "CI.upper.estimate", "std.error", "statistic", "standard.estimate")

  use.digits.ratio <- c("OR", "CI.lower.OR", "CI.upper.OR", "RR", "CI.lower.RR", "CI.upper.RR", "HR", "CI.lower.HR", "CI.upper.HR")
  use.digits.p <- c("p.value.sc", "p.value.log", "p.value.wald", "p.value.F", "p.value.lrt") #"p.value"

  cn <- colnames(df)

  df[cn %in% c(use.digits1, use.digits2)] <- lapply(df[cn %in% c(use.digits1, use.digits2)], formatC, digits = control$digits, format = "f")
  df[cn %in% use.digits.ratio] <- lapply(df[cn %in% use.digits.ratio], formatC, digits = control$digits.ratio, format = "f")
  df[cn %in% c("p.value", use.digits.p)] <- lapply(df[cn %in% c("p.value", use.digits.p)], formatC, digits = control$digits.p,
                                                   format = if(control$format.p) "f" else "g")

  if(control$format.p)
  {
    cutoff <- 10^(-control$digits.p)
    fmt <- paste0("< ", format(cutoff, digits = control$digits.p, format = "f"))

    for(tst in c("p.value", use.digits.p))
    {
      if(tst %in% cn) df[[tst]][df.orig[[tst]] < cutoff] <- fmt
    }
  }
  df[cn %in% c("p.value", use.digits.p)] <- lapply(df[cn %in% c("p.value", use.digits.p)], sub,
                                                   pattern = "^\\s*NA\\s*$", replacement = "")

  #### don't show the same statistics more than once ####
  dups <- if(hasStrata) unlist(by(df, df[[4]], function(x) duplicated(x$model), simplify = FALSE), use.names = FALSE) else duplicated(df$model)
  df[cn %in% c(use.digits0, use.digits1, use.digits.p)] <- lapply(df[cn %in% c(use.digits0, use.digits1, use.digits.p)],
                                                                  replace, list = dups, values = "")
  if(hasStrata)
  {
    df[[4]] <- as.character(df[[4]])
    df[[4]][duplicated(df[[4]])] <- ""
  }

  #### get rid of unnecessary columns ####
  df$y.term <- NULL
  df$y.label <- NULL
  df$strata.term <- NULL

  df$model <- NULL
  df$term <- NULL
  term.type <- df$term.type
  df$term.type <- NULL
  if(!adjustment.names)
  {
    df$adjustment <- NULL
  } else df$adjustment[dups] <- ""

  #### Format if necessary ####
  if(!is.null(width))
  {
    firstcol <- smart.split(df[[1L + hasStrata]], width = width, min.split = min.split)
    lens <- vapply(firstcol, length, NA_integer_)

    df <- do.call(cbind.data.frame, c(list(label = unlist(firstcol, use.names = FALSE)), lapply(df[-1L - hasStrata], insert_elt, times = lens)))
    if(hasStrata) df <- df[replace(seq_along(df), 1:2, 2:1)]
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
  if(length(control$stat.labels) > 0)
  {
    nm <- intersect(cn, names(control$stat.labels))
    if(length(nm)) cn[nm] <- unlist(control$stat.labels[nm])
  }
  cn["label"] <- term.name
  colnames(df) <- cn

  set_attr(df, "align", rep("l", ncol(df)))
}
#' @rdname summary.modelsum
#' @export
as.data.frame.summary.modelsum <- function(x, ..., text = x$text, term.name = x$term.name, adjustment.names = x$adjustment.names,
                                           width = NULL, min.split = NULL, list.ok = FALSE)
{
  if(is.null(term.name) || identical(term.name, TRUE))
  {
    term.name <- vapply(x$object, attr, NA_character_, "ylabel")
  }
  stopifnot(length(term.name) <= length(x$object))
  out <- Map(as_data_frame_summary_modelsum, x$object, x$hasStrata, term.name,
             MoreArgs = list(control = x$control, text = text, width = width, min.split = min.split, adjustment.names = adjustment.names))
  if(!list.ok)
  {
    if(length(out) == 1) out <- out[[1]] else warning("as.data.frame.summary.modelsum is returning a list of data.frames")
  }
  out
}


