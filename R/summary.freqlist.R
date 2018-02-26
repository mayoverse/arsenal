#' summary.freqlist
#'
#' Summarize the \code{freqlist} object.
#'
#' @param object an object of class \code{\link{freqlist}}
#' @param single a logical value indicating whether to collapse results created using a groupBy variable into a single table for printing
#' @param labelTranslations A named list (or vector) where the name is the label in the output to
#'        be replaced in the pretty rendering of freqlist by the character string value for the named
#'        element of the list, e.g., list(age="Age(years)", bmi="Body Mass Index").
#' @param dupLabels Should labels which are the same as the row above be printed? The default (\code{FALSE}) more
#'   closely approximates \code{PROC FREQ} output from SAS, where a label carried down from the row above is left blank.
#' @param title	Title for the table, defaults to \code{NULL} (no title)
#' @param ... For \code{summary.freqlist}, these are not used. For the print method, these are
#'   additional arguments passed to the \code{\link[knitr]{kable}} function.
#' @param x An object of class \code{summary.freqlist}.
#' @param format Passed to \code{\link[knitr]{kable}}: the format for the table. The default here is "markdown".
#'   To use the default in \code{kable}, pass \code{NULL}.
#' @return An object of class \code{"summary.freqlist"} (invisibly for the print method).
#' @seealso \code{\link{freqlist}}, \code{\link[base]{table}}, \code{\link[stats]{xtabs}}, \code{\link[knitr]{kable}}
#'
#' @examples
#' # load mockstudy data
#' data(mockstudy)
#' tab.ex <- table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA = "ifany")
#' noby <- freqlist(tab.ex, na.options = "include")
#' summary(noby)
#' withby <- freqlist(tab.ex, groupBy = c("arm","sex"), na.options = "showexclude")
#' summary(withby)
#' summary(withby, dupLabels = TRUE)
#' @author Tina Gunderson, with major revisions by Ethan Heinzen
#' @name summary.freqlist
NULL
#> NULL

#' @rdname summary.freqlist
#' @export
summary.freqlist <- function(object, single = FALSE, labelTranslations = NULL, dupLabels = FALSE, title = NULL, ..., format = "markdown")
{
  if(!is.logical(single) || length(single) != 1) stop("'single' must be TRUE or FALSE")
  if(!is.null(labelTranslations)) labels(object) <- labelTranslations
  if(!is.logical(dupLabels) || length(dupLabels) != 1) stop("'dupLabels' must be TRUE or FALSE")
  if("caption" %in% names(list(...))) .Defunct(msg = "Using 'caption = ' is defunct. Use 'title = ' instead.")

  fmtdups <- function(tab)
  {
    tab <- as.matrix(tab)
    tab[is.na(tab)] <- "NA"
    output <- tab
    num <- max(stringr::str_count(tab, ","))

    for(col in 1:ncol(tab))
    {
      tmp <- apply(tab[, 1:col, drop = FALSE], 1, paste, collapse = paste0(rep(",", num + 1), collapse = "")) # in R >= 3.3.0, we could use strrep instead
      output[duplicated(tmp), col] <- ""
    }
    output
  }

  cnames <- if(is.null(object$labels)) names(object$freqlist) else c(object$labels, "Freq", "cumFreq", "freqPercent", "cumPercent")
  freqdf <- object$freqlist

  if(is.null(object$byVar) || single)
  {
    if(ncol(freqdf) > 5 && !dupLabels)
    {
      freqdf[, 1:(ncol(freqdf)-4)] <- fmtdups(freqdf[, 1:(ncol(freqdf)-4), drop = FALSE])
    }
    freqdf <- list(freqdf)
  } else
  {
    byVar <- object$byVar
    freqdf[byVar] <- lapply(freqdf[byVar], function(x) if(anyNA(x)) addNA(x) else x)
    freqdf <- by(freqdf, freqdf[, rev(byVar)], FUN = data.frame)

    f <- function(x)
    {
      if(!is.null(x) && nrow(x) > 1 && !dupLabels) x[, 1:(ncol(x)-4)] <- fmtdups(x[, 1:(ncol(x)-4), drop = FALSE])
      x
    }
    freqdf <- lapply(freqdf, f)
  }
  freqdf <- lapply(freqdf, stats::setNames, cnames)
  structure(list(object = freqdf, title = title), class = "summary.freqlist")
}

#' @rdname summary.freqlist
#' @export
print.summary.freqlist <- function(x, ..., format = "markdown")
{
  if(!is.null(x$title)) cat("\nTable: ", x$title, sep = "")
  f <- function(x, ...) print(knitr::kable(x, ...))
  lapply(x$object, f, row.names = FALSE, caption = NULL, format = format, ...)
  cat("\n")
  invisible(x)
}
