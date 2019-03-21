#' summary.freqlist
#'
#' Summarize the \code{freqlist} object.
#'
#' @param object an object of class \code{\link{freqlist}}
#' @param ... For \code{summary.freqlist}, these are passed to \code{\link{as.data.frame.freqlist}} (and hence to
#'   \code{\link{freq.control}}). For the print method, these are
#'   additional arguments passed to the \code{\link[knitr]{kable}} function.
#' @param x An object of class \code{summary.freqlist}.
#' @inheritParams summary.tableby
#' @return An object of class \code{"summary.freqlist"} (invisibly for the print method).
#' @seealso \code{\link{freqlist}}, \code{\link[base]{table}}, \code{\link[stats]{xtabs}}, \code{\link[knitr]{kable}},
#'   \code{\link{freq.control}}, \code{\link{freqlist.internal}}
#'
#' @examples
#' # load mockstudy data
#' data(mockstudy)
#' tab.ex <- table(mockstudy[c("arm", "sex", "mdquality.s")], useNA = "ifany")
#' noby <- freqlist(tab.ex, na.options = "include")
#' summary(noby)
#' withby <- freqlist(tab.ex, strata = c("arm","sex"), na.options = "showexclude")
#' summary(withby)
#' summary(withby, dupLabels = TRUE)
#' @author Tina Gunderson, with major revisions by Ethan Heinzen
#' @name summary.freqlist
NULL
#> NULL

#' @rdname summary.freqlist
#' @export
summary.freqlist <- function(object, ..., labelTranslations = NULL, title = NULL)
{
  dat <- as.data.frame(object, ..., labelTranslations = labelTranslations, list.ok = TRUE)
  structure(list(
    object = set_attr(dat, "control", NULL),
    control = attr(dat, "control"),
    title = title
  ), class = c("summary.freqlist", "summary.arsenal_table"))
}

as_data_frame_summary_freqlist <- function(tb, labs, cntrl)
{
  fmtdups <- function(x, i)
  {
    x[i] <- lapply(x[i], as.character)
    if(nrow(x) == 0) return(x)
    tab <- as.matrix(x[i])
    tab[is.na(tab)] <- "NA"
    num <- max(stringr::str_count(tab, ","))

    for(col in seq_len(ncol(tab)))
    {
      tmp <- apply(tab[, 1:col, drop = FALSE], 1, paste, collapse = paste0(rep(",", num + 1), collapse = "")) # in R >= 3.3.0, we could use strrep instead
      x[c(FALSE, tmp[-1] == tmp[-length(tmp)]), colnames(tab)[col]] <- ""
    }
    x
  }

  fmtdigits <- function(x, digits.count, digits.pct)
  {
    if(nrow(x) == 0) return(x)
    if("Freq" %in% names(x)) x$Freq <- formatC(x$Freq, digits = digits.count, format = "f")
    if("cumFreq" %in% names(x)) x$cumFreq <- formatC(x$cumFreq, digits = digits.count, format = "f")
    if("freqPercent" %in% names(x)) x$freqPercent <- formatC(x$freqPercent, digits = digits.pct, format = "f")
    if("cumPercent" %in% names(x)) x$cumPercent <- formatC(x$cumPercent, digits = digits.pct, format = "f")
    x
  }
  idx <- names(tb) %nin% c("Freq", "cumFreq", "freqPercent", "cumPercent")
  tb <- fmtdigits(tb, digits.count = cntrl$digits.count, digits.pct = cntrl$digits.pct)
  if(!cntrl$dupLabels) tb <- fmtdups(tb, idx)
  tb <- stats::setNames(tb, labs[names(tb)])
  set_attr(set_attr(tb, "labels", NULL), "align", c("r", "l")[1 + idx])
}

#' @rdname summary.freqlist
#' @export
as.data.frame.summary.freqlist <- function(x, ..., list.ok = FALSE)
{
  out <- Map(x$object, lapply(x$object, attr, "labels"), f = as_data_frame_summary_freqlist, MoreArgs = list(cntrl = x$control))

  if(!list.ok)
  {
    if(length(out) == 1) out <- out[[1]] else warning("as.data.frame.summary.freqlist is returning a list of data.frames")
  }
  out
}

