#' summary.freqlist
#'
#' Summarize the \code{freqlist} object.
#'
#' @param object an object of class \code{\link{freqlist}}
#' @param single a logical value indicating whether to collapse results created using a strata variable into a single table for printing
#' @param dupLabels Should labels which are the same as the row above be printed? The default (\code{FALSE}) more
#'   closely approximates \code{PROC FREQ} output from SAS, where a label carried down from the row above is left blank.
#' @param ... For \code{summary.freqlist}, these are not used. For the print method, these are
#'   additional arguments passed to the \code{\link[knitr]{kable}} function.
#' @param x An object of class \code{summary.freqlist}.
#' @inheritParams summary.tableby
#' @return An object of class \code{"summary.freqlist"} (invisibly for the print method).
#' @seealso \code{\link{freqlist}}, \code{\link[base]{table}}, \code{\link[stats]{xtabs}}, \code{\link[knitr]{kable}}
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
summary.freqlist <- function(object, single = FALSE, dupLabels = FALSE, ..., labelTranslations = NULL, title = NULL)
{
  dat <- as.data.frame(object, single = single, ..., labelTranslations = labelTranslations, list.ok = TRUE)
  structure(list(
    object = dat,
    title = title,
    single = single,
    dupLabels = dupLabels
  ), class = "summary.freqlist")
}

#' @rdname summary.freqlist
#' @export
as.data.frame.summary.freqlist <- function(x, ..., list.ok = FALSE)
{
  fmtdups <- function(x)
  {
    idx <- names(x) %nin% c("Freq", "cumFreq", "freqPercent", "cumPercent")
    x[idx] <- lapply(x[idx], as.character)
    tab <- as.matrix(x[idx])
    tab[is.na(tab)] <- "NA"
    num <- max(stringr::str_count(tab, ","))

    for(col in seq_len(ncol(tab)))
    {
      tmp <- apply(tab[, 1:col, drop = FALSE], 1, paste, collapse = paste0(rep(",", num + 1), collapse = "")) # in R >= 3.3.0, we could use strrep instead
      x[duplicated(tmp), colnames(tab)[col]] <- ""
    }
    x
  }

  out <- x$object
  if(!x$dupLabels) out <- lapply(out, fmtdups)
  out <- Map(out, lapply(out, attr, "labels"), f = function(x, labs) stats::setNames(x, labs[names(x)]))
  out <- lapply(out, set_attr, "labels", NULL)

  if(!list.ok)
  {
    if(length(out) == 1) out <- out[[1]] else warning("as.data.frame.summary.freqlist is returning a list of data.frames")
  }
  out
}


#' @rdname summary.freqlist
#' @export
print.summary.freqlist <- function(x, ..., format = "markdown")
{
  df <- as.data.frame(x, ..., list.ok = TRUE)

  #### finally print it out ####
  if(!is.null(x$title)) cat("\nTable: ", x$title, sep = "")
  for(i in seq_along(df))
  {
    print(knitr::kable(df[[i]], caption = NULL, format = format, row.names = FALSE, ...))
  }
  cat("\n")
  invisible(x)
}




