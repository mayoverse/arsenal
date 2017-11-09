#' summary.freqlist
#'
#' Summarize the \code{freqlist} object
#'
#' @param object an object of class \code{\link{freqlist}}
#' @param single a logical value indicating whether to collapse results created using a groupBy variable into a single table for printing
#' @param labelTranslations A named list (or vector) where the name is the label in the output to
#'        be replaced in the pretty rendering of freqlist by the character string value for the named
#'        element of the list, e.g., list(age="Age(years)", bmi="Body Mass Index").
#' @param dupLabels Should labels which are the same as the row above be printed? The default (\code{FALSE}) more
#'   closely approximates \code{PROC FREQ} output from SAS, where a label carried down from the row above is left blank.
#' @param title	Title for the table, defaults to \code{NULL} (no title)
#' @param ... additional arguments passed to the \code{\link[knitr]{kable}} function (e.g., \code{format = "pandoc"})
#' @return Invisibly returns \code{object}, and uses \code{\link[knitr]{kable}} to print the object.
#' @seealso \code{\link[base]{table}}, \code{\link[stats]{xtabs}}, \code{\link[knitr]{kable}}
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
#' @author Tina Gunderson
#' @export
#'
summary.freqlist <- function(object, single = FALSE, labelTranslations = NULL, dupLabels = FALSE, title = NULL, ...)
{
  if(!is.logical(single) || length(single) != 1) stop("'single' must be TRUE or FALSE")
  if(!is.null(labelTranslations)) labels(object) <- labelTranslations
  if(!is.logical(dupLabels) || length(dupLabels) != 1) stop("'dupLabels' must be TRUE or FALSE")

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

  if(is.null(object$labels))
  {
    cnames <- names(object$freqlist)
  } else
  {
    cnames <- c(object$labels, "Freq", "cumFreq", "freqPercent","cumPercent")
  }
  if(is.null(object$byVar) || single)
  {
    freqdf <- object$freqlist
    if(ncol(freqdf) > 5 && !dupLabels)
    {
      freqdf[, 1:(ncol(freqdf)-4)] <- fmtdups(freqdf[, 1:(ncol(freqdf)-4), drop = FALSE])
    }
    if(!is.null(title)) cat("\nTable: ", title, sep = "")
    print(knitr::kable(freqdf, row.names = FALSE, col.names = cnames, caption = NULL, ...))
  } else
  {
    byVar <- object$byVar
    freqdf <- object$freqlist
    for(i in match(byVar, names(freqdf)))
    {
      if(sum(is.na(freqdf[[i]])) > 0) {freqdf[[i]] <- addNA(freqdf[[i]])}
    }
    printlist <- by(freqdf, freqdf[, rev(byVar)], FUN = data.frame)
    names(printlist) <- gsub("[.]",", ", levels(interaction(rev(freqdf[, byVar, drop = FALSE]))))
    for(i in 1:length(printlist))
    {
      if(i == 1L && !is.null(title)) cat("\nTable: ", title, sep = "")
      if(!is.null(printlist[[i]]))
      {
        if(nrow(printlist[[i]]) > 1)
        {
          sublist <- printlist[[i]]
          if(!dupLabels) sublist[, 1:(ncol(sublist)-4)] <- fmtdups(sublist[, 1:(ncol(sublist)-4), drop = FALSE])
          print(knitr::kable(sublist, row.names = FALSE, col.names = cnames, caption = NULL, ...))
        } else
        {
          print(knitr::kable(printlist[[i]], row.names = FALSE, col.names = cnames, caption = NULL, ...))
        }
      }
    }
  }
  invisible(object)
}
