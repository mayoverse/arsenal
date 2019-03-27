
#' Control settings for \code{paired} function
#'
#' Control test and summary settings for the \code{\link{paired}} function.
#'
#' @param diff logical, telling \code{paired} whether to calculate a column of differences between time points.
#' @param numeric.test name of test for numeric RHS variables in \code{paired}: paired.t, signed.rank, sign.test.
#' @param cat.test name of test for categorical variables: mcnemar
#' @param ordered.test name of test for ordered variables: signed.rank, sign.test
#' @param date.test name of test to perform for date variables: paired.t, signed.rank, sign.test
#' @param mcnemar.correct,signed.rank.exact,signed.rank.correct Options for statistical tests. See \code{\link{wilcox.test}}
#'   and \code{\link{mcnemar.test}} for details.
#' @param ... Arguments passed to \code{\link{tableby.control}}
#' @details Note that (with the exception of \code{total}) all arguments to \code{\link{tableby.control}} are accepted in
#'   this function (in fact, this function passes everything through to \code{\link{tableby.control}}).
#'   However, there are different defaults for the statistical tests (shown here). For details on the other arguments,
#'   please see the help page for \code{\link{tableby.control}}.
#' @return A list with settings to be used within the \code{\link{paired}} function.
#' @seealso \code{\link{paired}}, \code{\link{tableby}}, \code{\link{tableby.control}}, \code{\link{summary.tableby}}
#' @author Ethan Heinzen
#' @export
paired.control <- function(
  diff=TRUE, numeric.test="paired.t", cat.test="mcnemar", ordered.test="signed.rank", date.test="paired.t",
  mcnemar.correct = TRUE, signed.rank.exact = NULL, signed.rank.correct = TRUE, ...) {

  out <- tableby.control(numeric.test=numeric.test, cat.test=cat.test, ordered.test=ordered.test, date.test=date.test, ...)

  ## new args
  out$diff <- diff
  out$mcnemar.correct <- mcnemar.correct
  out$signed.rank.correct <- signed.rank.correct
  out$signed.rank.exact <- signed.rank.exact

  ## never show total
  out$total <- FALSE
  out
}
