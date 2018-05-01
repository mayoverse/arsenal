
#' Control settings for \code{paired} function
#'
#' Control test and summary settings for the \code{\link{paired}} function.
#'
#' @param test logical, telling \code{paired} whether to perform tests of x variables across time points.
#' @param diff logical, telling \code{paired} whether to calculate a column of differences between time points.
#' @param test.pname character string denoting the p-value column name in \code{\link{summary.tableby}}.
#'   Modifiable also with \code{\link{modpval.tableby}}.
#' @param numeric.test set test for numeric RHS variables in \code{tableby} to anova or kwt (Kruskal-Wallis) rank-based tests.
#'   If no LHS variable exists, then a mean is required for a univariate test.
#' @param numeric.stats summary statistics to include for numeric RHS variables of \code{tableby} within the levels of the group LHS variable.
#'   Options are N, Nmiss, Nmiss2, mean, meansd, median, q1q3, medianq1q3, iqr, range, medianrange, or other R built-in or user-written functions.
#' @param cat.test  name of test for categorical variables: chisq, fe (Fisher's Exact)
#' @param cat.stats summary statistics to include for categorical RHS variables of \code{tableby} within the levels of the group LHS variable.
#'   Options are N, Nmiss, Nmiss2, count, countpct, countrowpct, or other R built-in or user-written functions.
#' @param ordered.test name of test for ordered variables: trend
#' @param ordered.stats summary statistics to include for categorical RHS variables of \code{tableby} within the levels of the group LHS variable.
#'   Options are N, Nmiss, count, countpct, or other R built-in or user-written functions.
#' @param date.test name of test to perform for date variables: kwt
#' @param date.stats stats functions to perform for Date variables: Nmiss, median, range, medianrange, q1q3, medianq1q3,
#'   or other R built-in or user-written functions.
#' @param mcnemar.correct,signed.rank.exact,signed.rank.correct Options for statistical tests. See \code{\link{wilcox.test}}
#'   and \code{\link{mcnemar.test}} for details.
#' @inheritParams tableby.control
#' @param ... additional arguments.
#' @inherit tableby.control details
#' @return A list with settings to be used within the \code{paired} function.
#' @seealso \code{\link{paired}}, \code{\link{tableby}}, \code{\link{summary.tableby}}
#' @author Ethan Heinzen
#' @export
paired.control <- function(test=TRUE, diff=TRUE, test.pname=NULL,
   numeric.test="paired.t", cat.test="mcnemar", ordered.test="signed.rank", date.test="paired.t",
   numeric.stats=c("Nmiss", "meansd", "range"), cat.stats = c("Nmiss", "countpct"),
   ordered.stats=c("Nmiss", "countpct"), date.stats=c("Nmiss", "median","range"),
   stats.labels=list(),
   digits = 3L, digits.count = 0L, digits.p = 3L, format.p = TRUE,
   mcnemar.correct = TRUE, signed.rank.exact = NULL, signed.rank.correct = TRUE, ...) {

  ## can't do match.call()[[1]] <- "tableby.control", which would ignore new defaults
  out <- tableby.control(
    test=test, diff=diff, test.pname=test.pname, total = FALSE, cat.simplify = FALSE,
    numeric.test=numeric.test, cat.test=cat.test, ordered.test=ordered.test, date.test=date.test,
    numeric.stats=numeric.stats, cat.stats=cat.stats, ordered.stats=ordered.stats, date.stats=date.stats,
    stats.labels=stats.labels, digits=digits, digits.p=digits.p, digits.count = digits.count, format.p = format.p,
    ...
  )
  out$diff <- diff
  out$mcnemar.correct <- mcnemar.correct
  out$signed.rank.correct <- signed.rank.correct
  out$signed.rank.exact <- signed.rank.exact
  out
}
