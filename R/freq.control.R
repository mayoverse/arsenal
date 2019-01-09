
#' Control settings for \code{freqlist} function
#'
#' Control test and summary settings for the \code{\link{freqlist}} function.
#'
#' @param sparse a logical value indicating whether to keep rows with counts of zero.
#'   The default is \code{FALSE} (drop zero-count rows).
#' @param single logical, indicating whether to collapse results created using a strata variable into a single table for printing
#' @param dupLabels logical: should labels which are the same as the row above be printed? The default (\code{FALSE}) more
#'   closely approximates \code{PROC FREQ} output from SAS, where a label carried down from the row above is left blank.
#' @param digits.count Number of decimal places for count values.
#' @param digits.pct Number of decimal places for percents.
#' @param ... additional arguments.
#' @return A list with settings to be used within the \code{freqlist} function.
#'
#' @seealso \code{\link{freqlist}}, \code{\link{summary.freqlist}}
#' @author Ethan Heinzen
#' @export
freq.control <- function(sparse = FALSE, single = FALSE, dupLabels = FALSE, digits.count = 0L, digits.pct = 2L, ...)
{

  nm <- names(dots <- list(...))
  if("digits" %in% nm)
  {
    .Deprecated(msg = "Using 'digits = ' is deprecated. Use 'digits.pct = ' instead.")
    digits.pct <- dots$digits
  }

  # digits are OK to be NULL. See ?format
  if(!is.null(digits.count) && digits.count < 0L)
  {
    warning("digits.count must be >= 0. Set to default.")
    digits.count <- 0L
  }
  if(!is.null(digits.pct) && digits.pct < 0L)
  {
    warning("digits.pct must be >= 0. Set to default.")
    digits.pct <- 1L
  }

  list(sparse = sparse, single = single, dupLabels = dupLabels, digits.count = digits.count, digits.pct = digits.pct)
}
