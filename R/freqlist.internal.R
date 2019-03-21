internalTable <- function(data, na.options) {
  # orders and performs calculations for the table
  # split into a function to be able to use with by statement
  data <- data[do.call(order, unname(data)), ]
  na.index <- rowSums(is.na(data))
  if (na.options == 'remove') {
    data  <- data[na.index == 0, ]
    cumFreq <- cumsum(data$Freq)
    freqPct <- if(sum(data$Freq) > 0) 100 * data$Freq / sum(data$Freq) else NA_real_
    cumPct <- cumsum(freqPct)
  } else if(na.options == 'include') {
    cumFreq <- cumsum(data$Freq)
    freqPct <- if(sum(data$Freq) > 0) 100 * data$Freq / sum(data$Freq) else NA_real_
    cumPct <- cumsum(freqPct)
  } else if(na.options == 'showexclude') {
    freq_tmp <- data$Freq
    freq_tmp[na.index != 0] <- NA
    cumFreq <- cumfun(freq_tmp)
    denom <- max(stats::na.omit(cumFreq), na.rm = TRUE)
    freqPct <- if(denom > 0) 100 * freq_tmp / denom else NA_real_
    cumPct <- cumfun(freqPct)
  }
  data$cumFreq <- cumFreq
  data$freqPercent <- freqPct
  data$cumPercent <- cumPct
  row.names(data) <- NULL
  data
}

cumfun <- function(x) {
  # function to create a cumulative sum retaining NAs, but omitting in sum function
  x2 <- rep(NA, length(x))
  x.om <- stats::na.omit(x)
  if (length(x.om) == 0) {
    warning("For at least one level, all entries have NAs")
  } else {
    x2[!is.na(x)] <- cumsum(x.om)
  }
  x2
}

add_freqlist_xterms <- function(xTerms)
{
  xTerms$Freq <- list(variable="Freq", label="Freq", term="Freq")
  xTerms$cumFreq <- list(variable="cumFreq", label="Cumulative Freq", term="cumFreq")
  xTerms$freqPercent <- list(variable="freqPercent", label="Percent", term="freqPercent")
  xTerms$cumPercent <- list(variable="cumPercent", label="Cumulative Percent", term="cumPercent")
  xTerms
}

#' Helper functions for freqlist
#'
#' A set of helper functions for \code{\link{freqlist}}.
#'
#' @param x A \code{freqlist} object.
#' @inheritParams tableby.internal
#' @seealso \code{\link{merge.freqlist}}, \code{\link{arsenal_table}}
#' @name freqlist.internal
NULL
#> NULL

#' @rdname freqlist.internal
#' @export
is.freqlist <- function(x) inherits(x, "freqlist")

#' @rdname freqlist.internal
#' @export
is.summary.freqlist <- function(x) inherits(x, "summary.freqlist")

#' @rdname freqlist.internal
#' @export
head.summary.freqlist <- function(x, n = 6L, ...)
{
  x$object <- lapply(x$object, utils::head, n = n, ...)
  x
}

#' @rdname freqlist.internal
#' @export
tail.summary.freqlist <- function(x, n = 6L, ...)
{
  x$object <- lapply(x$object, utils::tail, n = n, ...)
  x
}

