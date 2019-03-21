internalTable <- function(data, na.options, keep_cols = c("cumFreq", "freqPercent", "cumPercent"), sort = FALSE, decreasing = FALSE) {
  if("Freq" %nin% names(data)) stop("You tried to create or sort a freqlist table with no 'Freq' column!")

  data <- if(!sort)
  {
    data[do.call(order, unname(data)), ]
  } else data[order(data$Freq, decreasing = decreasing), ]

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

  if("cumFreq" %in% keep_cols) data$cumFreq <- cumFreq
  if("freqPercent" %in% keep_cols) data$freqPercent <- freqPct
  if("cumPercent" %in% keep_cols) data$cumPercent <- cumPct
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
#' @param decreasing Should the sort be increasing or decreasing?
#' @seealso \code{\link{merge.freqlist}}, \code{\link{arsenal_table}}, \code{\link{sort}},
#'   \code{\link{freqlist}}, \code{\link{summary.freqlist}}, \code{\link{freq.control}},
#' @details
#' Note that \code{sort()} has to recalculate cumulative statistics. Note also that the reordering of rows
#'   will also affect which labels are duplicates; you may also want to consider using
#'   \code{dupLabels=TRUE} in \code{\link{freq.control}()}.
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

#' @rdname freqlist.internal
#' @export
sort.freqlist <- function(x, decreasing = FALSE, ...)
{
  for(i in seq_along(x$tables))
  {
    na.opts <- x$tables[[i]]$na.options
    keepcols <- names(x$tables[[i]]$x) # in case they subsetted away some of the columns

    x$tables[[i]]$tables[] <- lapply(x$tables[[i]]$tables, internalTable, na.options = na.opts,
                                     sort = TRUE, decreasing = decreasing, keep_cols = keepcols)
  }
  x
}
