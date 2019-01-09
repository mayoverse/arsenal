
as_data_frame_freqlist <- function(yList, single, sparse)
{
  filter_zero <- function(x) if(!sparse) droplevels(x[x$Freq != 0, , drop = FALSE]) else x
  labs <- vapply(yList$x, "[[", NA_character_, "label")
  if(single || !yList$strata$hasStrata)
  {
    list(set_attr(do.call(rbind_chr, lapply(yList$tables, filter_zero)), "labels", labs))
  } else
  {
    lapply(lapply(yList$tables, filter_zero), set_attr, "labels", labs)
  }
}

#' as.data.frame.freqlist
#'
#' Convert \code{\link{freqlist}} object to a data.frame.
#'
#' @param x An object of class \code{"freqlist"}.
#' @inheritParams summary.freqlist
#' @param ... Arguments to pass to \code{\link{freq.control}}
#' @return A data.frame corresponding to the \code{freqlist} object.
#' @export
as.data.frame.freqlist <- function(x, ..., labelTranslations = NULL, list.ok = FALSE)
{
  if(!is.null(labelTranslations)) labels(x) <- labelTranslations

  control <- c(list(...), x$control)
  control <- do.call("freq.control", control[!duplicated(names(control))])

  out <- lapply(x$tables, as_data_frame_freqlist, single = control$single, sparse = control$sparse)
  out <- unlist(out, recursive = FALSE, use.names = FALSE)
  if(!list.ok)
  {
    if(length(out) == 1) out <- out[[1]] else warning("as.data.frame.freqlist is returning a list of data.frames")
  }
  set_attr(out, "control", control)
}
