
as_data_frame_freqlist <- function(yList, single)
{
  labs <- vapply(yList$x, "[[", NA_character_, "label")
  if(single || !yList$strata$hasStrata)
  {
    list(set_attr(do.call(rbind_chr, yList$tables), "labels", labs))
  } else
  {
    lapply(yList$tables, set_attr, "labels", labs)
  }
}

#' as.data.frame.freqlist
#'
#' Convert \code{\link{freqlist}} object to a data.frame.
#'
#' @param x An object of class \code{"freqlist"}.
#' @inheritParams summary.freqlist
#' @return A data.frame corresponding to the \code{freqlist} object.
#' @export
as.data.frame.freqlist <- function(x, ..., single = FALSE, labelTranslations = NULL, list.ok = FALSE)
{
  if(!is.null(labelTranslations)) labels(x) <- labelTranslations
  out <- lapply(x$tables, as_data_frame_freqlist, single = single)
  out <- unlist(out, recursive = FALSE, use.names = FALSE)
  if(!list.ok)
  {
    if(length(out) == 1) out <- out[[1]] else warning("as.data.frame.freqlist is returning a list of data.frames")
  }
  out
}
