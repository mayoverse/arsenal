
#' Helper functions for paired
#'
#' A set of helper functions for \code{\link{paired}}.
#'
#' @param object A \code{data.frame} resulting from evaluating a \code{paired} formula.
#' @param ... Other arguments, or a vector of indices for extracting.
#' @return \code{na.paired} returns a subsetted version of \code{object} (with attributes).
#' @seealso \link{tableby.internal}
#' @name paired.internal
NULL
#> NULL


# 'fill' puts in the missing time points
# 'asis' doesn't do anything

#' @rdname paired.internal
#' @export
na.paired <- function(missings = c("fill", "asis"))
{
  missings <- match.arg(missings)
  switch(
    missings,
    fill = function(object, ...)
    {
      omit <- is.na(object[[1]]) | is.na(object[["(id)"]])
      xx <- object[!omit, , drop = FALSE]

      all.pairs <- expand.grid(times = unique(xx[[1]]), id = unique(xx[["(id)"]]),
                               stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
      merge(xx, all.pairs, by.x = c(colnames(xx)[1], "(id)"), by.y = c("times", "id"),
            all = TRUE, sort = FALSE)[names(xx)]
    },
    asis = function(object, ...)
    {
      # take away na's in tp and id
      omit <- is.na(object[[1]]) | is.na(object[["(id)"]])
      xx <- object[!omit, , drop = FALSE]
      if(any(omit)) {
        temp <- stats::setNames(seq_along(omit)[omit], attr(object, "row.names")[omit])
        attr(temp, "class") <- "omit"
        attr(xx, "na.action") <- temp
      }
      xx
    }
  )
}
