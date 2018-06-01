
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
# 'in.both' subsets to the people in both

#' @rdname paired.internal
#' @export
na.paired <- function(missings = c("in.both", "fill", "asis"))
{
  missings <- match.arg(missings)
  switch(
    missings,
    in.both = function(object, ...)
    {
      omit <- is.na(object[[1]]) | is.na(object[["(id)"]])
      xx <- object[!omit, , drop = FALSE]

      by.col <- xx[[1]]
      if(is.factor(by.col)) {
        by.col <- droplevels(by.col)
        by.levels <- levels(by.col)
      } else by.levels <- sort(unique(by.col))

      ids <- xx[["(id)"]]
      ids.both <- intersect(ids[by.col == by.levels[1]], ids[by.col == by.levels[2]])
      xx[ids %in% ids.both, , drop = FALSE]
    },
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
