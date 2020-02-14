
#' Helper functions for paired
#'
#' A set of helper functions for \code{\link{paired}}.
#'
#' @param missings A character string denoting which action to take. See "Details", below.
#' @return \code{na.paired} returns a function used to subset data.frames in \code{\link{paired}}.
#' @details
#'   All methods subset out any NA time points or IDs.
#'   \code{"in.both"} (the default) subsets the data.frame to individuals who appear at both time points.
#'   \code{"fill"} adds explicit missings for the people missing second time points.
#'   \code{"asis"} does nothing to add or remove missings.
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
      obj.id <- object[["(id)"]]
      omit <- is.na(object[[1]]) | is.na(obj.id)
      if("(strata)" %in% names(object)) omit <- omit | is.na(object[["(strata)"]])

      by.col <- object[[1]][!omit]
      if(is.factor(by.col)) {
        by.col <- droplevels(by.col)
        by.levels <- levels(by.col)
      } else by.levels <- sort(unique(by.col))
      if(length(by.levels) != 2) stop("Please specify exactly 2 time points")

      ids <- object[["(id)"]][!omit]
      omit <- omit | (obj.id %nin% intersect(ids[by.col == by.levels[1]], ids[by.col == by.levels[2]]))
      xx <- object[!omit, , drop = FALSE]

      if(any(omit)) {
        temp <- stats::setNames(seq_along(omit)[omit], attr(object, "row.names")[omit])
        attr(temp, "class") <- "omit"
        attr(xx, "na.action") <- temp
      }
      xx
    },
    fill = function(object, ...)
    {
      omit <- is.na(object[[1]]) | is.na(object[["(id)"]])
      if("(strata)" %in% names(object)) omit <- omit | is.na(object[["(strata)"]])

      xx <- object[!omit, , drop = FALSE]

      all.pairs <- expand.grid(times = unique(xx[[1]]), id = unique(xx[["(id)"]]),
                               stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
      xx <- merge(xx, all.pairs, by.x = c(colnames(xx)[1], "(id)"), by.y = c("times", "id"),
            all = TRUE, sort = FALSE)[names(xx)]
      if(any(omit)) {
        temp <- stats::setNames(seq_along(omit)[omit], attr(object, "row.names")[omit])
        attr(temp, "class") <- "omit"
        attr(xx, "na.action") <- temp
      }
      xx
    },
    asis = function(object, ...)
    {
      # take away na's in tp and id
      omit <- is.na(object[[1]]) | is.na(object[["(id)"]])
      if("(strata)" %in% names(object)) omit <- omit | is.na(object[["(strata)"]])

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
