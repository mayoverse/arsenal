
get_tb_part <- function(tbList, byLvls, statLabs)
{
  f <- function(x, nm, lab = FALSE)
  {
    if(inherits(x[[1]], "tbstat_multirow")) names(x[[1]]) else if(lab && !is.null(statLabs[[nm]])) statLabs[[nm]] else nm
  }

  out <- data.frame(
    variable = tbList$name,
    term = c(tbList$name, unlist(Map(f, tbList$stats, names(tbList$stats)), use.names = FALSE)),
    label = c(tbList$label, unlist(Map(f, tbList$stats, names(tbList$stats), lab = TRUE), use.names = FALSE)),
    variable.type = tbList$type
  )

  f <- function(x, lv)
  {
    if(inherits(x[[1]], "tbstat_multirow")) x[[lv]] else x[lv]
  }
  for(lvl in byLvls)
  {
    out[[lvl]] <- c("", unlist(lapply(tbList$stats, f, lv = lvl), recursive = FALSE, use.names = FALSE))
  }
  out$test <- tbList$test$method
  out$p.value <- tbList$test$p.value
  out
}

#' as.data.frame.tableby
#'
#' Coerce a \code{\link{tableby}} object to a \code{data.frame}.
#'
#' @param x A \code{\link{tableby}} object.
#' @param ... Arguments to pass to \code{\link{tableby}}.
#' @inheritParams summary.tableby
#' @seealso \code{\link{tableby}}, \code{\link{tableby}}
#' @return A \code{data.frame}.
#' @author Ethan Heinzen, based on code originally by Greg Dougherty
#' @export
as.data.frame.tableby <- function(x, ..., labelTranslations = NULL)
{
  if(!is.null(labelTranslations)) labels(x) <- labelTranslations

  control <- c(list(...), x$control)
  control <- do.call("tableby.control", control[!duplicated(names(control))])

  out <- do.call(rbind, c(lapply(x$x, get_tb_part, byLvls = names(x$y[[1]]$stats), statLabs = x$control$stats.labels), stringsAsFactors = FALSE))
  if(control$cat.simplify)
  {
    cat_simplify <- function(x)
    {
      if(nrow(x) != 3 || x$variable.type[1] %nin% c("categorical", "ordinal")) return(x)
      y <- x[3, , drop = FALSE]
      y$term[1] <- x$term[1]
      y$label[1] <- x$label[1]
      y
    }
    out <- do.call(rbind, c(by(out, out$variable, cat_simplify, simplify = FALSE), stringsAsFactors = FALSE))
  }

  row.names(out) <- NULL

  set_attr(out, "control", control)
}
