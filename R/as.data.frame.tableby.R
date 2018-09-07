
get_tb_part <- function(tbList, byLvls, statLabs)
{
  f <- function(x, nm, lab = FALSE)
  {
    if(inherits(x[[1]], "tbstat_multirow")) return(if(lab) names(x[[1]]) else rep(nm, length(x[[1]])))
    if(lab && nm %in% names(statLabs)) statLabs[[nm]] else nm
  }

  out <- data.frame(
    variable = tbList$variable,
    term = c(tbList$term, unlist(Map(f, tbList$stats, names(tbList$stats)), use.names = FALSE)),
    label = c(tbList$label, unlist(Map(f, tbList$stats, names(tbList$stats), lab = TRUE), use.names = FALSE)),
    variable.type = tbList$type
  )

  f2 <- function(x, lv)
  {
    if(inherits(x[[1]], "tbstat_multirow")) x[[lv]] else x[lv]
  }
  for(lvl in byLvls)
  {
    out[[lvl]] <- c("", unlist(lapply(tbList$stats, f2, lv = lvl), recursive = FALSE, use.names = FALSE))
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

  out <- do.call(rbind, c(lapply(x$x, get_tb_part, byLvls = names(x$y[[1]]$stats), statLabs = control$stats.labels), make.row.names = FALSE))

  f <- function(elt, whch = "cat.simplify") if(is.null(elt$control.list[[whch]])) control[[whch]] else elt$control.list[[whch]]
  simp.cat <- vapply(x$x, f, NA)
  simp.num <- vapply(x$x, f, NA, "numeric.simplify")

  if(any(simp.cat) || any(simp.num))
  {
    simplify <- function(x)
    {
      if(simp.cat[x$variable[1]] && nrow(x) == 3 &&
         all(x$term[2:3] %in% c("count", "countpct", "countcellpct", "countrowpct", "binomCI", "rowbinomCI")))
      {
        y <- x[3, , drop = FALSE]
        y$term[1] <- x$term[1]
        y$label[1] <- x$label[1]
      } else if(simp.num[x$variable[1]] && nrow(x) == 2)
      {
        y <- x[2, , drop = FALSE]
        y$term[1] <- x$term[1]
        y$label[1] <- x$label[1]
      } else y <- x
      y
    }
    out <- do.call(rbind, c(by(out, factor(out$variable, levels = unique(out$variable)),
                               simplify, simplify = FALSE), make.row.names = FALSE))
  }
  idx <- vapply(out, is.factor, NA)
  if(any(idx)) out[idx] <- lapply(out[idx], as.character) ## this is for R 3.2.3, whose rbind() doesn't have 'stringsAsFactors='

  set_attr(set_attr(out, "control", control), "control.list", lapply(x$x, function(x) x$control.list))
}
