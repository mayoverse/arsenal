get_tb_strata_part <- function(tbList, sValue, xList, ...)
{
  Map(get_tb_part, tbList, xList, MoreArgs = list(sValue = sValue, ...))
}


get_tb_part <- function(tbList, xList, yList, sList, sValue, statLabs)
{
  f <- function(x, nm, lab = FALSE)
  {
    if(inherits(x[[1]], "tbstat_multirow")) return(if(lab) names(x[[1]]) else rep(nm, length(x[[1]])))
    if(lab && nm %in% names(statLabs)) statLabs[[nm]] else nm
  }

  out <- data.frame(
    group.term = yList$term,
    group.label = yList$label,
    strata.term = if(sList$term == "") "" else paste0("(", sList$term, ") == ", sValue),
    strata.label = if(sList$term == "") "" else sValue,
    variable = xList$variable,
    term = c(xList$term, unlist(Map(f, tbList$stats, names(tbList$stats)), use.names = FALSE)),
    label = c(xList$label, unlist(Map(f, tbList$stats, names(tbList$stats), lab = TRUE), use.names = FALSE)),
    variable.type = tbList$type,
    stringsAsFactors = FALSE
  )
  if(sList$term == "") out$strata.label <- NULL else names(out)[4] <- sList$label

  f2 <- function(x, lv)
  {
    if(inherits(x[[1]], "tbstat_multirow")) x[[lv]] else x[lv]
  }
  for(lvl in names(yList$stats))
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
as.data.frame.tableby <- function(x, which = seq_along(x$tables), ..., labelTranslations = NULL, list.ok = FALSE)
{
  if(!is.null(labelTranslations)) labels(x) <- labelTranslations

  control <- c(list(...), x$control)
  control <- do.call("tableby.control", control[!duplicated(names(control))])

  if(length(which) > 1 || list.ok)
  {
    if(!list.ok) warning("as.data.frame.tableby is returning a list of data.frames")
    out <- lapply(which, as_data_frame_tableby, x = x, control = control)
  } else out <- as_data_frame_tableby(x, which, control = control)

  set_attr(out, "control", control)
}


as_data_frame_tableby <- function(x, which, control)
{
  strataList <- x$tables[[which]]
  stopifnot(length(strataList$tables) == length(strataList$strata$values))
  tabs <- Map(get_tb_strata_part, tbList = strataList$tables, sValue = strataList$strata$values,
              MoreArgs = list(yList = strataList$y, sList = strataList$strata, xList = strataList$x, statLabs = control$stats.labels))
  out <- do.call(rbind_chr, unlist(tabs, recursive = FALSE, use.names = FALSE))

  f <- function(elt, whch = "cat.simplify") if(is.null(elt[[whch]])) control[[whch]] else elt[[whch]]
  simp.cat <- vapply(strataList$control.list, f, NA)
  simp.num <- vapply(strataList$control.list, f, NA, "numeric.simplify")

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
    bylst <- list(factor(out$variable, levels = unique(out$variable)))
    if(x$hasStrata) bylst[[2]] <- factor(out[[4]], levels = unique(out[[4]]))
    out <- do.call(rbind_chr, c(by(out, bylst, simplify, simplify = FALSE), make.row.names = FALSE))
  }
  set_attr(out, "control.list", strataList$control.list)
}
