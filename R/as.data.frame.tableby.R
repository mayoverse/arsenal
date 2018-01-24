
get_tb_part <- function(tbList, byLvls, statLabs)
{
  if(tbList$type %in% c("ordinal", "categorical"))
  {
    lvls <- names(tbList$stats[[1]][[1]])
    out <- data.frame(
      variable = tbList$name,
      term = c(tbList$name, rep(lvls, times = length(tbList$stats))),
      label = c(tbList$label, rep(lvls, times = length(tbList$stats))),
      variable.type = tbList$type
    )
    for(lvl in byLvls)
    {
      out[[lvl]] <- c("", unlist(lapply(tbList$stats, function(x) x[[lvl]]), recursive = FALSE, use.names = FALSE))
    }
  } else if(tbList$type %in% c("Date", "numeric", "survival"))
  {
    out <- data.frame(
      variable = tbList$name,
      term = c(tbList$name, names(tbList$stats)),
      label = c(tbList$label, vapply(names(tbList$stats), function(x) if(is.null(statLabs[[x]])) x else statLabs[[x]], NA_character_)),
      variable.type = tbList$type
    )
    for(lvl in byLvls)
    {
      out[[lvl]] <- c("", lapply(tbList$stats, function(x) x[[lvl]]))
    }
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

  out <- do.call(rbind, lapply(x$x, get_tb_part, byLvls = names(x$y$Group$stats), statLabs = x$control$stats.labels))
  idx <- colnames(out) %nin% c("variable", "term", "label", "variable.type", "test", "p.value")
  out[idx] <- lapply(out[idx], as.tbstatcol)

  # Get rid of Nmiss if none missing
  # if("Nmiss" %in% colnames(out) && all(out$Nmiss == 0)) out$Nmiss <- NULL

  row.names(out) <- NULL

  set_attr(out, "control", control)
}
