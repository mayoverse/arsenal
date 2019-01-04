
locate <- function(string, pattern) stringr::str_locate_all(string, pattern)[[1L]][, 1L]

smartsplit <- function(string, width, min.split)
{
  if(width < min.split) stop("Desired width < min.split?")
  if(nchar(string) <= width) return(string)

  pos <- locate(string, "[ \t\n_.;:,-]")
  splt <- if(length(pos) == 0 || !any(idx <- pos <= width & pos >= min.split)) width else max(pos[idx])

  c(stringr::str_sub(string, 1L, splt), smartsplit(stringr::str_sub(string, splt+1L), width = width, min.split = min.split))
}

#' Split a string into pieces intelligently
#'
#' @param string A character vector
#' @param width Either \code{Inf} or \code{NULL} to specify no splitting,
#'   or a positive integer giving the largest allowed string length.
#' @param min.split Either \code{-Inf} or \code{NULL} to specify no
#'   lower bound on the string length, or a positive integer giving the minimum string length.
#' @inheritParams base::replace
#' @return For \code{smart.split}, a list of the same length as \code{string}, with each element being
#'   the "intelligently" split string.
#'
#'   For \code{replace2}, a vector with the proper values replaced.
#' @seealso \code{\link[base]{replace}}
#' @name internal.functions
NULL
#> NULL

#' @rdname internal.functions
#' @export
smart.split <- function(string, width = Inf, min.split = -Inf)
{
  if(is.null(width)) width <- Inf
  if(is.null(min.split)) min.split <- -Inf
  lapply(string, smartsplit, width = width, min.split = min.split)
}


insert_elt <- function(col, times, elt = "")
{
  f <- if(is.null(elt)) rep else function(x, i) c(x, rep(elt, times = i - 1L))
  unlist(Map(f, col, times), use.names = FALSE)
}

#' @rdname internal.functions
#' @export
replace2 <- function(x, list, values)
{
  x[[list]] <- values
  x
}

as_list_formula <- function(formula)
{
  if(is.list(formula)) return(formula)
  if(length(formula) == 2 || is.name(formula[[2]]) || !identical(formula[[2]][[1]], as.name("list")))
    return(list(formula)) # one-sided or LHS is single arg
  lapply(formula[[2]][-1], replace2, list = 2, x = formula)
}

# set all factors to characters
rbind_chr <- function(...)
{
  out <- rbind(..., make.row.names = FALSE)
  if(is.data.frame(out))
  {
    idx <- vapply(out, is.factor, NA)
    out[idx] <- lapply(out[idx], as.character)
  }
  out
}

na_lhs_strata <- function(object, ...) {
  omit <- is.na(object[[1]])
  if("(strata)" %in% names(object)) omit <- omit | is.na(object[["(strata)"]])

  xx <- object[!omit, , drop = FALSE]
  if(any(omit)) {
    temp <- stats::setNames(seq_along(omit)[omit], attr(object, "row.names")[omit])
    attr(temp, "class") <- "omit"
    attr(xx, "na.action") <- temp
  }
  xx
}

subset_lhs_strata <- function(x, i, j, ..., warn = TRUE) {
  if(missing(i) && missing(j)) return(x)
  newx <- x

  give_warn <- function(vec) if(warn) warning(paste0("Some indices not found in object: ", paste0(vec, collapse = ", ")), call. = FALSE)
  if(!missing(j))
  {
    if(is.character(j) && any(tmp <- j %nin% names(newx$tables)))
    {
      give_warn(j[tmp])
      j <- j[!tmp]
    } else if(is.numeric(j) && any(tmp <- j %nin% seq_along(newx$tables)))
    {
      give_warn(j[tmp])
      j <- j[!tmp]
    } else if(is.logical(j) && length(j) != length(newx$tables))
    {
      stop("Logical vector index not the right length")
    }
    if(length(j) == 0 || anyNA(j)) stop("Indices must have nonzero length and no NAs.")
    newx$tables <- newx$tables[j]
  }

  if(!missing(i))
  {
    newx$tables <- lapply(newx$tables, function(yList) {
      if(is.character(i) && any(tmp <- i %nin% names(yList$x)))
      {
        give_warn(i[tmp])
        i <- i[!tmp]
      } else if(is.numeric(i) && any(tmp <- i %nin% seq_along(yList$x)))
      {
        give_warn(i[tmp])
        i <- i[!tmp]
      } else if(is.logical(i) && length(i) != length(yList$x))
      {
        stop("Logical vector index not the right length")
      }
      if(length(i) == 0 || anyNA(i)) stop("Indices must have nonzero length and no NAs.")
      yList$x <- yList$x[i]
      yList$tables <- lapply(yList$tables, "[", i)
      yList
    })
  }
  newx
}

labels_lhs_strata <- function(object, ...) {

  get_lab <- function(x)
  {
    xLabs <- vapply(x$x, "[[", NA_character_, "label")
    aLabs <- if(is.null(x$adjust)) NULL else vapply(x$adjust, "[[", NA_character_, "label")
    c(stats::setNames(x$y$label, x$y$term), xLabs, aLabs)
  }

  labs <- unlist(unname(lapply(object$tables, get_lab)), recursive = FALSE)
  labs[!duplicated(labs) | !duplicated(names(labs))]
}


make_ms_labs <- function(x)
{
  x$label <- if(identical(x$term, x$variable2)) x$varlabel else sub(x$variable2, paste0(x$varlabel, " "), x$term, fixed = TRUE)
  x
}

labels_assign_lhs_strata <- function(x, value) {
  ## if the value vector is named, then assign the labels to
  ## those names that match those in x and y
  if(is.list(value)) value <- unlist(value)
  if(is.null(value))
  {
    for(i in seq_along(x$tables))
    {
      mk_lab <- function(elt)
      {
        if(is.null(elt$variable2) || is.null(elt$varlabel))
        { # for tableby
          elt$label <- elt$term
        } else
        {
          elt$varlabel <- elt$variable
          elt <- make_ms_labs(elt)
        }
        elt
      }
      for(j in seq_along(x$tables[[i]]$x)) x$tables[[i]]$x[[j]] <- mk_lab(x$tables[[i]]$x[[j]])
      for(j in seq_along(x$tables[[i]]$adjust)) x$tables[[i]]$adjust[[j]] <- mk_lab(x$tables[[i]]$adjust[[j]])
      x$tables[[i]]$y$label <- x$tables[[i]]$y$term
      x$tables[[i]]$strata$label <- x$tables[[i]]$strata$term

    }
  } else if(!is.null(names(value))) {
    for(L in seq_along(value))
    {
      for(i in seq_along(x$tables))
      {
        nm <- names(value)[L]
        val <- unname(value[L])
        for(j in seq_along(x$tables[[i]]$x))
        {
          if(nm %in% x$tables[[i]]$x[[j]]$term) x$tables[[i]]$x[[j]]$label[x$tables[[i]]$x[[j]]$term == nm] <- val
        }
        for(j in seq_along(x$tables[[i]]$adjust))
        {
          if(nm %in% x$tables[[i]]$adjust[[j]]$term) x$tables[[i]]$adjust[[j]]$label[x$tables[[i]]$adjust[[j]]$term == nm] <- val
        }

        if(nm == x$tables[[i]]$y$term) x$tables[[i]]$y$label <- val
        if(nm == x$tables[[i]]$strata$term) x$tables[[i]]$strata$label <- val
      }
    }
  } else stop("Unnamed label assignments are no longer supported")
  x
}

print_lhs_strata <- function(x, ...)
{
  cat(class(x)[1], "Object\n\nFunction Call:\n")
  print(x$Call)
  cat("\nVariable(s):\n")
  lapply(x$tables, function(tab) {
    cat(tab$y$term, ": ", paste0(unlist(lapply(tab$x, "[[", "term"), use.names = FALSE), collapse = ", "), "\n", sep = "")
  })
  invisible(x)
}

merge_lhs_strata <- function(x, y, all = FALSE, all.x = all, all.y = all, ...) {
  Call <- match.call()
  if(x$hasStrata != y$hasStrata) stop("One of x or y has a strata, but the other doesn't.")
  if(x$hasWeights != y$hasWeights) stop("One of x or y has weights, but the other doesn't.")
  nms.x <- names(x$tables)
  nms.y <- names(y$tables)
  nms <- if(all.x && all.y) union(nms.x, nms.y) else if(all.x) nms.x else if(all.y) nms.y else intersect(nms.x, nms.y)
  if(length(nms) == 0) stop("No terms in common.")
  x <- x[, nms, warn = FALSE]
  y <- y[, nms, warn = FALSE]
  nms.x <- names(x$tables)
  nms.y <- names(y$tables)

  for(ytrm in names(y$tables))
  {
    if(ytrm %nin% names(x$table))
    {
      x$tables[[ytrm]] <- y$tables[[ytrm]]
      next
    }
    if(!identical(x$tables[[ytrm]]$y, y$tables[[ytrm]]$y)) stop("By-variables not identical for term ", ytrm)
    if(!identical(x$tables[[ytrm]]$strata, y$tables[[ytrm]]$strata)) stop("Strata not identical for term ", ytrm)
    if(!identical(x$tables[[ytrm]]$adjust, y$tables[[ytrm]]$adjust)) stop("Adjust not identical for term ", ytrm)

    xtrms <- names(y$tables[[ytrm]]$x)
    x$tables[[ytrm]]$x[xtrms] <- y$tables[[ytrm]]$x
    for(j in seq_along(x$tables[[ytrm]]$tables))
    {
      x$tables[[ytrm]]$tables[[j]][xtrms] <- y$tables[[ytrm]]$tables[[j]]
    }
  }

  x$Call <- Call
  x
}

#' @rdname modelsum.internal
#' @export
"[.modelsum" <- subset_lhs_strata

## retrieve variable labels (y, x-vec) from tableby object
#' @rdname modelsum.internal
#' @export
labels.modelsum <- labels_lhs_strata

## assign labels to modelsum object
#' @rdname modelsum.internal
#' @export
'labels<-.modelsum' <- labels_assign_lhs_strata

#' @rdname modelsum
#' @export
print.modelsum <- print_lhs_strata

#' @rdname modelsum.internal
#' @export
merge.modelsum <- merge_lhs_strata


#' @rdname tableby.internal
#' @export
"[.tableby" <- subset_lhs_strata

#' @rdname tableby.internal
#' @export
labels.tableby <- labels_lhs_strata

#' @rdname tableby.internal
#' @export
'labels<-.tableby' <- labels_assign_lhs_strata

#' @rdname tableby
#' @export
print.tableby <- print_lhs_strata

#' @rdname tableby.internal
#' @export
merge.tableby <- merge_lhs_strata
