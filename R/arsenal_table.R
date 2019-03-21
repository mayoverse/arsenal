
#' \code{arsenal} tables with common structure
#'
#' @param x,y,object An object of class \code{"arsenal_table"}
#' @param i,j A vector to index \code{x} with: either names of variables, a numeric vector, or a logical vector of appropriate length.
#'   \code{i} indexes the x-variables, and \code{j} indexes the by-variables.
#' @param value A list of new labels.
#' @param all,all.x,all.y Logicals, denoting which terms to keep if not all are in common.
#' @param ... Other arguments (only used in \code{print.summary.arsenal_table})
#' @param format Passed to \code{\link[knitr]{kable}}: the format for the table. The default here is "markdown".
#'   To use the default in \code{kable}, pass \code{NULL}. If \code{x$text} specifies LaTeX or HTML formatting,
#'   that format is used in the table.
#' @param escape Passed to \code{\link[knitr]{kable}}: should special characters be escaped when printed?
#' @param width,min.split Passed to \code{\link{smart.split}} for formatting of the "term" column.
#' @seealso \code{\link{merge}}, \code{\link{labels}}
#' @name arsenal_table
NULL
#> NULL

#' @rdname arsenal_table
#' @export
has_strata <- function(x) vapply(x$tables, function(x) x$strata$hasStrata, NA)

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

#' @rdname arsenal_table
#' @export
`[.arsenal_table` <- function(x, i, j, ...) {
  if(missing(i) && missing(j)) return(x)
  newx <- x

  give_warn <- function(vec) warning(paste0("Some indices not found in object: ", paste0(vec, collapse = ", ")), call. = FALSE)
  if(!missing(j))
  {
    if(is.character(j))
    {
      if(any(tmp <- j %nin% names(newx$tables)))
      {
        give_warn(j[tmp])
        j <- j[!tmp]
      }
      # we could leave "j" alone here and use the names, but for when names are missing ("") we'll do this
      j <- match(names(newx$tables), j, nomatch = 0)
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
        # we expect the names to be non-missing (""), unlike y
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

#' @rdname arsenal_table
#' @export
labels.arsenal_table <- function(object, ...) {

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
  tmp <- x$term
  for(i in seq_along(x$varterm2))
  {
    colon <- if(i > 1) ":" else ""
    space <- if(all(grepl(paste0(x$varterm2[i], ":"), tmp, fixed = TRUE))) "" else " "

    tmp <- sub(paste0(colon, x$varterm2[i]), paste0(colon, x$varlabel[i], space), tmp, fixed = TRUE)
  }
  x$label <- trimws(tmp)
  x
}

#' @rdname arsenal_table
#' @export
`labels<-.arsenal_table` <- function(x, value) {
  ## if the value vector is named, then assign the labels to
  ## those names that match those in x and y
  if(is.list(value)) value <- unlist(value)
  if(is.null(value))
  {
    for(i in seq_along(x$tables))
    {
      mk_lab <- function(elt)
      {
        if(is.null(elt$variable2) || is.null(elt$varlabel) || is.null(elt$varterm))
        { # for tableby
          elt$label <- elt$term
        } else
        {
          elt$varlabel <- elt$varterm
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
        if(nm %in% x$tables[[i]]$strata$term) x$tables[[i]]$strata$label[x$tables[[i]]$strata$term == nm] <- val
      }
    }
  } else stop("Unnamed label assignments are no longer supported")
  x
}

#' @rdname arsenal_table
#' @export
print.arsenal_table <- function(x, ...)
{
  cat(class(x)[1], "Object\n\nFunction Call:\n")
  print(x$Call)
  cat("\nVariable(s):\n")
  lapply(x$tables, function(tab) {
    cat(tab$y$term, " ~ ", paste0(unlist(lapply(tab$x, "[[", "term"), use.names = FALSE), collapse = ", "),
        if(tab$strata$hasStrata) paste0(" (strata = ", paste0(tab$strata$term, collapse = ", "), ")"), "\n", sep = "")
  })
  invisible(x)
}

#' @rdname arsenal_table
#' @export
merge.arsenal_table <- function(x, y, all = FALSE, all.x = all, all.y = all, ...) {
  Call <- match.call()

  nms.x <- names(x$tables)
  nms.y <- names(y$tables)
  nms <- if(all.x && all.y) union(nms.x, nms.y) else if(all.x) nms.x else if(all.y) nms.y else intersect(nms.x, nms.y)
  if(length(nms) == 0) stop("No terms in common. Do you need 'all=TRUE'?")
  x <- x[, nms.x %in% nms] # could use names themselves with warn=FALSE, but for freqlist, let's do this instead
  y <- y[, nms.y %in% nms]
  nms.x <- names(x$tables)
  nms.y <- names(y$tables)

  for(i in seq_along(y$tables))
  {
    ytrm <- names(y$tables)[i]
    if(ytrm == "")
    {
      x$tables[[length(x$tables) + 1]] <- y$tables[[i]] # this is why we need 'i' instead of just 'ytrm'
      next
    } else if(ytrm %nin% names(x$table))
    {
      x$tables[[ytrm]] <- y$tables[[ytrm]]
      next
    }
    if(!identical(x$tables[[ytrm]]$y, y$tables[[ytrm]]$y)) stop("By-variables not identical for term ", ytrm)
    if(!identical(x$tables[[ytrm]]$strata, y$tables[[ytrm]]$strata)) stop("Strata not identical for term ", ytrm)
    if(!identical(x$tables[[ytrm]]$adjust, y$tables[[ytrm]]$adjust)) stop("Adjust not identical for term ", ytrm)
    if(x$tables[[ytrm]]$hasWeights != y$tables[[ytrm]]$hasWeights) stop("Weights not present in both objects for term ", ytrm)
    if(!identical(x$tables[[ytrm]]$family, y$tables[[ytrm]]$family)) stop("Weights not present in both objects for term ", ytrm)

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

#' @rdname arsenal_table
#' @export
merge.freqlist <- function(x, y, all = TRUE, ...)
{
  nms.x <- setdiff(names(x$tables), "")
  nms.y <- setdiff(names(y$tables), "")
  # this check should be okay even if both nms.x and nms.y are empty
  if(any(nms.x %in% nms.y)) stop("Can only merge freqlist objects with different left-hand sides")
  NextMethod(all = TRUE)
}

#' @rdname arsenal_table
#' @export
print.summary.arsenal_table <- function(x, ..., format = if(!is.null(x$text) && x$text %in% c("html", "latex")) x$text else "markdown",
                                     escape = x$text %nin% c("html", "latex"), width = NULL, min.split = NULL)
{
  df <- as.data.frame(x, ..., width = width, min.split = min.split, list.ok = TRUE)

  #### finally print it out ####
  if(!is.null(x$title)) cat("\nTable: ", x$title, sep = "")
  shown <- FALSE
  for(i in seq_along(df))
  {
    if(nrow(df[[i]]) == 0) next
    shown <- TRUE
    print(knitr::kable(df[[i]], caption = NULL, align = attr(df[[i]], "align"), format = format, row.names = FALSE, escape = escape, ...))
    if(!is.null(attr(df[[i]], "tests"))) cat(paste0(attr(df[[i]], "tests"), "\n", collapse = ""))
  }
  if(!shown) stop("There wasn't anything to summarize! (All of the tables have 0 rows)")
  cat("\n")

  invisible(x)
}

