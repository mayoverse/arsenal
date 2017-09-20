
#' Include a YAML header in \code{write2}
#'
#' @param x An object of class \code{"yaml"}.
#' @param ... For \code{yaml()}, arguments to be bundled into a list and passed to \code{\link[yaml]{as.yaml}}.
#'   For \code{print.yaml()}, extra arguments. For \code{c.yaml()}, "yaml" objects to be concatenated.
#' @param recursive Not in use at this time.
#' @return A text string of class \code{"yaml"}.
#' @author Ethan Heinzen, adapted from an idea by Brendan Broderick
#' @examples
#' x <- yaml(title = "My cool title", author = "Ethan P Heinzen")
#' x
#' y <- yaml("header-includes" = list("\\usepackage[labelformat=empty]{caption}"))
#' y
#' c(x, y)
#' @name yaml
NULL
#> NULL

#' @rdname yaml
#' @export
yaml <- function(...)
{
  x <- yaml::as.yaml(list(...))
  class(x) <- "yaml"
  x
}

#' @rdname yaml
#' @export
print.yaml <- function(x, ...)
{
  cat(c("---\n", x, "---\n"), sep = "")
  invisible(x)
}

#' @rdname yaml
#' @export
c.yaml <- function(..., recursive = FALSE)
{
  structure(paste0(c(unlist(lapply(list(...), unclass))), collapse = ""), class = "yaml")
}

#' @rdname yaml
#' @export
is.yaml <- function(x)
{
  inherits(x, "yaml")
}
