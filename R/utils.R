
#' Defer doing anything to data
#'
#' This function is used as a stand-in for an element modifying function that
#' does nothing special to the data except place it in a data.frame.
#'
#' @param ... Named arguments
#'
#' @return A \code{data.frame}
#' @export
#'
#' @examples
#' element_line_seq(fun = defer)
defer <- function(...) {
  x <- list(...)
  x[vapply(x, is.null, logical(1))] <- NULL
  new_df(x)
}

#' @keywords internal
#' A cheat data frame constructor based on R4.0.0 list2DF
#' Only change is rep_len --> rep
#' This is to deal with units more properly
new_df <- function (x = list(), nrow = NULL)
{
  stopifnot(is.list(x), is.null(nrow) || nrow >= 0L)
  if (n <- length(x)) {
    if (is.null(nrow))
      nrow <- max(lengths(x), 0L)
    x <- lapply(x, rep, length.out = nrow)
  }
  else {
    if (is.null(nrow))
      nrow <- 0L
  }
  if (is.null(names(x)))
    names(x) <- character(n)
  class(x) <- "data.frame"
  attr(x, "row.names") <- .set_row_names(nrow)
  x
}

#' @keywords internal
#' Returns x or NULL, not (!) 0-length vectors like numeric(0) or character(0)
check_zerolength <- function(x) {
  if (length(x) == 0)
    NULL
  else x
}

#' @keywords internal
#' Replace objects with the same name in old with new
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

#' @keywords internal
#' Just to make it easier to lapply over the indices instead of objects.
inv_subset <- function(i, x) {
  x[i]
}

#' @keywords internal
inherit_unit <- function(x, ptype = NULL, default) {
  if (is.unit(x)) {
    return(x)
  }
  if (is.null(ptype)) {
    return(unit(x, default))
  }
  type <- if (is.unit(ptype)) {
    unit_type(ptype)
  } else {
    NULL}
  if (!is.null(type)) {
    return(unit(x, type))
  }
  return(unit(x, default))
}

#' @keywords internal
seq_between <- function(x, n) {
  if (n == 1 || {lx <- length(x)} == 1) {
    return(x)
  }
  seq <- seq(0, 1, length.out = n + 1)[-(n + 1)]
  dx <- diff(x)
  out <- rep(seq, lx - 1) * rep(dx, each = n) + rep(x[-lx], each = n)
  c(out, x[lx])
}

# An unlist that should prevent attributes to be dropped when the class has
# appropriate c() methods.
stitch <- function(list) {
  UseMethod("stitch", list[[1]])
}

stitch.default <- function(list) {
  do.call(c, list)
}

stitch.unit <- function(list) {
  do.call(unit.c, list)
}

# To standardise IDs across grobs
resolve_id <- function(id, id.lengths, alt_length) {
  id <- if (is.null(id)) {
    if (is.null(id.lengths)) {
      rep(1L, alt_length)
    } else {
      rep(seq_along(id.lengths), id.lengths)
    }
  } else {
    id
  }
}

length_uni <- function(x) {
  length(unique(x))
}

.grab_ggplot_internals <- function() {
  objects <- c("axis_label_element_overrides", "draw_axis_labels",
               "combine_elements")
  objects <- setNames(nm = objects)
  out <- lapply(objects, function(i) {
    utils::getFromNamespace(i, "ggplot2")
  })
}

.int <- .grab_ggplot_internals()

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


