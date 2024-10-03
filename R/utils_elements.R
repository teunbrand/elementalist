.check_geom_element <- function(element, shape = "line") {
  if (is.null(element)) {
    return(element)
  }
  class <- paste0("element_", shape)
  if (!inherits(element, c(class, "element_blank"))) {
    stop("The `element` argument should be of type `", class, "`.",
         call. = FALSE)
  }
  element
}

# This is just a temporary hacky solution until ggplot2#2749 is implemented
sniff_theme <- function(tries = 20) {
  theme <- NULL
  for (i in seq_len(tries)) {
    env <- parent.frame(i)
    if ("theme" %in% names(env)) {
      theme <- env$theme
      break
    }
  }
  if (is.null(theme)) {
    theme <- theme_get()
  }
  theme
}

compute_element <- function(child = NULL, type = "line") {
  # Get from global theme first
  theme <- sniff_theme()
  parent <- calc_element(paste0("elementalist.geom_", type), theme)
  if (is.null(child)) {
    elem <- parent
  } else {
    # Compute child when given
    elem <- combine_elements(child, parent)
  }
  if (length(class(elem)) < 3) {
    child <- switch (type,
      line = element_line_seq(n = 1),
      rect = element_rect_seq(n = 1),
      element_polygon()
    )
    elem <- combine_elements(child, elem)
  }

  return(elem)
}

combine_elements <- function(e1, e2) {

  if (is.null(e2) || inherits(e1, "element_blank")) {
    return(e1)
  }
  if (is.null(e1)) {
    return(e2)
  }
  if (inherits(e1, "rel")) {
    if (inherits(e2, "rel")) {
      return(rel(unclass(e1) * unclass(e2)))
    }
    if (is.numeric(e2) || is.unit(e2)) {
      return(unclass(e1) * e2)
    }
    return(e1)
  }
  if (!inherits(e1, "element") && !inherits(e2, "element")) {
    return(e1)
  }
  if (inherits(e2, "element_blank")) {
    if (e1$inherit.blank) {
      return(e2)
    } else {
      return(e1)
    }
  }
  n <- names(e1)[vapply(e1, is.null, logical(1))]
  e1[n] <- e2[n]

  if (inherits(e1$size, "rel")) {
    e1$size <- e2$size * unclass(e1$size)
  }
  if (inherits(e1$linewidth, "rel")) {
    e1$linewidth <- e2$linewidth * unclass(e1$linewidth)
  }
  inheritance <- inherits(e2, class(e1), which = TRUE)
  is_subclass <- !any(inheritance == 0) && length(setdiff(class(e2), class(e1))) > 0
  if (is_subclass) {
    new <- c(e1, e2[setdiff(names(e2), names(e1))])
    e2[names(new)] <- new
    return(e2)
  }
  e1
}
