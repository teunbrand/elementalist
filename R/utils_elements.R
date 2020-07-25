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
    elem <- .int$combine_elements(child, parent)
  }
  if (length(class(elem)) < 3) {
    child <- if (type == "line") {
      element_line_seq(n = 1)
    } else {
      element_rect_seq(n = 1)
    }
    elem <- .int$combine_elements(child, elem)
  }

  return(elem)
}
