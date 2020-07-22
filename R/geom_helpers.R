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

exchange_defaults <- function(data, type = "line", element) {
  if (inherits(element, "element_blank")) {
    data[] <- lapply(data, unset_default)
    return(data)
  }

  data <- as.list(data)
  is_line <- type == "line"

  # Colour
  if ("colour" %in% names(data)) {
    if (was_defaulted(data$colour)) {
      data$colour <- element$colour
    } else {
      if (is_line) {
        data$colour <- as_grouped_variable(alpha(data$colour, data$alpha))
      } else {
        data$colour <- as_grouped_variable(data$colour)
      }
    }
  }

  # Fill
  if ("fill" %in% names(data)) {
    if (was_defaulted(data$fill)) {
      data$fill <- element$fill
    } else {
      if (!is_line) {
        data$fill <- as_grouped_variable(alpha(data$fill, data$alpha))
      } else {
        data$fill <- as_grouped_variable(data$fill)
      }
    }
  }

  # Size
  if ("size" %in% names(data)) {
    if (was_defaulted(data$size)) {
      data$size <- element$size
    } else {
      data$size <- as_grouped_variable(data$size)
    }
  }

  # Linetype
  if ("linetype" %in% names(data)) {
    if (was_defaulted(data$linetype)) {
      data$linetype <- element$linetype
    } else {
      data$linetype <- as_grouped_variable(data$linetype)
    }
  }

  checked <- c("colour", "fill", "size", "linetype")
  not_checked <- setdiff(names(data), checked)
  data[not_checked] <- lapply(data[not_checked], unset_default)

  return(data)
}

by_default <- function(x, y) {
  if (was_defaulted(x)) {
    if (is.null(y)) {
      class(x) <- setdiff("defaulted", class(x))
      return(x)
    }
    return(rep_len(y, length(x)))
  } else {
    return(x)
  }
}

#' @export
#' @noRd
#' @keywords internal
vec_arith.defaulted <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

as_grouped_variable <- function(x) {
  vctrs::new_vctr(x, class = "grouped_variable", inherit_base_type = TRUE)
}

new_default <- function(x = double()) {
  vctrs::new_vctr(x, class = "defaulted", inherit_base_type = TRUE)
}

set_default <- function(x = double()) {
  new_default(x)
}

unset_default <- function(x) {
  class(x) <- setdiff(class(x), c("defaulted", "vctrs_vctr", "grouped_variable"))
  x
}

was_defaulted <- function(x) {
  inherits(x, "defaulted")
}

#' @export
vec_ptype2.defaulted.double <- function(x, y, ...) double()
#' @export
vec_ptype2.double.defaulted <- function(x, y, ...) double()
#' @export
vec_cast.defaulted.double <- function(x, to, ...) set_default(x)
#' @export
#' @method vec_cast.double defaulted
vec_cast.double.defaulted <- function(x, to, ...) as.double(vec_data(x))
