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

#' @export
#' @noRd
#' @keywords internal
vec_arith.defaulted <- function(op, x, y, ...) {
  vec_arith_base(op, x, y)
}

as_grouped_variable <- function(x = numeric()) {
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

#' @export
vec_ptype2.grouped_variable.double <- function(x, y, ...) as_grouped_variable()
#' @export
vec_ptype2.double.grouped_variable <- function(x, y, ...) as_grouped_variable()
#' @export
vec_casted.grouped_variable.double <- function(x, to, ...) as_grouped_variable(x)
#' @export
#' @method vec_cast.double grouped_variable
vec_cast.double.grouped_variable <- function(x, to, ...) as.double(vec_data(x))
#' @export
vec_arith.grouped_variable <- function(op, x, y, ...) {
  UseMethod('vec_arith.grouped_variable', y)
}
#' @export
#' @method vec_arith.grouped_variable default
vec_arith.grouped_variable.default <- function(op, x, y, ...) {
  as_grouped_variable(vec_math_base(op, vec_data(x), y))
}
