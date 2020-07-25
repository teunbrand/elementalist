
#' @title Vctr utilies
#'
#' @description Elementalist uses vctrs to keep track of some circumstances in
#'   which variables were created, namely when they were defaulted and wheter
#'   some variables came from the theme or the geom. These functions help
#'   organise this a bit better, but should not be of interest to any user. This
#'   documentation merely exists to satisfy the R CMD check.
#'
#' @name utils_vctrs
#' @keywords internal
NULL

#' @export
#' @keywords internal
#' @aliases NULL
#' @rdname utils_vctrs
#' @inheritParams vctrs::vec_arith
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
#' @rdname utils_vctrs
#' @inheritParams vctrs::vec_ptype2
#' @noRd
#' @keywords internal
vec_ptype2.defaulted.double <- function(x, y, ...) double()

#' @export
#' @rdname utils_vctrs
#' @noRd
#' @keywords internal
vec_ptype2.double.defaulted <- function(x, y, ...) double()

#' @export
#' @rdname utils_vctrs
#' @inheritParams vctrs::vec_cast
#' @noRd
#' @keywords internal
vec_cast.defaulted.double <- function(x, to, ...) set_default(x)

#' @export
#' @rdname utils_vctrs
#' @method vec_cast.double defaulted
#' @noRd
#' @keywords internal
vec_cast.double.defaulted <- function(x, to, ...) as.double(vec_data(x))

#' @export
#' @rdname utils_vctrs
#' @noRd
#' @keywords internal
vec_ptype2.grouped_variable.double <- function(x, y, ...) as_grouped_variable()

#' @export
#' @rdname utils_vctrs
#' @noRd
#' @keywords internal
vec_ptype2.double.grouped_variable <- function(x, y, ...) as_grouped_variable()

#' @export
#' @rdname utils_vctrs
#' @noRd
#' @keywords internal
vec_cast.grouped_variable.double <- function(x, to, ...) as_grouped_variable(x)

#' @export
#' @rdname utils_vctrs
#' @method vec_cast.double grouped_variable
#' @noRd
#' @keywords internal
vec_cast.double.grouped_variable <- function(x, to, ...) as.double(vec_data(x))

#' @export
#' @keywords internal
#' @aliases NULL
#' @rdname utils_vctrs
#' @method vec_arith grouped_variable
#' @inheritParams vctrs::vec_arith
vec_arith.grouped_variable <- function(op, x, y, ...) {
  as_grouped_variable(vec_arith_base(op, vec_data(x), y))
}
