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
  class(x) <- setdiff(class(x), c("defaulted", "vctrs_vctr"))
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
