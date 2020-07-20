#' Multicolour lines
#'
#' Interpolates lines and colour vectors, creating a gradient-like line. The
#' \code{element_*_multicolour()} functions are convenience wrappers around
#' \code{element_*_seq()}.
#'
#' @param colour A \code{character} vector with colour names.
#' @inheritDotParams element_line_seq
#' @inheritDotParams element_rect_seq
#'
#' @details The \code{multicolour} function is a function factory that produces
#' a function, that subsequently can be used to gradient-colour points.
#'
#' For the \code{element_rect_multicolour()} the gradient is applied to each
#' side seperately.
#'
#' @return A \code{function}.
#' @export
#'
#' @examples
#' element_line_multicolour()
multicolour <- function() {
  function(x, y, colour, size = NULL, id, n) {
    if (!is.null(colour)) {
      n <- n  + 1
    }
    nn <- n * (length(x) - 1)
    x <- seq_between(unclass(x), n)
    y <- seq_between(unclass(y), n)
    col <- c(col_interpol(colour, nn), NA)
    if (!is.null(size)) {
      size <- c(rep_len(size, length(x) - 1), NA)
    }
    id <- rep(id[1], length(x))
    out <- list(
      x = x,
      y = y,
      col = col,
      lwd = size,
      id = id
    )
    out[vapply(out, is.null, logical(1))] <- NULL
    return(out)
  }
}

#' @rdname multicolour
#' @export
#' @importFrom grDevices rainbow
element_line_multicolour <- function(colour = rainbow(10), ...) {
  element_line_seq(fun = multicolour(), color = colour, ...)
}

#' @export
#' @rdname multicolour
element_rect_multicolour <- function(colour = rainbow(10), ...) {
  element_rect_seq(fun = multicolour(), color = colour, ...)
}
