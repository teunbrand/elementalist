#' Glowing lines
#'
#' Makes copies of lines with increasing size and decreasing alpha, giving an
#' glowing appearance. The \code{element_*_glow()} functions are convenience
#' wrappers around \code{element_*_seq()}.
#'
#' @param amount A \code{numeric} larger than 0 for the amount of glow to add.
#' @inheritDotParams element_line_seq
#' @inheritDotParams element_rect_seq
#'
#' @details The \code{glow} function is a function factory that produces a
#' function, that subsequently can be used to make lines glow.
#'
#' When the elements have no colours by setting them to \code{NA}, this will not
#' draw glowing lines.
#'
#' @return A \code{function}.
#' @export
#'
#' @examples
#' element_line_glow()
glow <- function(amount = 3) {
  amount <- force(amount)
  function(x, y, colour, size = 1, id, n) {
    if (all(is.na(colour))) {
      out <- list(
        x = x,
        y = y,
        col = colour,
        lwd = size,
        id = id,
        sub_id = id
      )
      return(out)
    }
    len <- length(x)
    id <- rep(id, n)
    sub_id <- rep(seq_len(n), each = len)
    x <- rep(x, n)
    y <- rep(y, n)

    colour <- alpha(colour, NA)
    alpha <- extract_alpha(colour)
    colour <- substr(colour, 1, 7)

    # Alpha sequence based on coolbutuseless' ggblur alpha.R
    alpha <- seq(1/(n + 1), alpha, length.out = n + 1)
    alpha <- head(alpha, -1)
    ialpha <- numeric(n)
    ialpha[1] <- alpha[1]
    for (i in seq(n - 1)) {
      ialpha[i + 1] <- 1 - (1 - alpha[i + 1]) / (1 - alpha[i])
    }

    colour <- alpha(colour, rev(ialpha))
    colour <- rep(c(colour), each = len)

    size <- qcauchy(seq(0.5, 0.975, length.out = n), size, amount)
    size <- rep(c(size), each = len)

    out <- list(
      x = x,
      y = y,
      col = colour,
      lwd = size,
      id = id,
      sub_id = sub_id
    )
    out[vapply(out, is.null, logical(1))] <- NULL
    out
  }
}

#' @rdname glow
#' @export
element_line_glow <- function(amount = 3, ...) {
  element_line_seq(fun = glow(amount), ...)
}

#' @rdname glow
#' @export
element_rect_glow <- function(amount = 3, ...) {
  element_rect_seq(fun = glow(amount), ...)
}
