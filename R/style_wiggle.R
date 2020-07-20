#' Wiggle style
#'
#' 'Wiggle' is a theme style that adds an amount of cumulative uniform noise to
#' interpolated lines, making them wiggle a bit.
#'
#' @param amount A \code{numeric} of length 1 setting the amount of wiggling to
#'   occur.
#' @param seed An \code{integer} to set the seed for reproducible wiggling.
#' @inheritDotParams element_line_seq
#' @inheritDotParams element_rect_seq
#'
#' @details The amount of wiggle added to lines and rectangles is absolute. This
#' makes it easier to make more uniform wiggles, but causes relative distortion
#' when resizing the plot window or device.
#'
#' The \code{element_*_wiggle()} functions are convenience wrappers around
#' \code{element_*_seq()}.
#'
#' The \code{wiggle()} function is a function factory that produces a
#' function, that subsequently can be used to wiggle points.
#'
#' @return A \code{function}.
#' @export
#'
#' @examples
#' plot <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(~ drv)
#'
#' # Line wiggles
#' plot + theme(
#'   axis.line = element_line_wiggle(1),
#'   panel.grid.major = element_line_wiggle(1)
#' )
#'
#' # Turning clipping off makes the panel background wiggle better
#' plot +
#'   coord_cartesian(clip = "off") +
#'   theme(
#'     strip.background = element_rect_wiggle(),
#'     panel.background = element_rect_wiggle(sides = "lb", colour = "blue")
#'   )
wiggle <- function(amount = 3, seed = NULL) {
  seed <- force(seed)
  amount <- amount / 2
  function(x, y, colour, size = NULL, id, n) {
    nn <- n * (length(x) - 1) + 1
    id <- rep(id[1], nn)
    if (!is.null(seed)) {
      set.seed(seed)
      on.exit(set.seed(NULL)) # Reset on exit
    }
    z <- cumsum(runif(nn, -amount, amount))
    xy <- fit_along(unclass(x), unclass(y), z)
    x <- seq_between(unclass(x), n)
    y <- seq_between(unclass(y), n)
    col <- c(col_interpol(colour, nn - 1), NA)
    if (!is.null(size)) {
      size <- c(rep_len(size, length(x) - 1), NA)
    }

    out <- list(
      x = x,
      y = y,
      dx = unclass(xy$x) - x,
      dy = unclass(xy$y) - y,
      col = col,
      lwd = size,
      id = id
    )
    out[vapply(out, is.null, logical(1))] <- NULL
    out
  }
}

#' @rdname wiggle
#' @export
element_line_wiggle <- function(amount = 3, seed = NULL, ...) {
  element_line_seq(fun = wiggle(amount, seed), ...)
}

#' @rdname wiggle
#' @export
element_rect_wiggle <- function(amount = 3, seed = NULL, ...) {
  element_rect_seq(fun = wiggle(amount, seed), ...)
}
