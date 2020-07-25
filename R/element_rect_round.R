# User function -----------------------------------------------------------

#' Rounded rectangle theme element
#'
#' Draws rectangles with rounded corners in conjuction with the theme system.
#'
#' @inheritParams ggplot2::element_rect
#' @param radius A \code{unit} of length 1 specifying the maximum radius of
#'   circles inscribing the corners. If given a \code{numeric}, it is converted
#'   to a \code{unit} object with the \code{"snpc"} unit.
#'
#' @return A \code{list} of the class \code{element_rect_round}.
#' @export
#'
#' @examples
#' # Adding rounded rectangles to geoms and theme elements
#' ggplot(mpg, aes(class, colour = class)) +
#'   geom_bar_theme(element = element_rect_round(radius = 0.25)) +
#'   facet_grid(~ year) +
#'   theme(
#'     strip.background = element_rect_round(radius = 0.5),
#'     panel.background = element_rect_round()
#'   )
element_rect_round <- function(
  fill = NULL,
  colour = NULL,
  size = NULL,
  linetype = NULL,
  color = NULL,
  inherit.blank = FALSE,
  radius = unit(0.1, "snpc")
) {
  if (!is.null(color)) colour <- color
  if (!is.unit(radius)) radius <- unit(radius, "snpc")
  structure(
    list(fill = fill,
         colour = colour,
         size = size,
         linetype = linetype,
         inherit.blank = inherit.blank,
         radius = radius),
    class = c("element_rect_round", "element_rect", "element")
  )
}

# Grob generator ----------------------------------------------------------

#' @rdname element_rect_round
#' @usage NULL
#' @format NULL
#' @keywords internal
#' @export
element_grob.element_rect_round <- function(
  element,
  x = 0.5, y = 0.5,
  width = 1, height = 1,
  fill = NULL, colour = NULL, size = NULL,
  linetype = NULL, lineend = "butt", linejoin = "mitre",
  default.units = "npc",
  fun = NULL, ...
) {
  fun_gp <- gpar(
    col = colour, fill = fill,
    lwd = check_zerolength(size * .pt),
    lty = linetype,
    lineend = lineend,
    linejoin = linejoin
  )
  element_gp <- gpar(
    col = element$colour,
    fill = element$fill,
    lwd = check_zerolength(element$size * .pt),
    lty = element$linetype,
    lineend = lineend
  )
  gp <- modify_list(element_gp, fun_gp)

  multiroundrectGrob(
    x, y, width, height,
    default.units, r = element$radius,
    gp = gp, ...
  )
}

# Grob --------------------------------------------------------------------

#' @keywords internal
multiroundrectGrob <- function(
  x = 0.5, y = 0.5, width = 1, height = 1,
  default.units = "npc", r = unit(0.1, "snpc"),
  just = "centre", name = NULL, gp = NULL, vp = NULL
) {
  len <- max(length(x), length(y), length(width), length(height))
  x <- rep_len(x, len)
  y <- rep_len(y, len)
  width = rep_len(width, len)
  height = rep_len(height, len)
  r <- rep(r, len)

  gps <- lapply(seq_len(len), function(i) {
    g <- gp
    g[] <- lapply(g, function(x) {
      if (length(x) == 1) {
        return(x)
      } else {
        x[i]
      }
    })
    g
  })

  grobs <- mapply(roundrectGrob,
                  x = x, y = y, width = width, height = height,
                  default.units = default.units, r = list(r), just = list(just),
                  name = list(NULL), gp = gps, vp = list(NULL), SIMPLIFY = FALSE)
  do.call(grobTree, c(grobs, list(name = name, vp = vp)))
}
