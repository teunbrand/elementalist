# User function -----------------------------------------------------------

#' Interpolated line theme element
#'
#' Interpolates a line.
#'
#' @inheritParams ggplot2::element_line
#' @param fun A \code{function} to apply to the interpolated lines
#' @param n An \code{integer} of length one indicating how many points to
#'   interpolate.
#'
#' @return A \code{list} of the class \code{element_line_seq}
#' @export
#'
#' @examples
#' element_line_seq()
element_line_seq <- function(
  colour = NULL, linewidth = NULL, linetype = NULL, lineend = NULL,
  color = NULL, arrow = NULL, inherit.blank = FALSE,
  fun = defer, n = 50
) {
  if (!is.null(color))
    colour <- color
  if (is.null(arrow))
    arrow <- FALSE
  structure(
    list(
      colour = colour,
      linewidth = linewidth,
      linetype = linetype,
      lineend = lineend,
      arrow = arrow,
      inherit.blank = inherit.blank,
      n = n,
      fun = fun
    ),
    class = c("element_line_seq", "element_line", "element")
  )
}

# Grob generator ----------------------------------------------------------

#' @rdname element_line_seq
#' @usage NULL
#' @format NULL
#' @keywords internal
#' @export
element_grob.element_line_seq <- function(
  element, x = 0:1, y = 0:1, colour = NULL, linewidth = NULL,
  linetype = NULL, lineend = NULL, default.units = "npc",
  id.lengths = NULL, id = NULL, fun = NULL, ...
) {
  fun_gp <- gpar(
    # col = colour, fill = colour,
    # lwd = check_zerolength(linewidth * .pt),
    lty = linetype,
    lineend = lineend
  )
  element_gp <- gpar(
    # col = element$colour,
    fill = element$colour,
    # lwd = check_zerolength(element$linewidth * .pt),
    lty = element$linetype,
    lineend = lineend
  )

  if (is.null(colour)) {
    colour <- element$colour
  }
  if (is.null(linewidth)) {
    linewidth <- check_zerolength(element$linewidth * .pt)
  } else {
    linewidth <- check_zerolength(linewidth * .pt)
  }

  arrow <- if (is.logical(element$arrow) && !element$arrow) {
    NULL
  }
  else {
    element$arrow
  }
  lineseqGrob(
    x, y,
    default.units = default.units,
    colour = colour,
    linewidth = linewidth,
    gp = modify_list(element_gp, fun_gp),
    id = id,
    id.lengths = id.lengths,
    n = element$n,
    fun = element$fun,
    arrow = arrow,
    ...
  )
}

# Grob --------------------------------------------------------------------

#' @keywords internal
lineseqGrob <- function(x = 0:1, y = 0:1,
                        id = NULL, id.lengths = NULL,
                        default.units = "npc",
                        colour = NULL,
                        linewidth = NULL,
                        arrow = NULL,
                        name = NULL,
                        n = 10,
                        fun = defer,
                        gp = gpar(),
                        vp = NULL) {
  id <- resolve_id(id, id.lengths, length(x))
  force(gp)
  proto <- apply_lines(fun, x, y, colour, linewidth, id, default.units, n)

  if (!is.null(proto$colour)) {
    gp$col <- check_zerolength(proto$colour)
  }
  if (!is.null(proto$linewidth)) {
    gp$lwd <- check_zerolength(proto$linewidth)
  }
  if (!is.null(proto$sub_id)) {
    id <- paste0(proto$id, "$", proto$sub_id)
    id <- match(id, unique(id))
  } else {
    id <- proto$id
  }

  decide_linegrob(
    x = proto$x, y = proto$y,
    id = id,
    default.units = default.units, arrow = arrow,
    name = name, gp = gp, vp = vp
  )
}
