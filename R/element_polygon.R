#' Polygon theme elements
#'
#' Constructor for polygon theme elements.
#'
#' @param fill A colour specification for the fill.
#' @param colour A colour specification for the borders or lines.
#' @param size A \code{numeric} for the width of lines in millimetres.
#' @param linetype A \code{integer} or \code{string} specifying a line type.
#' @param linejoin One of the following strings: \code{"round"}, \code{"mitre"}
#'   or \code{"bevel"}.
#' @param lineend One of the following strings: \code{"round"}, \code{"mitre"}
#'   or \code{"bevel"}.
#' @param color Alias for the \code{colour} argument.
#' @param inherit.blank A \code{logical} of length 1: should this element
#'   inherit the existence of an \code{element_blank} among its parents? If
#'   \code{TRUE}, the existance of a blank element among its parents will cause
#'   this element to be blank as well. If \code{FALSE}, any blank parent will be
#'   ignored when calculating final element state.
#'
#' @details Themes in vanilla ggplot2 do not contain polygons.
#'
#' @return A \code{list} with the class \code{element_polygon}.
#' @export
#'
#' @examples
#' NULL
element_polygon <- function(
  fill = NULL,
  colour = NULL,
  size = NULL,
  linetype = NULL,
  linejoin = NULL,
  lineend  = NULL,
  color = NULL,
  inherit.blank = FALSE
) {
  # Just pass to generic without params or subtypes
  element_polygon_generic(
    fill = fill, colour = colour, size = size,
    linetype = linetype, linejoin = linejoin, lineend = lineend,
    color = color, inherit.blank = inherit.blank
  )
}


#' Generic polygon element generator
#'
#' @inheritParams element_polygon
#' @param subtype A \code{character} of length 1 specifying a subclass of
#'   polygon, noting a particular style.
#' @param params A named \code{list} with parameters for the styling of the
#'   polygon.
#'
#' @return A \code{list} with the \code{subclass, element_polygon} class.
#'
#' @details Styled variations on \code{element_polygon()} call this function
#'   with hardcoded \code{subtype}s. This function is not exported since it is
#'   not expected to work with arbitrary \code{subtype} and \code{params}
#'   arguments.
#'
#' @noRd
#' @keywords internal
#'
#' @examples
#' NULL
element_polygon_generic <- function(
  fill = NULL,
  colour = NULL,
  size = NULL,
  linetype = NULL,
  linejoin = NULL,
  lineend = NULL,
  color = NULL,
  inherit.blank = FALSE,
  subtype = NULL,
  params = list()
) {
  if (!is.null(color)) {
    colour <- color
  }
  structure(
    list(
      fill = fill,
      colour = colour,
      size = size,
      linetype = linetype,
      linejoin = linejoin,
      lineend  = lineend,
      inherit.blank = inherit.blank,
      params = params
    ),
    class = c(subtype, "element_polygon", "element")
  )
}

#' @export
#' @method element_grob element_polygon
element_grob.element_polygon <- function(
  element,
  x = c(0, 0.5, 1, 0.5),
  y = c(0.5, 1, 0.5, 0),
  colour = NULL, fill = NULL, size = NULL,
  linetype = NULL, lineend = "round", linejoin = "round",
  id = NULL, pathId = NULL,
  id.lengths = NULL, pathId.lengths = NULL,
  default.units = "npc", rule = "evenodd",
  ...
) {
  fun_gp <- gpar(
    col = colour, fill = fill,
    lwd = check_zerolength(size * .pt),
    lty = linetype, lineend = lineend, linejoin = linejoin
  )

  element_gp <- gpar(
    col = element$colour, fill = element$fill,
    lwd = check_zerolength(element$size * .pt),
    lty = element$linetype,
    lineend = element$linetype, linejoin = element$linejoin
  )

  sub_id <- resolve_id(pathId, pathId.lengths, length(x))
  id <- resolve_id(id, id.lengths, length(x))

  gp <- modify_list(element_gp, fun_gp)

  pathGrob(
    x = x, y = y, id = id, pathId = sub_id,
    gp = gp, default.units = default.units,
    rule = rule,
    ...
  )
}
