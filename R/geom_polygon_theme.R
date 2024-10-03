# User function -----------------------------------------------------------

#' Thematic polygons
#'
#' This geom closely follows \code{geom_polygon}, but take defaults from the
#' theme and are drawn through theme elements. They use the
#' \code{elementalist.geom_polygon} theme element.
#'
#' @inheritParams ggplot2::geom_polygon
#' @param element An \code{element_polygon} object, typically constructed with
#'   \code{element_polygon*} functions. Will inherit from the
#'   \code{elementalist.geom_polygon} theme element. When \code{NULL} this theme
#'   element is instead taken from the plot theme.
#'
#' @return A \code{LayerInstance} object that can be added to a plot.
#' @export
#'
#' @eval ggplot2:::rd_aesthetics("geom", "polygon_theme")
#'
#' @examples
#' df <- data.frame(
#'   x = c(0, 0.5, 1, 0.5, 0.25, 0.5, 0.75, 0.5),
#'   y = c(0.5, 0, 0.5, 1, 0.5, 0.25, 0.5, 0.75),
#'   sub_id = rep(c(1, 2), each = 4),
#'   id = rep(1, each = 8)
#' )
#'
#' ggplot(df, aes(x, y, group = id, subgroup = sub_id)) +
#'   geom_polygon_theme() +
#'   theme(elementalist.geom_polygon = element_polygon_glow(colour = "blue"))
geom_polygon_theme <- function(
  mapping = NULL, data = NULL,
  stat = "identity", position = "identity",
  rule = "evenodd",
  ...,
  na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE,
  element = NULL
) {
  element <- .check_geom_element(element, "polygon")
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPolygonTheme,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      rule = rule,
      element = element,
      ...
    )
  )
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname elementalist_extensions
GeomPolygonTheme <- ggproto(
  "GeomPolygonTheme", GeomPolygon,
  draw_panel = function(self, data, panel_params, coord, rule = "evenodd",
                        element = NULL) {
    element <- compute_element(child = element, type = "polygon")

    n <- nrow(data)
    if (n == 1) return(zeroGrob())

    munched <- coord_munch(coord, data, panel_params)

    if (is.null(munched$subgroup)) {
      munched <- munched[order(munched$group), ]
      id <- munched$group
    } else {
      munched <- munched[order(munched$group, munched$subgroup), ]
      id <- match(munched$subgroup, unique(munched$subgroup))
    }
    munched <- exchange_defaults(munched, "polygon", element)

    grob <- element_grob(
      element,
      x = munched$x, y = munched$y,
      id = id, pathId = munched$group,
      default.units = "native",
      colour = munched$colour,
      fill = munched$fill,
      linewidth = munched$linewidth,
      linetype = munched$linetype,
      rule = rule
    )
    grob$name <- grobName(grob, "geom_polygon_theme")
    grob
  },
  use_defaults = function(self, data, params = list(), modifiers = aes(), ...) {
    # Marks variables taken from the default values as the 'defaulted' class.
    provided_names <- union(colnames(data), names(params))
    data <- ggproto_parent(GeomPolygon, self)$use_defaults(
      data, params, modifiers
    )
    new_aes <- setdiff(colnames(data), provided_names)
    data[new_aes] <- lapply(data[new_aes], set_default)
    data
  }
 )
