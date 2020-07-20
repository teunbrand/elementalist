# User function -----------------------------------------------------------

#' These geoms closely follow \code{geom_rect()} and \code{geom_tile()} but take
#' defaults from the theme and are drawn through theme elements. They use the
#' \code{elementalist.geom_rect} theme element.
#'
#' @inheritParams ggplot2::geom_rect
#' @inheritParams ggplot2::geom_tile
#'
#' @return A \code{LayerInstance} object that can be added to a plot.
#' @export
#'
#' @eval ggplot2:::rd_aesthetics("geom", "tile_theme")
#'
#' @examples
#' df <- data.frame(
#'   x = rep(c(2, 5, 7, 9, 12), 2),
#'   y = rep(c(1, 2), each = 5),
#'   z = factor(rep(1:5, each = 2)),
#'   w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
#' )
#'
#' ggplot(df, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1)) +
#'   geom_rect_theme(aes(fill = z)) +
#'   theme(elementalist.geom_rect = element_rect_wiggle())
#'
#' ggplot(df, aes(x, y, width = w)) +
#'   geom_tile_theme(aes(fill = z)) +
#'   theme(elementalist.geom_rect = element_rect_multicolour())
geom_rect_theme <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            linejoin = "mitre",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRectTheme,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @rdname geom_rect_theme
geom_tile_theme <- function(
  mapping = NULL, data = NULL,
  stat = "identity", position = "identity",
  ...,
  linejoin = "mitre", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTileTheme,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}


# ggproto -----------------------------------------------------------------

## rectangle --------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname elementalist_extensions
GeomRectTheme <- ggproto(
  "GeomRectTheme", GeomRect,
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
    # browser()
    theme <- sniff_theme()

    el <- calc_element("elementalist.geom_rect", theme)
    if (length(setdiff(class(el), c("element_rect", "element"))) < 1) {
      data[] <- lapply(data, unset_default)
      grob <- ggproto_parent(GeomRect, self)$draw_panel(
         data = data, panel_params = panel_params,
         coord = coord, linejoin = linejoin
      )
      return(grob)
    }

    if (!coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x","y", "xmin", "xmax", "ymin", "ymax")
      )
      stop("Polar coordinates not implemented yet. Feel free to leave an issue",
           " on the github tracker.",
           call. = FALSE)
    } else {
      coords <- coord$transform(data, panel_params)
      if (was_defaulted(coords$fill)) {
        fill <- rep_len(el$fill, nrow(coords))
        if (!was_defaulted(coords$alpha)) {
          fill <- alpha(fill, coords$alpha)
        }
      } else {
        fill <- alpha(coords$fill, coords$alpha)
      }

      if (was_defaulted(coords$colour)) {
        colour <- el$colour
      } else {
        colour <- as_grouped_variable(coords$colour)
      }

      grob <- element_grob(
        el,
        coords$xmin, coords$ymax,
        width = coords$xmax - coords$xmin,
        height = coords$ymax - coords$ymin,
        default.units = "native",
        just = c("left", "top"),
        colour = colour,
        fill = fill,
        size = by_default(coords$size, el$size),
        linetype = by_default(coords$linetype, el$linetype),
        # linejoin = linejoin,
        lineend = if (identical(linejoin, "round")) "round" else "square"
      )
      grob
    }
  },
  use_defaults = function(self, data, params = list(), modifiers = aes()) {
    provided_names <- union(colnames(data), names(params))
    data <- ggproto_parent(GeomLine, self)$use_defaults(
      data, params, modifiers
    )
    new_aes <- setdiff(colnames(data), provided_names)
    data[new_aes] <- lapply(data[new_aes], set_default)
    data
  }
)

## tile -------------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname elementalist_extensions
GeomTileTheme <- ggproto(
  "GeomTileTheme", GeomRectTheme,
  extra_params = c("na.rm"),
  setup_data = function(data, params) {
    data$width  <- data$width  %||% params$width  %||% resolution(data$x, FALSE)
    data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)

    transform(
      data,
      xmin = x - width  / 2, xmax = x + width  / 2, width  = NULL,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },
  default_aes = aes(fill = "grey20", colour = NA, size = 0.1, linetype = 1,
                    alpha = NA, width = NA, height = NA),
  required_aes = c("x", "y"),
  draw_key = draw_key_polygon
)
