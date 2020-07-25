# User functions ----------------------------------------------------------

#' Thematic bars
#'
#' These geoms closely follow the \code{geom_col()} and \code{geom_tile()} but
#' take defaults from the theme and are drawn through theme elements. They use
#' the \code{elementalist.geom_rect} theme element.
#'
#' @inheritParams ggplot2::geom_col
#' @inheritParams ggplot2::geom_bar
#' @inheritParams ggplot2::geom_histogram
#' @inheritParams geom_rect_theme
#'
#' @return A \code{Layer} object that can be added to a plot.
#' @export
#'
#' @eval ggplot2:::rd_aesthetics("geom", "col_theme")
#'
#' @include geom_rect_theme.R
#'
#' @examples
#' df <- data.frame(
#'   category = LETTERS[1:5],
#'   value = c(10, 5, 2, 8, 9)
#' )
#'
#' # Styling through the partial theme setters
#' ggplot(df, aes(category, value)) +
#'   geom_col_theme() +
#'   wiggling_geoms()
#'
#' # Styling through the `element` argument
#' ggplot(mpg, aes(class)) +
#'   geom_bar_theme(aes(colour = class),
#'                  element = element_rect_glow())
#'
#' # Styling through the main theme
#' ggplot(diamonds, aes(log10(carat))) +
#'   geom_histogram_theme(bins = 20) +
#'   theme(
#'     elementalist.geom_rect = element_rect_multicolour()
#'   )
geom_col_theme <- function(mapping = NULL, data = NULL,
                           position = 'stack',
                           ...,
                           width = NULL,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           element = NULL) {
  element <- .check_geom_element(element, "rect")
  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomColTheme,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      na.rm = na.rm,
      element = element,
      ...
    )
  )
}

#' @export
#' @rdname geom_col_theme
geom_bar_theme <- function(mapping = NULL, data = NULL,
                           stat = "count", position = "stack",
                           ...,
                           width = NULL, na.rm = FALSE, orientation = NA,
                           show.legend = NA,
                           inherit.aes = TRUE, element = NULL) {
  element <- .check_geom_element(element, "rect")
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomColTheme,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      na.rm = na.rm,
      orientation = orientation,
      element = element,
      ...
    )
  )
}

#' @export
#' @rdname geom_col_theme
geom_histogram_theme <- function(mapping = NULL, data = NULL,
                                 stat = "bin", position = "stack",
                                 ...,
                                 binwidth = NULL,
                                 bins = NULL,
                                 na.rm = FALSE,
                                 orientation = NA,
                                 show.legend = NA,
                                 inherit.aes = TRUE,
                                 element = NULL) {
  element <- .check_geom_element(element, "rect")
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomColTheme,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      binwidth = binwidth,
      bins = bins,
      na.rm = na.rm,
      orientation = orientation,
      pad = FALSE,
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
GeomColTheme <- ggproto(
  "GeomColTheme", GeomRectTheme,
  required_aes = c("x", "y"),
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },
  extra_params = c("na.rm", "orientation", "width"),
  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$width <- data$width %||% params$width %||%
      (resolution(data$x, FALSE) * 0.9)
    data <- transform(
      data,
      ymin = pmin(y, 0), ymax = pmax(y, 0),
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
    flip_data(data, params$flipped_aes)
  }
)






