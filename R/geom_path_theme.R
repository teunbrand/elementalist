# User functions ----------------------------------------------------------

#' Lines for connected observations
#'
#' These mostly function as \code{\link[ggplot2:geom_path]{geom_line()}} and
#' \code{geom_path()} but replaces default values from the
#' \code{elementalist.geom_line} theme element and uses it to generate the grob.
#'
#' @inheritParams ggplot2::geom_line
#' @inheritParams ggplot2::geom_path
#'
#' @return A \code{LayerInstance} ggproto object that can be added to a plot.
#' @export
#'
#' @eval ggplot2:::rd_aesthetics("geom", "path_theme")
#'
#' @examples
#' ggplot(pressure, aes(temperature, pressure)) +
#'   geom_line_theme() +
#'   theme(
#'     elementalist.geom_line = element_line_wiggle(10, colour = "red", n = 10)
#'   )
geom_path_theme <- function(
  mapping = NULL, data = NULL,
  stat = "identity", position = "identity",
  ...,
  lineend = "butt", linejoin = "round", linemitre = 10,
  arrow = NULL, na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPathTheme,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @rdname geom_path_theme
geom_line_theme <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomLineTheme,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          na.rm = na.rm,
          orientation = orientation,
          ...
        ))
}

# ggproto -----------------------------------------------------------------

## path -------------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname elementalist_extensions
GeomPathTheme <- ggproto(
  "GeomPathTheme", GeomPath,
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round",
                        linemitre = 10, na.rm = FALSE) {
    if (!anyDuplicated(data$group)) {
      message("geom_path: Each group consists of only one observation. ",
              "Do you need to adjust the group aesthetic?")
    }
    # browser()
    theme <- sniff_theme()

    el <- calc_element("elementalist.geom_line", theme)
    if (length(setdiff(class(el), c("element_line", "element"))) < 1) {
      data[] <- lapply(data, unset_default)
      grob <- ggproto_parent(GeomPath, self)$draw_panel(
        data, panel_params, coord, arrow = arrow, lineend = lineend,
        linejoin = linejoin, linemitre = linemitre, na.rm = FALSE
      )
      return(grob)
    }

    # Sort on group
    data <- data[order(data$group), , drop = FALSE]
    munched <- coord_munch(coord, data, panel_params)

    # Drop lines with less than two datapoints, while preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group,
                       FUN = length)
    munched <- munched[rows >= 2, ]

    if ({n <- nrow(munched)} < 2) {
      return(zeroGrob())
    }

    # Use lines or segments?
    attr <- ggplot2:::dapply(munched, "group", function(df) {
      linetype <- unique(df$linetype)
      new_df(list(
        solid = identical(linetype, 1) || identical(linetype, "solid"),
        constant = nrow(unique(df[, c("alpha", "colour", "size", "linetype")])) == 1
      ), n = 1)
    })
    solid_lines <- all(attr$solid)
    constant <- all(attr$constant)
    if (!solid_lines && !constant) {
      stop("geom_path: If you are using dotted or dashed lines,",
           " colour, size and linetype must be constant over the line")
    }

    # Work out grouping variables for grobs
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <- c(group_diff, TRUE)
    i <- if (!constant) !end else start

    colour <- if (was_defaulted(munched$colour)) {
      lapply(which(start), function(i) {
        i <- rep(i, length.out = length(el$colour))
        alpha(el$colour, munched$alpha[i])
      })
    } else {
      colour <- as_grouped_variable(alpha(munched$colour, munched$alpha))
    }


    gp <- gpar(
      lwd = by_default(munched$size, el$size)[i],
      lty = by_default(munched$linetype, el$linetype)[i],
      lineend = el$lineend,
      linejoin = el$linejoin,
      linemitre = linemitre
    )

    id <- match(munched$group, unique(munched$group))
    element_grob(
      el,
      x = munched$x, y = munched$y, colour = colour,
      size = gp$lwd, linetype = gp$lty, lineend = lineend,
      default.units = "native", id = id
    )
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


## line -------------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname elementalist_extensions
GeomLineTheme <- ggproto(
  "GeomLineTheme", GeomPathTheme,
  extra_params = c("na.rm", "orientation"),
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },
  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- data[order(data$PANEL, data$group, data$x), ]
    flip_data(data, params$flipped_aes)
  }
)
