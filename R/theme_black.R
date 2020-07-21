#' Black theme
#'
#' This theme makes all background elements black and all foreground elements
#' a colour of choice. Intermediate elements, such as strip backgrounds and grid
#' lines are set to a very transparent version of the foreground color.
#'
#' @inheritParams ggplot2::theme_gray
#' @param base_colour base colour for foreground elements
#'
#' @return A \code{theme} object that can be added to a plot.
#' @export
#'
#' @examples
#' mtcars2 <- within(mtcars, {
#'   vs <- factor(vs, labels = c("V-shaped", "Straight"))
#'   am <- factor(am, labels = c("Automatic", "Manual"))
#'   cyl  <- factor(cyl)
#'   gear <- factor(gear)
#' })
#'
#' p <- ggplot(mtcars2) +
#'   geom_point(aes(x = wt, y = mpg, colour = gear)) +
#'   labs(
#'     title = "Fuel economy declines as weight increases",
#'     subtitle = "(1973-74)",
#'     caption = "Data from the 1974 Motor Trend US magazine.",
#'     tag = "Figure 1",
#'     x = "Weight (1000 lbs)",
#'     y = "Fuel economy (mpg)",
#'     colour = "Gears"
#'   ) +
#'   facet_grid(vs ~ am)
#'
#' p + theme_black()
#'
#' # Oscilloscope / Radar version
#' p + theme_black(base_colour = "green")
#'
#' # Red alert version
#' p + theme_black(base_colour = "red")
theme_black <- function(base_size = 11, base_family = "",
                        base_line_size = base_size / 22,
                        base_rect_size = base_size / 22,
                        base_colour = "white") {
  half_line <- base_size / 2

  t <- theme(
    line =
      element_line(
        colour = base_colour, size = base_line_size,
        linetype = 1, lineend = "butt"
      ),
    rect =
      element_rect(
        fill = "black", colour = base_colour,
        size = base_rect_size, linetype = 1
      ),
    text =
      element_text(
        family = base_family, face = "plain",
        colour = base_colour, size = base_size,
        lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
        margin = margin(), debug = FALSE
      ),
    axis.line = element_line(colour = alpha(base_colour, 0.9)),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = element_text(size = rel(0.8), colour = alpha(base_colour, 0.9)),
    axis.text.x =
      element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top =
      element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =
      element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right =
      element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks = element_line(colour = alpha(base_colour, 0.9)),
    axis.ticks.length = unit(half_line / 2, 'pt'),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x =
      element_text(margin = margin(t = half_line / 2), vjust = 1),
    axis.title.x.top =
      element_text(margin = margin(b = half_line / 2), vjust = 0),
    axis.title.y =
      element_text(margin = margin(r = half_line / 2), vjust = 1, angle = 90),
    axis.title.y.right =
      element_text(margin = margin(l = half_line / 2), vjust = 0, angle = -90),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.spacing = unit(2 * half_line, 'pt'),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = margin(half_line, half_line, half_line, half_line),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.key.size = unit(1.2, 'lines'),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(2 * half_line, "pt"),
    panel.background = element_rect(fill = NA, colour = alpha(base_colour, 0.02)),
    panel.border = element_blank(),
    panel.grid = element_line(colour = alpha(base_colour, 0.05)),
    panel.grid.minor = element_line(size = rel(0.5)),
    panel.spacing = unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = element_rect(fill = alpha(base_colour, 0.05), colour = NA),
    strip.text =
      element_text(
        colour = alpha(base_colour, 0.9), size = rel(0.8),
        margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
      ),
    strip.text.x = NULL,
    strip.text.y = element_text(angle = -90),
    strip.text.y.left = element_text(angle = 90),
    strip.placement = "outside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(half_line / 2, "pt"),
    plot.background = element_rect(fill = "black", colour = NA),
    plot.title =
      element_text(
        size = rel(1.2), hjust = 0, vjust = 1, margin = margin(b = half_line)
      ),
    plot.title.position = "panel",
    plot.subtitle =
      element_text(
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
    plot.caption =
      element_text(
        size = rel(0.8), hjust = 1, vjust = 1, margin = margin(t = half_line)
      ),
    plot.tag = element_text(size = rel(1.2), hjust = 0.5, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = margin(half_line, half_line, half_line, half_line),
    elementalist.geom_line =
      element_line(colour = base_colour),
    elementalist.geom_rect =
      element_rect(colour = base_colour, fill = alpha(base_colour, 0.5)),
    complete = TRUE
  )
}

ggplot(mpg, aes(displ, cty)) + geom_point(aes(colour = manufacturer)) +
  facet_grid(vars(drv), vars(cyl)) +
  theme_black()
