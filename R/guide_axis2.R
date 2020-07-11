#' Axis guides
#'
#' This is just an axis guide that produces the same thing as the regular one.
#' It is just differently parametrised to allow axis ticks to be controlled
#' by position modifying theme elements.
#'
#' @inheritParams ggplot2::guide_axis
#'
#' @return A \code{list} with the class \code{"guide"}.
#' @export
#'
#' @examples
#' NULL
guide_axis2 <- function(
  title = waiver(),
  check.overlap = FALSE,
  angle = NULL,
  n.dodge = 1,
  order = 0,
  position = waiver()
) {
  structure(list(
    title = title,

    check.overlap = check.overlap,
    angle = angle,
    n.dodge = n.dodge,

    order = order,
    position = position,
    available_aes = c("x", "y"),
    name = "axis"
  ),
  class = c("guide", "axis2", 'axis'))
}

#' @export
#' @noRd
guide_gengrob.axis2 <- function(guide, theme) {
  aesthetic <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]
  draw_axis2(
    break_positions = guide$key[[aesthetic]],
    break_labels = guide$key$.label,
    axis_position = guide$position,
    theme = theme,
    check.overlap = guide$check.overlap,
    angle = guide$angle,
    n.dodge = guide$n.dodge
  )
}

draw_axis2 <- function(
  break_positions,
  break_labels,
  axis_position,
  theme,
  check.overlap = FALSE,
  angle = NULL,
  n.dodge = 1
) {
  axis_position <- match.arg(substr(axis_position, 1, 1),
                             c("t", "b", "r", "l"))
  aes <- if (axis_position %in% c("t", "b")) "x" else "y"

  elements <- build_axis_elements(axis_position, angle, theme)

  params <- setup_axis_params(axis_position)
  line_grob <- build_axis_line(elements$line, params)

  if ({n_breaks <- length(break_positions)} == 0) {
    out <- grid::gTree(
      children = grid::gList(line_grob),
      width = grid::grobWidth(line_grob),
      height = grid::grobHeight(line_grob),
      cl = "absoluteGrob"
    )
    return(out)
  }

  label_grobs <- build_axis_labels(
    elements,
    labels = break_labels,
    position = break_positions,
    dodge = n.dodge, check.overlap = check.overlap, params = params
  )

  # Setup ticks
  sizes <- unit.c(elements$tick_length)
  tick_grob <- build_axis_ticks(elements$ticks, sizes,
                                break_positions, params)
  elements$tick_length <- max(sizes)

  assemble_axis_grobs(ticks = tick_grob, labels = label_grobs,
                      lines = line_grob, elements = elements,
                      params = params)
}

build_axis_elements <- function(axis_position = "b", angle = NULL, theme) {
  aesthetic <- if (axis_position %in% c("t", "b")) "x" else "y"
  axis_position <- match.arg(axis_position, c("top", "bottom", "left", "right"))

  element_names = c(line = "axis.line.", ticks = "axis.ticks.",
                    tick_length = "axis.ticks.length.", label = "axis.text.")
  element_names <- setNames(paste0(element_names, aesthetic, ".",
                                   axis_position),
                            names(element_names))
  elements <- lapply(element_names, calc_element, theme)

  if (inherits(elements$label, "element_text")) {
    lab_overrides <- .int$axis_label_element_overrides(axis_position, angle)
    elements$label$angle <- lab_overrides$angle %||% elements$label$angle
    elements$label$hjust <- lab_overrides$hjust %||% elements$label$hjust
    elements$label$vjust <- lab_overrides$vjust %||% elements$label$vjust
  }
  elements
}

build_axis_line <- function(element, params) {
  args <- list(element, unit(c(0, 1), "npc"), rep(params$pos, 2L))
  names(args) <- c("element", params$aes, params$non_aes)
  do.call(element_grob, args)
}

build_axis_labels <- function(
  elements, labels, position, dodge = 1, check.overlap = FALSE, params
) {
  n_breaks <- length(position)
  if (n_breaks == 0) {
    return(list(zeroGrob()))
  }

  # Validate labels
  if (is.list(labels)) {
    if (any(vapply(labels, is.language, logical(1)))) {
      labels <- do.call(expression, labels)
    } else {
      labels <- unlist(labels)
    }
  }

  dodge_pos <- rep(seq_len(dodge), length.out = n_breaks)
  dodge_idxs <- split(seq_len(n_breaks), dodge_pos)
  label_grobs <- lapply(dodge_idxs, function(idx) {
    .int$draw_axis_labels(
      break_positions = position[idx],
      break_labels = labels[idx],
      label_element = elements$label,
      is_vertical = params$vertical,
      check.overlap = check.overlap
    )
  })
}

build_axis_ticks <- function(element, length, position, params) {
  n_breaks <- length(position)
  pos <- unit(c(params$pos, params$pos + (params$tick_dir * 1)), "npc")
  pos <- rep(pos[params$tick_ord], times = n_breaks)

  args <- list(element, unit(rep(position, each = 2), "native"),
               pos, rep(2, times = n_breaks))
  names(args) <- c("element", params$aes, params$non_aes, "id.lengths")

  do.call(element_grob, args)
}

assemble_axis_grobs <- function(ticks, labels, lines, elements, params) {
  non_dims <- paste0(params$non_dim, "s")
  label_dims <- do.call(unit.c, lapply(labels, params$labels_measure))
  grobs <- c(list(ticks), labels)
  grob_dims <- unit.c(elements$tick_length, label_dims)

  if (params$labels_first) {
    grobs <- rev(grobs)
    grob_dims <- rev(grob_dims)
  }

  gt <- base::do.call(
    params$gtable_element,
    setNames(list("axis", grobs, grob_dims, unit(1, "npc")),
             c("name", "grobs", non_dims, params$dim))
  )

  # create viewport
  justvp <- base::do.call(
    grid::viewport,
    setNames(list(params$pos, params$gtable_measure(gt),
                  params$opposite_axis),
             c(params$non_aes, params$non_dim, "just"))
  )

  gTree(children = gList(lines, gt),
        width = gtable::gtable_width(gt),
        height = gtable::gtable_height(gt),
        xmin = NULL, ymin = NULL, vp = justvp,
        cl = "absoluteGrob")
}

setup_axis_params <- function(axis_position) {
  aesthetic <- if (axis_position %in% c("t", "b")) "x" else "y"

  # Set verticality parameters
  if (is_vertical <- axis_position %in% c("l", "r")) {
    position_dim <- "y"
    position_size <- "height"
    gtable_element <- gtable::gtable_row
    gtable_measure <- gtable::gtable_width
    measure_labels <- grid::grobWidth
  } else {
    position_dim <- "x"
    position_size <- "width"
    gtable_element <- gtable::gtable_col
    gtable_measure <- gtable::gtable_height
    measure_labels <- grid::grobHeight
  }
  non_position_dim <- setdiff(c("x", "y"), position_dim)
  non_position_size <- setdiff(c("width", "height"), position_size)

  # Set secondarity parameters
  if (is_second <- axis_position %in% c("r", "t")) {
    tick_direction <- 1
    non_position_panel <- 0
    tick_coord_order <- c(2, 1)
  } else {
    tick_direction <- -1
    non_position_panel <- 1
    tick_coord_order <- c(1, 2)
  }

  labels_first <- axis_position %in% c("l", "t")
  axis_opposite <- chartr("tblr", "btrl", axis_position)
  axis_opposite <- match.arg(axis_opposite, c("top", "bottom", "left", "right"))

  list(aes = aesthetic, non_aes = non_position_dim,
       dim = position_size, non_dim = non_position_size,
       gtable_element = gtable_element,
       gtable_measure = gtable_measure,
       labels_measure = measure_labels,
       tick_dir = tick_direction,
       tick_ord = tick_coord_order,
       pos = non_position_panel,
       labels_first = labels_first,
       opposite_axis = axis_opposite,
       vertical = is_vertical)
}
