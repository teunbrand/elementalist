# User function -----------------------------------------------------------

#' Interpolated rectangle theme element.
#'
#' Interpolates the sides of a rectangle.
#'
#' @inheritParams element_line_seq
#' @param sides A \code{character} of length one containing any of \code{"t"},
#'   \code{"l"}, \code{"b"} and/or \code{"r"}. If these letters are present,
#'   edges are drawn at the top (t), left (l), bottom (b) or right (r),
#'   respectively. Including all these letters will draw all rectangle edges
#'   (default), whereas including none of these letters will not draw edges.
#'
#' @return A \code{list} of the class \code{element_line_seq}
#' @export
#'
#' @examples
#' element_rect_seq()
element_rect_seq <- function(
  fill = NULL,
  colour = NULL,
  size = NULL,
  linetype = NULL,
  color = NULL,
  inherit.blank = FALSE,
  fun = defer,
  n = 50,
  sides = "tblr"
) {
  if (!is.null(color)) {
    colour <- color
  }
  # Check which sides are in the argument.
  # I don't really understand the regex part, so kind of surfing the
  # StackOverflow wave here. But hey! Seems to work!
  sides <- c(
    top    = grepl("(?=.*t)", sides, perl = TRUE),
    left   = grepl("(?=.*l)", sides, perl = TRUE),
    bottom = grepl("(?=.*b)", sides, perl = TRUE),
    right  = grepl("(?=.*r)", sides, perl = TRUE)
  )

  structure(
    list(
      fill = fill,
      colour = colour,
      size = size,
      linetype = linetype,
      inherit.blank = inherit.blank,
      fun = fun,
      n = n,
      sides = sides
    ),
    class = c("element_rect_seq", "element_rect", "element")
  )
}

# Grob generator ----------------------------------------------------------

#' @rdname element_rect_seq
#' @usage NULL
#' @format NULL
#' @keywords internal
#' @export
element_grob.element_rect_seq <- function(
  element,
  x = 0.5, y = 0.5,
  width = 1, height = 1,
  fill = NULL, colour = NULL, size = NULL,
  linetype = NULL, lineend = "butt", default.units = "npc",
  fun = NULL, ...
) {
  fun_gp <- gpar(
    col = colour, fill = fill,
    lwd = check_zerolength(size * .pt),
    lty = linetype,
    lineend = lineend
  )
  element_gp <- gpar(
    col = element$colour,
    fill = element$fill,
    lwd = check_zerolength(element$size * .pt),
    lty = element$linetype,
    lineend = lineend
  )
  rectseqGrob(
    x, y,
    width, height,
    default.units = default.units,
    gp = modify_list(element_gp, fun_gp),
    n = element$n,
    fun = element$fun,
    sides = element$sides,
    ...
  )
}

# Grob --------------------------------------------------------------------

#' @keywords internal
rectseqGrob <- function(
  x = 0.5, y = 0.5,
  width = 1, height = 1,
  just = "center", hjust = NULL, vjust = NULL,
  default.units = "npc", name = NULL,
  gp = gpar(), vp = NULL, n = 10, fun = defer,
  sides = c(top = TRUE, left = TRUE, right = TRUE, bottom = TRUE)
) {

  poly <- rect_as_polygon(x, y, width, height, just, hjust, vjust)

  # Parameterise as segments
  seg <- split(poly, poly$id)
  # Take top, left, bottom, right vertex pairs
  seg <- lapply(seg, function(x) {x[c(3, 4, 4, 1, 1, 2, 2, 3),]})
  seg <- do.call(rbind, seg)
  fake_id <- rep(seq_len(nrow(seg)/2), each = 2) * 2
  part <- rep(rep(c("top", "left", "bottom", "right"), each = 2),
              length.out = nrow(seg))

  # Make prototype
  proto <- apply_lines(fun, seg$x, seg$y, gp$col, fake_id, default.units, n)

  proto_id <- seg$id[proto$id]
  gp_rect <- gp
  gp_rect$col <- proto$colour[!duplicated(proto_id)]

  # Make prototype into rectangle-polygon
  rect <- polygonGrob(
    x = proto$x, y = proto$y, id = proto_id,
    name = name,
    default.units = default.units, gp = gp_rect, vp = vp
  )

  # Check for uniform colour
  colour <- proto$colour
  unicol <- length(colour) == length(proto_id)
  if (unicol) {
    unicol <- all(vapply(split(colour, proto_id), function(x) {
      sum(!is.na(unique(x)))}, numeric(1)
    ) == 1)
  }

  # If we need to keep all sides, just return as-is
  if (all(sides) & unicol) {
    return(rect)
  } else {
    # Otherwise remove the colours
    rect$gp$col <- NA
    # If no sides, then return without colour
    if (!any(sides)) {
      return(rect)
    }
  }

  # Make prototype into edges-polyline (already is polyline)
  edges <- proto
  edges$name <- paste0(rect$name, ".", sample(1000, 1))

  # Figure out what sides to keep
  sides <- unname(sides[part])
  keep <- sides[edges$id]

  # Subset edges
  edges$x <- edges$x[keep]
  edges$y <- edges$y[keep]
  if (length(edges$id) == length(edges$colour)) {
    edges$gp[] <- lapply(edges$gp, function(x) {
      if (length(x) == 1) {
        return(x)
      } else {
        return(x[keep])
      }
    })
  }
  edges$id <- edges$id[keep]
  gp$col <- check_zerolength(edges$colour)

  edges <- decide_linegrob(
    x = edges$x, y = edges$y, id = edges$id, gp = gp,
    default.units = default.units, arrow = NULL,
    name = edges$name, vp = vp
  )

  grob <- grobTree(
    rect, edges,
    name = paste0("rectseq", ".", sample(1000, 1))
  )

  return(grob)
}

# Helpers -----------------------------------------------------------------

# Note that this creates an open polygon that doesn't close the last two points.
# It is assumed that grid::polygonGrob takes care of this automatically.
rect_as_polygon <- function(x, y, width, height,
                            just = "center", hjust = NULL, vjust = NULL) {
  # Set justifications
  hjust = resolveHJust(just, hjust)
  vjust = resolveVJust(just, vjust)

  # Keep track of separate rectangles and vertices
  id <- seq_along(x)
  vrtx <- rep(1:4, length(id))

  # Choose concatenation strategy for x
  concat <- if (is.unit(x) | is.unit(width)) {
    unit.c
  } else base::c

  # Convert to polygon coordinates
  # Left, right, right, left
  x <- concat(x - hjust * width,
              x + (1 - hjust) * width,
              x + (1 - hjust) * width,
              x - hjust * width)

  # Choose concatenation strategy for y
  concat <- if (is.unit(y) | is.unit(height)) {
    unit.c
  } else base::c

  # Convert to polygon coordinates
  # Bottom, bottom, top, top
  y <- concat(y - vjust * height,
              y - vjust * height,
              y + (1 - vjust) * height,
              y + (1 - vjust) * height)

  # Match ids to expanded coordinates
  id <- rep(id, each = 4)

  new_df(list(x = x, y = y, id = id, vrtx = vrtx))
}
