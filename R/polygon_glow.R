# Element grob constructor ------------------------------------------------

#' @export
#' @method element_grob element_polygon_glow
element_grob.element_polygon_glow <- function(
  element,
  x = c(0, 0.5, 1, 0.5),
  y = c(0.5, 1, 0.5, 0),
  colour = NULL, fill = NULL, linewidth = NULL,
  linetype = NULL, lineend = "round", linejoin = "round",
  id = NULL, pathId = NULL,
  id.lengths = NULL, pathId.lengths = NULL,
  default.units = "npc", rule = "evenodd",
  ...
) {
  fun_gp <- gpar(
    col = colour, fill = fill,
    lwd = check_zerolength(linewidth * .pt),
    lty = linetype, lineend = lineend, linejoin = linejoin
  )

  element_gp <- gpar(
    col = element$colour, fill = element$fill,
    lwd = check_zerolength(element$linewidth * .pt),
    lty = element$linetype,
    lineend = element$linetype, linejoin = element$linejoin
  )

  sub_id <- resolve_id(pathId, pathId.lengths, length(x))
  id <- resolve_id(id, id.lengths, length(x))

  gp <- modify_list(element_gp, fun_gp)

  poly <- polygon_glow(
    x = x, y = y,
    id = id, sub_id = sub_id,
    n = element$params$n, amount = element$params$amount,
    default.units = default.units, rule = rule,
    gp = gp, bg = TRUE, closed = TRUE,
    ...
  )

  if (all(poly$fg$gp$lty %in% c("solid", "1") | poly$fg$gp$lty == 1)) {
    class(poly$fg)[[1]] <- "polyline"
  }

  if (!is.null(poly$bg)) {
    return(grobTree(bg = poly$bg, fg = poly$fg))
  } else {
    return(poly$fg)
  }
}

# Style application -------------------------------------------------------

#' Apply glow style to polygons.
#'
#' Forms a grob graphical object that will draw polygons with a glow.
#'
#' @param x,y A \code{numeric} or \code{unit} object specifying x/y coordinates.
#' @param id,sub_id A \code{numeric} to separate locations into distinct paths
#'   or sub-paths. All coordinates with the same \code{id} belong to the same
#'   path or sub-path. These arguments are assumed to start at 1 and increase by
#'   exactly 1 at the start of a new path.
#' @param colour,fill A colour specification of either length 1 or of the same
#'   length as \code{length(unique(sub_id))}.
#' @param lwd A \code{numeric} specifying the line width of either length 1 or
#'   of the same length as \code{length(unique(sub_id))}.
#' @param lty A linetype specification of either length 1 or of the same length
#'   as \code{length(unique(sub_id))}.
#' @param n An \code{integer} for the number of times a line is to be repeated.
#'   Larger numbers look smoother but might also be somewhat heavier to compute.
#' @param amount A \code{numeric} of length 1 indicating the amount of glow to
#'   produce.
#' @param bg A \code{logical} of length 1. Whether to return the filled
#'   background of the polygon (default: \code{TRUE}).
#' @param closed A \code{logical} of length 1. Whether the polygon is to be
#'   closed (default: \code{FALSE}).
#' @param ... Other arguments passed on to the \code{grid::gpar()} function.
#'
#' @details The glowing effect is achieved by copying the polygon several times
#'   with increasing width and decreasing alpha.
#'
#'   Will make separate foreground (lines) and background (fills) grobs.
#'
#' @return A \code{list} with elements \code{fg} and \code{bg} which are grobs
#'   or \code{NULL}.
#' @export
#'
#' @examples
#' # Some example data
#' df <- data.frame(
#'   x = c(0, 0.5, 1, 0.5, 0.25, 0.5, 0.75, 0.5),
#'   y = c(0.5, 0, 0.5, 1, 0.5, 0.25, 0.5, 0.75),
#'   sub_id = rep(c(1, 2), each = 4),
#'   id = rep(1, each = 8)
#' )
#'
#' # Constructing polygon
#' pgon <- polygon_glow(
#'   df$x,  df$y,
#'   df$id, df$sub_id,
#'   colour = c("blue", "red"), lty = c(1, 4),
#'   fill = "grey50", n = 50, closed = TRUE, lwd = 3
#' )
#'
#' # Rendering in grid
#' grid.newpage()
#' grid.draw(pgon$bg)
#' grid.draw(pgon$fg)
polygon_glow <- function(
  x, y,
  id, sub_id = id,
  gp = gpar(),
  n = 10, amount = 3,
  bg = TRUE, closed = FALSE,
  rule = "evenodd",
  ...
) {
  colour <- gp$col %||% "black"
  lwd <- gp$lwd %||% 1
  lty <- gp$lty %||% "solid"

  if (bg && !is.null(gp$fill)) {
    bg <- pathGrob(
      x = x, y = y,
      id = id, pathId = sub_id, rule = rule,
      gp = do.call(gpar, within(unclass(gp), col <- NA)),
      ...
    )
  } else {
    bg <- NULL
  }

  if (amount == 0) {
    n <- 1
  }
  # Count unique groups
  ngroup <- length(unique(id))
  nsub <- split(id, sub_id)
  nsub <- vapply(nsub, length_uni, integer(1), USE.NAMES = FALSE)

  if (isTRUE(closed)) {
    # Rule: Repeat first observation before duplicating.
    dup_fun <- function(x) rep.int(c(x, x[1]), n)
  } else {
    # Rule: Duplicate observations.
    dup_fun <- function(x) rep.int(x, n)
  }

  # Build duplication index
  i <- split(seq_along(id), id)
  starts <- unlist(lapply(lengths(i), function(m) {
    rep.int(c(1L, rep.int(0L, m - 1L + isTRUE(closed))), n)
  }), recursive = FALSE, use.names = FALSE)
  i <- unlist(lapply(i, dup_fun), recursive = FALSE, use.names = FALSE)

  # Duplicate coordinates and groups
  x <- x[i]
  y <- y[i]
  original_id <- id[i]
  id <- cumsum(starts)

  # Apply a series of alpha to colours
  colour <- rep_len(rep.int(colour, nsub[seq_along(colour)]), ngroup)
  colour <- dilute_colour(colour, n)
  dim(colour) <- NULL

  # Generating series of increasing width, following upper Cauchy quantiles
  lwd <- rep_len(rep.int(lwd, nsub[seq_along(nsub)]), ngroup)
  # lwd <- rep_len(rep.int(lwd, nsub), ngroup)
  series <- seq(0.5, 0.975, length.out = n)
  lwd <- vapply(lwd, function(x) qcauchy(series, x, amount), series)
  dim(lwd) <- NULL

  # Repeat linetypes
  lty <- rep(rep_len(rep.int(lty, nsub[seq_along(lty)]), ngroup), each = n)
  # lty <- rep(rep_len(rep.int(lty, nsub), ngroup), each = n)

  # Build grob
  gp$col <- check_zerolength(colour)
  gp$lwd <- check_zerolength(lwd)
  gp$lty <- check_zerolength(lty)
  gp$fill <- NA
  fg <- polylineGrob(
    x = x, y = y, id = id,
    gp = gp, ...
  )

  class(fg)[[1]] <- "glowlineGrob"
  fg$original_id <- original_id
  list(fg = fg, bg = bg)
}

# Rendering ---------------------------------------------------------------

#' @export
#' @method makeContext glowlineGrob
makeContext.glowlineGrob <- function(x) {
  lty <- x$gp$lty
  class(x)[[1]] <- "polyline"

  # Early exit condition for default linetype
  if (is.null(lty) | all(lty == 1) | all(lty == "1") | all(lty == "solid")) {
    x$orig_id <- NULL
    return(x)
  }

  # Extract grouping information
  id_split <- split(x$id, x$original_id)
  origin_group <- vapply(id_split, min, integer(1), USE.NAMES = FALSE)
  group_rle <- rle(x$original_id[!duplicated(x$id)])
  group_len <- group_rle$lengths

  # Extract and split coordinates to groups
  new_x <- split(convertX(x$x, "mm", valueOnly = TRUE), x$id)
  new_y <- split(convertY(x$y, "mm", valueOnly = TRUE), x$id)

  # Convert linetype to dash-gap parametrisation
  lty <- mapply(interpret_linetype,
                lty = x$gp$lty[origin_group],
                lwd = x$gp$lwd[origin_group],
                SIMPLIFY = FALSE)
  dashes <- lapply(lty, `[[`, "dash")
  gaps   <- lapply(lty, `[[`, "gap")

  # Break up lines into line-pieces
  newcoords <- mapply(fracture_linetype,
                      x = new_x[origin_group],
                      y = new_y[origin_group],
                      dash = dashes, gap = gaps,
                      SIMPLIFY = FALSE, USE.NAMES = FALSE)
  newcoords <- matrix(unlist(newcoords, recursive = FALSE), ncol = length(group_len))

  # Calculate graphical parameter lengths
  lengs  <- vapply(newcoords[3, ], sum, double(1))
  lengs  <- rep(lengs, times = group_len)

  # Repeat and bind coordinates for every glowline
  newcoords <- apply(newcoords, 1, function(x) {
    do.call("c", rep(x, times = group_len))
  })

  x$gp$col <- rep(x$gp$col, lengs)
  x$gp$lwd <- rep(x$gp$lwd, lengs)
  x$gp$lty <- NULL

  x$x <- unit(newcoords[, 1], "mm")
  x$y <- unit(newcoords[, 2], "mm")
  x$id <- cumsum(newcoords[, 3])

  x
}
