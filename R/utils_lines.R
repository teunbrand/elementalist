#' Fitting a pattern along a line.
#'
#' @param x A \code{numeric} vector of baseline x-coordinates; of the same
#'   length as the \code{y} argument.
#' @param y A \code{numeric} vector of baseline y-coordinates; of the same
#'   length as the \code{x} argument.
#' @param z A numeric vector with a pattern to fit to the xy coordinates.
#'
#' @return A \code{list} with elements \code{x} and \code{y} representing the
#'   new coordinates.
#'
#' @keywords internal
#' @noRd
#' @examples
#' fit_along(x = 0:1, y = 0:1, z = runif(100))
fit_along <- function(x, y, z) {
  xy <- matrix(c(x, y), nrow = {nxy <- length(x)})
  dxy <- xy[-nxy, , drop = FALSE] - xy[-1, , drop = FALSE]
  xy_len <- sqrt(rowSums(dxy^2))
  xy_ang <- atan2(dxy[, 2], dxy[, 1])

  z <- matrix(c(seq_along(z), z), {nz <- length(z)})
  pts <- seq(1, nz, length.out = nxy)
  dz <- z[pts, , drop = FALSE][-nxy, , drop = FALSE]
  dz <- dz - z[pts, , drop = FALSE][-1, , drop = FALSE]
  z_len <- sqrt(rowSums(dz^2))
  z_ang <- atan2(dz[, 2], dz[, 1])

  scale <- xy_len / z_len
  angle <- xy_ang - z_ang

  scale <- outer(diag(2), scale)
  angle <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)),
                  ncol = 4)

  M <- vapply(seq_len(nxy - 1), function(i) {
    scale[,,i] %*% matrix(angle[i,], 2)
  }, diag(2))

  out <- z
  j <- 0
  for (i in seq_len(nxy - 1)) {
    lii <- length({ii <- (pts[i] + j):pts[i + 1]})
    tmp <- z[ii, ]
    # translate to (0,0)
    tmp <- tmp - rep(z[ii[1] - j,  ], each = lii)
    # rotate and scale
    tmp <- t(tcrossprod(M[, , i], tmp))
    # translate back
    out[ii, ] <- tmp + rep(xy[i, ], each = lii)
    j <- max(j, 1)
  }
  return(list(x = out[, 1], y = out[, 2]))
}

#' Apply a function to a line
#'
#' @param fun A \code{function} to apply to the line.
#' @param x A \code{numeric} with x coordinates.
#' @param y A \code{numeric} with y coordinates.
#' @param colour A \code{character} or \code{list} with colours.
#' @param id An \code{integer} indicating which coordinates for a line together.
#' @param default.units A \code{character} noting the default \code{grid::unit}.
#' @param n An \code{integer} with how many line-segments to interpolate
#'   (if applicable).
#'
#' @return A \code{list} with modified values for coordinates, colours and IDs.
#'
#' @details Will probably return \code{NA} as last colour in a sequence, because
#' it assumes it will be parametrised as segments.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' apply_lines(wiggle(), colour = c("black", "white"))
apply_lines <- function(
  fun = defer,
  x = 0:1, y = 0:1,
  colour = NULL,
  size = NULL,
  id = rep(1, length(x)),
  default.units = "npc",
  n = 10
) {
  # Split data into chunks
  seq <- seq_along(x)
  seq <- unname(split(seq, id))
  x  <- lapply(seq, inv_subset, x = x)
  y  <- lapply(seq, inv_subset, x = y)
  colour <- grouped_variable(colour, seq)
  size <- grouped_variable(size, seq)
  id <- lapply(seq, inv_subset, x = id)

  # Apply function to data
  outcome <- mapply(fun, x = x, y = y,
                    colour = colour, size = size,
                    id = id, n = n, SIMPLIFY = FALSE)
  # Stitch data back together
  outcome <- lapply(
    setNames(nm = names(outcome[[1]])),
    function(i) {
      stitch(lapply(outcome, `[[`, i))
    })

  # Apply some formatting
  x <- inherit_unit(outcome$x, x, default.units)
  y <- inherit_unit(outcome$y, y, default.units)
  if (all(c("dx", "dy") %in% names(outcome))) {
    x <- x + unit(outcome$dx, "cm")
    y <- y + unit(outcome$dy, "cm")
  }
  list(x = x, y = y, id = outcome$id,
       size = outcome$lwd, colour = outcome$col,
       sub_id = outcome$sub_id)
}

#' Build a polyline or segments grob
#'
#' This function decides which one is more appropriate.
#'
#' @param x A \code{numeric} with x coordinates.
#' @param y A \code{numeric} with y coordinates.
#' @param id An \code{integer} indicating which coordinates belong together in
#' the same line.
#' @param gp A \code{grid::gpar()} object.
#' @inheritDotParams grid::polylineGrob
#' @inheritDotParams grid::segmentsGrob
#'
#' @return Either an S3 \code{polyline} or S3 \code{segments} grob.
#' @noRd
#' @keywords internal
#'
#' @examples
#' # One colour means polylinegrob
#' decide_linegrob(x = 1:4, y = 1:4, id = c(1,1,2,2),
#'                 gp = gpar(col = c("blue")))
#'
#' # If colours match up with id, it mean segmentsgrob
#' decide_linegrob(x = 1:4, y = 1:4, id = c(1,1,2,2),
#'                 gp = gpar(col = c("blue", "red", "green", "black")))
decide_linegrob <- function(x, y, id, gp = gpar(), ...) {
  if (length({col <- gp$col}) > 2) {
    if (length(id) > length(col)) {
      gp$col <- check_zerolength(col[!is.na(col)])
      polyline <- TRUE
    } else {
      nunique <- vapply(split(col, id), function(x) {
        sum(!is.na(unique(x)))
      },
      integer(1))
      if (all(nunique == 1)) {
        gp$col <- gp$col[!duplicated(id)]
        gp$lwd <- gp$lwd[!duplicated(id)]
        polyline <- TRUE
      } else {
        polyline <- FALSE
      }
    }
  } else {
    polyline <- TRUE
  }

  if (polyline) {
    polylineGrob(
      x = x, y = y,
      id = id, gp = gp,
      ...
    )
  } else {
    groups <- which(id[-1] != id[-length(id)])
    ends   <- c(groups, length(id))
    starts <- c(1, groups + 1)
    gp$col <- check_zerolength(gp$col[-ends])
    segmentsGrob(
      x0 = x[-ends], x1 = x[-starts],
      y0 = y[-ends], y1 = y[-starts],
      gp = gp, ...
    )
  }
}

grouped_variable <- function(var, seq) {
  if (length(var) != 1) {
    if (inherits(var, "grouped_variable")) {
      # If it is a grouped colour, it probably comes from a geom
      # so we can expect it to be of equal length as the x or y
      var <- lapply(seq, inv_subset, x = var)
      var <- lapply(var, unclass)
      var <- lapply(var, `[`, 1)
    }
    if (!is.list(var)) {
      var <- rep(list(var), length(seq))
    }
  }
  return(var)
}
