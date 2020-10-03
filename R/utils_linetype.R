#' Convert linetype parameters to mm
#'
#' Translates character or integers to linetype dash lengths and gap lengths.
#'
#' @param lty A linetype parameter of length 1.
#' @param lwd A \code{numeric} noting linewidth.
#' @param mult A \code{numeric} multiplier. Through trial and error it seems
#'   0.2646 is reasonable, though I don't know why exactly.
#'
#' @return A \code{list} with the elements named \code{dash} and \code{gap} that
#'   give dash and gap lengths in millimetres.
#'
#' @noRd
#' @keywords internal
#' @examples
#' interpret_linetype(3)
#' interpret_linetype("F282")
interpret_linetype <- function(lty, lwd = 1, mult = 0.2646) {
  if (is.numeric(lty) && lty < 12) {
    lty <- as.integer(lty)
    types <- c("solid", "44", "13", "1343", "73", "2262",
               "12223242", "F282", "F4448444", "224282F2", "F1")
    lty <- types[lty]
  }
  lty <- as.character(lty)
  if (lty == "solid") {
    return(list(dash = 1, gap = 0))
  }

  chars <- strsplit(lty, "")[[1]]
  if (length(chars) == 1) {
    return(list(dash = 1, gap = 0))
  }

  chars <- strtoi(chars, 16) * lwd * mult

  id <- seq_along(chars)
  dash <- chars[(id %% 2) == 1]
  gap  <- chars[(id %% 2) == 0]
  if (length(gap) != length(dash) || length(gap) > 8) {
    stop("Invalid line type: must be length 2, 4, 6 or 8.",
         call. = FALSE)
  }
  list(dash = dash, gap = gap)
}

#' Apply dashes and gaps to a line.
#'
#' @param x,y A \code{numeric} with absolute x and y coordinates. Typically in
#'   millimetres.
#' @param dash A \code{numeric} for dash lengths.
#' @param gap A \code{numeric} for gap lengths.
#'
#' @return A \code{list} with the elements \code{x}, \code{y} and \code{id}.
#'
#' @details \code{id}s are returned wherein first values are 1 and the rest 0.
#'   This makes it easier to later \code{cumsum()} over the IDs.
#'
#' @noRd
#' @keywords internal
#' @examples
#' fracture_linetype(c(0, 10), c(0, 10), dash = 1, gap = 1)
fracture_linetype <- function(x, y,
                              dash = 1, gap = 0) {
  # Early exit
  if (all(gap == 0)) {
    return(list(x = x, y = y, id = c(1L, rep(0L, length(x) - 1L))))
  }

  replen <- sum(dash) + sum(gap)
  tolerance <- 1000 * .Machine$double.eps

  # Express linepieces as cumulative distances
  dist <- sqrt(diff(x)^2 + diff(y)^2)
  keep <- dist > tolerance
  x <- x[c(TRUE, keep)]
  y <- y[c(TRUE, keep)]
  dist <- dist[keep]
  cumdist <- cumsum(c(0, dist))
  maxdist <- max(cumdist)

  # Take new lines as points along the cumulative distances
  starts <- seq(0, maxdist, by = replen)
  ends   <- pmin(starts + dash[[1]], maxdist)
  if (length(dash) > 1L) {
    this_dash <- 0
    for (i in seq_along(dash)[-1]) {
      this_dash <- sum(dash[i - 1], this_dash)
      altstarts <- seq(this_dash + gap[[i]], maxdist, by = replen)
      altends   <- pmin(altstarts + dash[[i]], maxdist)
      starts <- sort(c(starts, altstarts))
      ends   <- sort(c(ends, altends))
    }
  }

  # Dealing with elbow pieces
  # Find mismatches between original line-pieces where start or end fall on
  start_id <- findInterval(starts, cumdist)
  end_id   <- findInterval(ends, cumdist)
  is_mismatch <- which(start_id != end_id)

  # Insert elbow pieces
  missing_pieces <- lapply(is_mismatch, function(i) {
    seq(start_id[i] + 1L, end_id[i])
  })
  n_missing <- lengths(missing_pieces)
  missing_pieces <- cumdist[unlist(missing_pieces, FALSE, FALSE)]
  starts <- sort(c(starts, missing_pieces))
  ends   <- sort(c(ends,   missing_pieces))

  # Join elbow pieces through ids
  new_id <- seq_along(start_id)
  if (length(is_mismatch)) {
    i <- rep_len(1, length(new_id))
    i[is_mismatch] <- n_missing + 1
    new_id <- rep(new_id, i)
  }
  new_id <- rbind(new_id, new_id)
  dim(new_id) <- NULL
  new_id <- c(1L, diff(new_id))

  # Interpolate x
  xfun <- approxfun(cumdist, x)
  new_x <- rbind(xfun(starts), xfun(ends))
  dim(new_x) <- NULL

  # Interpolate y
  yfun <- approxfun(cumdist, y)
  new_y <- rbind(yfun(starts), yfun(ends))
  dim(new_y) <- NULL

  list(x = new_x, y = new_y, id = new_id)
}
