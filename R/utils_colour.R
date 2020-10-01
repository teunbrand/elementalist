#' Alpha extractor
#'
#' Takes the alpha component from a hexidecimal notation of colours.
#'
#' @param colour A \code{character} with colours in hexadecimal notation.
#'
#' @return A \code{numeric} between 0 and 1.
#'
#' @keywords internal
#' @noRd
#' @examples
#' extract_alpha("#FF000088")
extract_alpha <- function(colour) {
  if (all(nchar(colour) != 9)) {
    return(rep(1, length(colour)))
  }
  colour <- substr(colour, 8, 9)
  colour[nchar(colour) != 2] <- "FF"
  as.integer(as.hexmode(colour)) / 255
}


#' Calculate alpha dilution.
#'
#' Takes a starting alpha and makes a series of dilutions that will decrease the
#' cumulative alpha by a linear amount.
#'
#' @param alpha A \code{numeric} between 0 and 1 as starting value for alpha.
#' @param n An \code{integer} with the number of dilution steps.
#'
#' @return A \code{numeric} of length \code{n}
#'
#' @details The real thing decreasing is the transmissivity, which is \eqn{1 -
#'   alpha}. Transmissivity is multiplicative.
#'
#' @noRd
#' @keywords internal
#' @examples
#' # Dilute alpha
#' new_alpha <- dilute_alpha(0.9, 9)
#'
#' # Calculating percieved alpha
#' rev(1 - cumprod(1 - rev(new_alpha)))
dilute_alpha <- function(alpha, n) {
  alpha <- 1 - seq(alpha, 0, length.out = n + 1)[-(n + 1)]
  1 - c(alpha[-n] / alpha[-1], alpha[n])
}

#' Calculate colour dilution
#'
#' Dilutes colours by decreasing the alpha at each step.
#'
#' @param colour A vector of colour representations.
#' @param n An \code{integer} with the number of dilution steps.
#'
#' @return A \code{character matrix} of dimensions \eqn{n * length(colour)} with
#'   colours in hexadecimal notation.
#'
#' @noRd
#' @keywords internal
#' @examples
#' dilute_colour(c("red", "#FF00FF88"), 10)
dilute_colour <- function(colour, n) {
  colour <- alpha(colour, NA)
  alpha  <- extract_alpha(colour)
  colour <- substr(colour, 1, 7)
  colour <- rep(colour, each = n)
  alpha  <- vapply(alpha, dilute_alpha, numeric(n), n)
  matrix(alpha(colour, alpha), nrow = n)
}

# To interpolate colours
#' @importFrom scales colour_ramp
col_interpol <- function(col, n) {
  if (is.null(col)) {
    return(NULL)
  }
  if (length(col) < 2 || n < 2) {
    return(col)
  }
  seq <- seq(0, 1, length.out = n)
  scales::colour_ramp(col)(seq)
}


