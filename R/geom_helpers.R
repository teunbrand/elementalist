exchange_defaults <- function(data, type = "line", element) {
  if (inherits(element, "element_blank")) {
    data[] <- lapply(data, unset_default)
    return(data)
  }

  data <- as.list(data)
  is_line <- type == "line"
  is_poly <- type == "polygon"
  if (is_poly) {
    first <- which(!duplicated(data$group))
  }

  # Colour
  if ("colour" %in% names(data)) {
    if (was_defaulted(data$colour)) {
      data$colour <- element$colour
    } else {
      if (is_line) {
        data$colour <- as_grouped_variable(alpha(data$colour, data$alpha))
      } else if (is_poly) {
        data$colour <- data$colour[first]
      } else {
        data$colour <- as_grouped_variable(data$colour)
      }
    }
  }

  # Fill
  if ("fill" %in% names(data)) {
    if (was_defaulted(data$fill)) {
      data$fill <- element$fill
    } else {
      if (!is_line && !is_poly) {
        data$fill <- as_grouped_variable(alpha(data$fill, data$alpha))
      } else if (is_poly) {
        data$fill <- alpha(data$fill, data$alpha)[first]
      } else {
        data$fill <- as_grouped_variable(data$fill)
      }
    }
  }

  # Size
  if ("size" %in% names(data)) {
    if (was_defaulted(data$size)) {
      data$size <- element$size
    } else {
      if (is_poly) {
        data$size <- data$size[first]
      } else {
        data$size <- as_grouped_variable(data$size)
      }
    }
  }

  # Linetype
  if ("linetype" %in% names(data)) {
    if (was_defaulted(data$linetype)) {
      data$linetype <- element$linetype
    } else {
      if (is_poly) {
        data$linetype <- data$linetype[first]
      } else {
        data$linetype <- as_grouped_variable(data$linetype)
      }
    }
  }

  checked <- c("colour", "fill", "size", "linetype")
  not_checked <- setdiff(names(data), checked)
  data[not_checked] <- lapply(data[not_checked], unset_default)

  return(data)
}


