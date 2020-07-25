.check_geom_element <- function(element, shape = "line") {
  if (is.null(element)) {
    return(element)
  }
  class <- paste0("element_", shape)
  if (!inherits(element, c(class, "element_blank"))) {
    stop("The `element` argument should be of type `", class, "`.",
         call. = FALSE)
  }
  element
}
