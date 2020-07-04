theme_registration <- function() {
  register_theme_elements(
    elementalist.geom_line = element_line(),
    elementalist.geom_rect = element_rect(),
    elementalist.geom_text = element_text(),
    element_tree = list(
      elementalist.geom_line = el_def("element_line", "line"),
      elementalist.geom_rect = el_def("element_rect", "rect"),
      elementalist.geom_text = el_def("element_text", "text")
    )
  )
}
