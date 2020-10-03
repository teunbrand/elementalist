theme_registration <- function() {
  register_theme_elements(
    elementalist.polygon = element_polygon(
      fill = "white", colour = "black", size = 0.5, linetype = 1,
      inherit.blank = TRUE, linejoin = "round", lineend = "round"
    ),
    elementalist.geom_polygon = element_polygon(),
    elementalist.geom_line = element_line(),
    elementalist.geom_rect = element_rect(),
    elementalist.geom_text = element_text(),
    element_tree = list(
      elementalist.polygon = el_def("element_polygon"),
      elementalist.geom_polygon = el_def("element_polygon",
                                         "elementalist.polygon"),
      elementalist.geom_line = el_def("element_line", "line"),
      elementalist.geom_rect = el_def("element_rect", "rect"),
      elementalist.geom_text = el_def("element_text", "text")
    )
  )
}
