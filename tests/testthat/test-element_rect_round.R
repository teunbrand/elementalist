test_that("element_rect_round returns correct class", {
  el <- element_rect_round(radius = 0.5)
  expect_s3_class(el, "element_rect_round")
  expect_s3_class(el, "element_rect")
  expect_s3_class(el, "element")
})

test_that("element_rect_round converts numeric radius", {
  el_ctrl <- element_rect_round(radius = unit(0.5, "snpc"))
  el_case <- element_rect_round(radius = 0.5)
  expect_identical(el_ctrl$radius, el_case$radius)
})

test_that("element_rect_round generates grobs", {
  el <- element_rect_round(colour = "purple", size = 2, radius = 0.4)
  grob <- element_grob(el, width = 0.4, height = 0.4)
  expect_s3_class(grob, "gTree")
  expect_length(grob$children, 1)
  expect_s3_class(grob$children[[1]], "roundrect")
})

test_that("element_rect_round can be used with the theme system", {
  ctrl <- ggplot(mpg, aes(displ, cty)) + geom_point()
  case <- ctrl + theme(panel.background = element_rect_round())

  ctrl <- ggplotGrob(ctrl)
  case <- ggplotGrob(case)

  is_panel <- grep("^panel$", ctrl$layout$name)
  ctrl <- ctrl$grobs[[is_panel]]$children[[1]]$children
  case <- case$grobs[[is_panel]]$children[[1]]$children

  is_background <- grep("^panel.background", names(ctrl))
  ctrl <- ctrl[[is_background]]
  case <- case[[is_background]]

  expect_s3_class(ctrl, "rect")
  expect_s3_class(case$children[[1]], "roundrect")
})

test_that("element_rect_round can be used with thematic geoms", {
  ctrl <- ggplot(mpg, aes(class))
  case <- ctrl + geom_bar_theme(element = element_rect_round())
  ctrl <- ctrl + geom_bar()

  ctrl <- layer_grob(ctrl)[[1]]
  case <- layer_grob(case)[[1]]

  expect_s3_class(ctrl, "rect")
  expect_s3_class(case$children[[1]], "roundrect")
})
