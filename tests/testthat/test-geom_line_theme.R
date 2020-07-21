
test_that('geom_line_theme defaults to geom_line when no theme is set', {
  base <- ggplot(pressure, aes(temperature, pressure))
  case <- base + geom_line_theme()
  ctrl <- base + geom_line()
  case <- layer_grob(case, 1)[[1]]
  ctrl <- layer_grob(ctrl, 1)[[1]]
  case$name <- NULL
  ctrl$name <- NULL
  expect_identical(case, ctrl)
})

test_that("geom_line_theme can change line appearance", {
  base  <- ggplot(pressure, aes(temperature, pressure)) + geom_line_theme()
  case1 <- base + theme(elementalist.geom_line = element_line_wiggle())
  case2 <- base + theme(elementalist.geom_line = element_line_multicolour())
  ctrl  <- base + theme(elementalist.geom_line = element_line_seq())

  cases <- list(case1, case2, ctrl)
  cases <- lapply(cases, ggplotGrob)
  cases <- lapply(cases, function(x) {
    is_panel <- grep("panel", x$layout$name)
    panel <- x$grobs[[is_panel]]
    grob <- which(vapply(panel$children, inherits, logical(1), "polyline"))
    if (length(grob) != 1) {
      grob <- which(vapply(panel$children, inherits, logical(1), "segments"))
    }
    panel$children[[grob]]
  })
  expect_length(cases[[1]]$x, 901)
  expect_length(cases[[1]]$gp$col, 2)

  expect_length(cases[[2]]$x0, 918)
  expect_length(cases[[2]]$gp$col, 918)

  expect_length(cases[[3]]$x, 19)
  expect_length(cases[[3]]$gp$col, 1)
})
