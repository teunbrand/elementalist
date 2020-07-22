test_that("geom_line_theme can change line appearance", {
  base  <- ggplot(pressure, aes(temperature, pressure)) + geom_path_theme()
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

test_that("geom_path_theme child elements inherits from theme", {
  test  <- ggplot(pressure, aes(temperature, pressure)) +
    geom_path_theme(element = element_line_seq(colour = "blue")) +
    theme(
      elementalist.geom_line = element_line_seq(linetype = 2)
    )
  gt <- ggplotGrob(test)
  gt <- gt$grobs[[grep("panel", gt$layout$name)]]
  gt <- gt$children[vapply(gt$children, inherits, logical(1), "polyline")][[1]]

  expect_equal(gt$gp$col, c("blue"))
  expect_equal(gt$gp$lty, c(2))
})

test_that("geom_path_theme rejects inappropriate elements", {
  case <- substitute(geom_path_theme(element = NULL))
  expect_silent(eval(case))

  case <- substitute(geom_path_theme(element = element_line()))
  expect_silent(eval(case))

  case <- substitute(geom_path_theme(element = element_rect()))
  expect_error(eval(case), "should be of type")
})
