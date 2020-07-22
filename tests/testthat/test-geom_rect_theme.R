df <- data.frame(
  xmin = c(0, 1),
  xmax = c(2, 3),
  ymin = c(0, 1),
  ymax = c(2, 3),
  colour = c("A", "B")
)
base <- ggplot(df, aes(xmin = xmin, xmax = xmax,
                       ymin = ymin, ymax = ymax,
                       fill = colour))

test_that("geom_rect_theme can change rect appearance", {
  base  <- base + geom_rect_theme()
  case1 <- base + theme(elementalist.geom_rect = element_rect_wiggle())
  case2 <- base + theme(elementalist.geom_rect = element_rect_multicolour())
  ctrl  <- base + theme(elementalist.geom_rect = element_rect_seq())

  cases <- list(case1, case2, ctrl)
  cases <- lapply(cases, ggplotGrob)
  cases <- lapply(cases, function(x) {
    is_panel <- grep("panel", x$layout$name)
    panel <- x$grobs[[is_panel]]
    names <- names(panel$children)
    i <- which(startsWith(names, "rectseq") | startsWith(names, "GRID.polygon"))
    panel$children[[i]]
  })

  expect_length(cases[[1]]$children[[1]]$x, 408)
  expect_length(cases[[1]]$children[[1]]$gp$col, 1)

  expect_length(cases[[2]]$children[[1]]$x, 416)
  expect_length(cases[[2]]$children[[2]]$gp$col, 408)

  expect_length(cases[[3]]$x, 16)
  expect_length(cases[[3]]$gp$col, 2)
})

test_that("geom_rect_theme child elements inherits from theme", {
  test <- base + geom_rect_theme(
    element = element_rect_seq(colour = "blue")
    ) +
    theme(
      elementalist.geom_rect = element_rect_seq(linetype = 2)
    )
  gt <- ggplotGrob(test)
  gt <- gt$grobs[[grep("panel", gt$layout$name)]]
  gt <- gt$children[vapply(gt$children, inherits, logical(1), "polygon")][[1]]

  expect_equal(gt$gp$col, c("blue", "blue"))
  expect_equal(gt$gp$lty, c(2))
})

test_that("geom_rect_theme rejects inappropriate elements", {
  case <- substitute(geom_rect_theme(element = NULL))
  expect_silent(eval(case))

  case <- substitute(geom_rect_theme(element = element_rect()))
  expect_silent(eval(case))

  case <- substitute(geom_rect_theme(element = element_line()))
  expect_error(eval(case), "should be of type")
})
