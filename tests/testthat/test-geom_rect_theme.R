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

test_that("geom_rect_theme defaults to geom_rect when no theme is set", {
  case <- base + geom_rect_theme()
  ctrl <- base + geom_rect()
  case <- layer_grob(case, 1)[[1]]
  ctrl <- layer_grob(ctrl, 1)[[1]]
  case$name <- NULL
  ctrl$name <- NULL
  expect_identical(case, ctrl)
})

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
