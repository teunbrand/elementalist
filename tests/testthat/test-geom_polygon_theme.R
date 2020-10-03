df <- data.frame(
  x = c(0, 0.5, 1, 0.5, 0.25, 0.5, 0.75, 0.5),
  y = c(0.5, 0, 0.5, 1, 0.5, 0.25, 0.5, 0.75),
  sub_id = rep(c(1, 2), each = 4),
  id = rep(1, each = 8)
)

base <- ggplot(df, aes(x, y, group = id, subgroup = sub_id))

test_that("geom_polygon_theme can change polygon appearance", {
  base  <- base + geom_polygon_theme()
  ctrl  <- base
  case1 <- base + theme(elementalist.geom_polygon = element_polygon(size = 5))
  case2 <- base + theme(elementalist.geom_polygon = element_polygon_glow())

  cases <- list(ctrl, case1, case2)
  cases <- lapply(cases, ggplotGrob)
  cases <- lapply(cases, function(x) {
    is_panel <- grep("panel", x$layout$name)
    panel <- x$grobs[[is_panel]]
    names <- names(panel$children)
    i <- which(startsWith(names, "geom_polygon_theme"))
    panel$children[[i]]
  })

  expect_equal(cases[[1]]$gp$lwd, 0.5 * .pt)
  expect_equal(cases[[2]]$gp$lwd, 5 * .pt)

  expect_s3_class(cases[[1]], "pathgrob")
  expect_s3_class(cases[[3]], "gTree")
  expect_s3_class(cases[[3]]$children[[1]], "pathgrob")
  expect_s3_class(cases[[3]]$children[[2]], "polyline")
})

test_that("geom_polygon_theme rejects inappropriate elements", {
  case <- substitute(geom_polygon_theme(element = NULL))
  expect_silent(eval(case))

  case <- substitute(geom_polygon_theme(element = element_polygon()))
  expect_silent(eval(case))

  case <- substitute(geom_polygon_theme(element = element_line()))
  expect_error(eval(case), "should be of type")
})
