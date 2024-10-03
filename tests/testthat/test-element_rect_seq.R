test_that("element_rect_seq returns correct class", {

  el <- element_rect_seq()
  expect_s3_class(el, "element_rect_seq")
  expect_s3_class(el, "element_rect")
  expect_s3_class(el, "element")
})

test_that("element_rect_seq parses sides", {
  el <- element_rect_seq()
  expect_true(all(el$sides))
  el <- element_rect_seq(sides = "")
  expect_false(any(el$sides))
  el <- element_rect_seq(sides = "tb")
  expect_equal(el$sides, c(top = TRUE, left = FALSE,
                           bottom = TRUE, right = FALSE))
  el <- element_rect_seq(sides = "lr")
  expect_equal(el$sides, c(top = FALSE, left = TRUE,
                           bottom = FALSE, right = TRUE))
})

test_that("element_rect_seq generates grobs", {
  el <- element_rect_seq(fun = wiggle(5), fill = 'grey50', colour = "blue",
                         sides = 't', linewidth = 2)

  grob <- element_grob(el, width = 0.5, height = 0.5)
  expect_s3_class(grob, "gTree")
  expect_s3_class(grob$children[[1]], "polygon")
  expect_s3_class(grob$children[[2]], "polyline")
})

test_that("element_rect_seq generates sides correctly", {
  cases <- c("", "t", "b", "lr", "tblr")
  cases <- lapply(cases, function(side) {
    element_rect_seq(sides = side, colour = "black")
  })
  cases <- lapply(cases, element_grob)
  expect_s3_class(cases[[1]], "polygon")
  expect_s3_class(cases[[5]], "polygon")
  test <- lapply(cases[2:4], expect_s3_class, class = "gTree")
  # First case should have no colour
  expect_equal(cases[[1]]$gp$col, NA)
  expect_equal(cases[[5]]$gp$col, "black")
  cases <- lapply(cases[2:4], function(x){x$children[[2]]})
  xx <- lapply(cases, function(x) {
    x <- c(unclass(x$x), numeric(0))
  })
  yy <- lapply(cases, function(y) {
    y <- c(unclass(y$y), numeric(0))
  })

  expect_equal(xx, list(c(1, 0), c(0, 1), c(0, 0, 1, 1)))
  expect_equal(yy, list(c(1, 1), c(0, 0), c(1, 0, 0, 1)))
})

test_that("element_rect_seq handles non-npc units", {
  el <- element_rect_seq(sides = "")
  grob <- element_grob(
    el,
    x = unit(0.5, "cm"), y = unit(1, "inch"),
    width = unit(0.2, "npc"), height = unit(10, "points")
  )
  expect_s3_class(grob, "polygon")
})
