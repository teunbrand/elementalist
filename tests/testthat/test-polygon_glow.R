test_that("polygon_glow responds to makeContext", {
  x <- c(0, 10)
  y <- c(0, 10)

  ctrl <- polygon_glow(
    x = x, y = y, id = c(1, 1),
    gp = gpar(
      fill = NULL,
      lty = 1
    ),
    amount = 0, closed = FALSE
  )

  test <- polygon_glow(
    x = x, y = y, id = c(1, 1),
    gp = gpar(
      fill = NULL,
      lty = 2
    ),
    amount = 0, closed = FALSE
  )

  expect_equal(test$fg$x, ctrl$fg$x)
  expect_s3_class(test$fg, "glowlineGrob")
  expect_s3_class(ctrl$fg, "glowlineGrob")

  ctrl <- makeContext(ctrl$fg)
  test <- makeContext(test$fg)

  expect_gt(length(test$x), length(ctrl$x))
  expect_equal(ctrl$gp$lty, 1)
  expect_null(test$gp$lty)
})
