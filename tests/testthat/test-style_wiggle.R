test_that("wiggling works", {
  fun <- wiggle(5)
  expect_is(fun, "function")

  df <- fun(x = 1:3, y = c(1,2,1), id = c(1,1,1),
            colour = c("blue", "red"), n = 2)
  expect_equal(df$x, c(1, 1.5, 2, 2.5, 3))
  expect_equal(df$y, c(1, 1.5, 2, 1.5, 1))
  expect_equal(df$col, c("#0000FF", "#AD00AF", "#DF0063",
                         "#FF0000", NA))
  expect_true(sum(abs(df$dx)) > 0)
  expect_true(sum(abs(df$dy)) > 0)
})

test_that("element_*_wiggle return correct objects", {
  el <- element_line_wiggle(n = 10)
  expect_s3_class(el, "element_line_seq")
  expect_is(el$fun, "function")
  expect_equal(el$n, 10)

  el <- element_rect_wiggle(sides = "t")
  expect_s3_class(el, "element_rect_seq")
  expect_is(el$fun, "function")
  expect_equal(el$sides, c(top = TRUE, left = FALSE, bottom = FALSE, right = FALSE))
})

test_that("element_*_wiggle grobs can be built", {
  el <- element_line_wiggle(n  = 10)
  case <- element_grob(el)
  ctrl <- element_grob(element_line())
  expect_length(ctrl$x, 2)
  expect_length(case$x, 11)

  el <- element_rect_wiggle(n = 10, sides = "")
  case <- element_grob(el)
  ctrl <- element_grob(element_rect())
  expect_length(ctrl$x, 1)
  expect_length(case$x, 44)
})

test_that("wiggling_geoms can be added to a theme", {
  test <- theme_get() + wiggling_geoms(colour = "magenta")

  expect_true("elementalist.geom_rect" %in% names(test))
  expect_true("elementalist.geom_line" %in% names(test))

  expect_equal(test$elementalist.geom_line$colour, "magenta")
  expect_equal(test$elementalist.geom_rect$colour, "magenta")
})
