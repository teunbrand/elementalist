test_that("multicolour works", {
  fun <- multicolour()
  expect_is(fun, "function")

  df <- fun(x = 1:3, y = c(1,2,1), id = c(1,1,1),
            colour = c("blue", "red"), n = 1)
  expect_equal(df$x, c(1, 1.5, 2, 2.5, 3))
  expect_equal(df$y, c(1, 1.5, 2, 1.5, 1))
  expect_equal(df$col, c("#0000FF", "#AD00AF", "#DF0063",
                         "#FF0000", NA))
})

test_that("element_*_multicolour return correct objects", {
  el <- element_line_multicolour(n = 10)
  expect_s3_class(el, "element_line_seq")
  expect_is(el$fun, "function")
  expect_equal(el$n, 10)

  el <- element_rect_multicolour(sides = "t")
  expect_s3_class(el, "element_rect_seq")
  expect_is(el$fun, "function")
  expect_equal(el$sides, c(top = TRUE, left = FALSE, bottom = FALSE, right = FALSE))
})

test_that("element_*_wiggle grobs can be built", {
  el <- element_line_multicolour(n  = 10)
  case <- element_grob(el)
  ctrl <- element_grob(element_line())
  expect_length(ctrl$x, 2)
  expect_length(case$x0, 11)

  el <- element_rect_multicolour(n = 10, sides = "")
  case <- element_grob(el)
  ctrl <- element_grob(element_rect())
  expect_length(ctrl$x, 1)
  expect_length(case$x, 48)
})
