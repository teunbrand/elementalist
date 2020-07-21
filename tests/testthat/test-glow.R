test_that("glowing works", {
  fun <- glow(5)
  expect_is(fun, "function")

  df <- fun(x = 1:3, y = c(1,2,1), id = c(1,1,1),
            colour = "pink", n = 2)
  expect_equal(df$x, c(1:3, 1:3))
  expect_equal(df$y, c(1, 2, 1, 1, 2, 1))
  expect_equal(df$col, rep(c("#FFC0CB80", "#FFC0CB55"), each = 3))
  expect_equal(df$sub_id, c(1,1,1,2,2,2))
})

test_that("element_*_wiggle return correct objects", {
  el <- element_line_glow(n = 10)
  expect_s3_class(el, "element_line_seq")
  expect_is(el$fun, "function")
  expect_equal(el$n,  10)

  el <- element_rect_glow(sides = "t")
  expect_s3_class(el, "element_rect_seq")
  expect_is(el$fun, "function")
  expect_equal(el$sides, c(top = TRUE, left = FALSE, bottom = FALSE, right = FALSE))
})

test_that("element_*_glow grobs can be built", {
  el <- element_line_glow(n = 10, colour = "blue", size = 0.5)
  case <- element_grob(el)
  ctrl <- element_grob(element_line())

  expect_length(ctrl$x, 2)
  expect_length(case$x, 20)

  el <- element_rect_glow(n = 10, colour = "blue", size = 0.5)
  case <- element_grob(el)
  ctrl <- element_grob(element_rect())
  expect_length(ctrl$x, 1)
  expect_length(case$children[[2]]$x, 80)
})
