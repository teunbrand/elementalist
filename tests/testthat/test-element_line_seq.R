test_that("element_line_seq returns correct class", {

  el <- element_line_seq()
  expect_s3_class(el, "element_line_seq")
  expect_s3_class(el, "element_line")
  expect_s3_class(el, "element")
})

test_that("element_line_seq generates grobs", {
  el <- element_line_seq(fun = wiggle(1))

  grob <- element_grob(el)
  expect_s3_class(grob, "polyline")
})
