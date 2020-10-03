


test_that("fracture_linetype fractures a line", {
  out <- fracture_linetype(
    c(0, 10), c(0, 10), dash = sqrt(2), gap = sqrt(2)
  )
  expect_equal(out$x, out$y)
  expect_equal(out$x, c(0:10, 10))
  expect_equal(out$id, rep(c(1, 0), 6))
})

test_that("fracture_linetype fractures unevenly", {
  out <- fracture_linetype(
    c(0, 10), c(0, 10),
    dash = c(sqrt(2), sqrt(2)),
    gap = c(sqrt(2), sqrt(2) * 2)
  )

  expect_equal(out$x, c(0, 1, 3, 4, 5, 6, 8, 9, 10, 10))
})

test_that("fracture_linetype handles elbow pieces", {
  out <- fracture_linetype(
    c(0, 5, 10), c(0, 5, 0),
    dash = sqrt(2), gap = sqrt(2)
  )

  expect_equal(out$x, c(0:5, 5, 5:10, 10))
  expect_equal(out$y, c(0:5, 5, 5:0, 0))
  expect_equal(out$id, c(1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0))
})

test_that("fracture_linetype elbows can span multiple pieces", {
  out <- fracture_linetype(
    c(0, 0.5, 1, 1.5, 2),
    c(0, 0.5, 0, 0.5, 0),
    dash = sqrt(2)*1.25, gap = sqrt(2)
  )
  expect_equal(out$x, c(0, 0.5, 0.5, 1, 1, 1.25))
  expect_equal(out$y, c(0, 0.5, 0.5, 0, 0, 0.25))
  expect_equal(out$id, c(1, rep(0, 5)))
})

test_that("interpret_linetype finds correct linetypes", {

  out <- interpret_linetype(1, mult = 1)
  expect_equal(out, list(dash = 1, gap = 0))

  out <- interpret_linetype(2, mult = 1)
  expect_equal(out, list(dash = 4, gap = 4))

  out <- interpret_linetype("F2", mult = 1)
  expect_equal(out, list(dash = 15, gap = 2))

  out <- substitute(interpret_linetype("222", mult = 1))
  expect_error(eval(out), "Invalid line type")
})
