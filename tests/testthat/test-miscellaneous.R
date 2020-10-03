test_that("theme elements are loaded by default", {
  tree <- get_element_tree()
  expect_true(sum(grepl("elementalist", names(tree))) > 2)
})

test_that("theme elements can be removed", {
  reset_theme_settings()
  tree <- get_element_tree()
  expect_equal(sum(grepl("elementalist", names(tree))), 0)
})

test_that("theme elements can be set again", {
  reset_theme_settings()
  tree <- get_element_tree()
  expect_equal(sum(grepl("elementalist", names(tree))), 0)
  theme_registration()
  tree <- get_element_tree()
  expect_true(sum(grepl("elementalist", names(tree))) > 2)
})

test_that("All elements can be computed", {
  elements <- c("line", "rect", "polygon")

  tests <- lapply(elements, function(x){compute_element(type = x)})

  expect_s3_class(tests[[1]], "element_line_seq")
  expect_s3_class(tests[[2]], "element_rect_seq")
  expect_s3_class(tests[[3]], "element_polygon")
})

test_that("new_df constructs empty dataframes", {
  df <- new_df()
  expect_equal(dim(df), c(0,0))

  df <- new_df(nrow = 5)
  expect_equal(dim(df), c(5,0))
  expect_equal(names(df), character(0))
})

test_that("unit inheritance works", {
  expect_equal(inherit_unit(5, default = "mm"), unit(5, "mm"))
  expect_equal(inherit_unit(5, unit(1, "mm")), unit(5, "mm"))
})
