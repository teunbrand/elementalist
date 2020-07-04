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
