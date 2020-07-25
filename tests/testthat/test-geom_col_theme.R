test_that("geom_col_theme child elements inherit from theme", {
 df <- data.frame(x = LETTERS[1:3], y = 1:3)

 test <- ggplot(df, aes(x, y)) +
   geom_col_theme(element = element_rect_seq(colour = "blue")) +
   theme(elementalist.geom_rect = element_rect_seq(fill = "yellow"))

 gt <- ggplotGrob(test)
 gt <- gt$grobs[[grep("panel", gt$layout$name)]]
 gt <- gt$children[vapply(gt$children, inherits, logical(1), "polygon")][[1]]

 expect_equal(gt$gp$col, rep("blue", 3))
 expect_equal(gt$gp$fill, "yellow")
})

test_that("geom_bar_theme child elements inherit from theme", {
  df <- data.frame(x = LETTERS[1:3], y = 1:3)

  test <- ggplot(mpg, aes(class)) +
    geom_bar_theme(element = element_rect_seq(colour = "blue")) +
    theme(elementalist.geom_rect = element_rect_seq(fill = "yellow"))

  gt <- ggplotGrob(test)
  gt <- gt$grobs[[grep("panel", gt$layout$name)]]
  gt <- gt$children[vapply(gt$children, inherits, logical(1), "polygon")][[1]]

  expect_equal(gt$gp$col, rep("blue", 7))
  expect_equal(gt$gp$fill, "yellow")
})

test_that("geom_histogram_theme child elements inherit from theme", {
  df <- data.frame(x = LETTERS[1:3], y = 1:3)

  test <- ggplot(faithful, aes(waiting)) +
    geom_histogram_theme(element = element_rect_seq(colour = "blue"), bins = 20) +
    theme(elementalist.geom_rect = element_rect_seq(fill = "yellow"))

  gt <- ggplotGrob(test)
  gt <- gt$grobs[[grep("panel", gt$layout$name)]]
  gt <- gt$children[vapply(gt$children, inherits, logical(1), "polygon")][[1]]

  expect_equal(gt$gp$col, rep("blue", 20))
  expect_equal(gt$gp$fill, "yellow")
})
