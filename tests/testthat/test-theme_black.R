test_that("theme_black returns correct object", {
  obj <- theme_black()
  expect_s3_class(obj, "theme")
  expect_equal(
    sum(grepl("elementalist", names(obj))), 2
  )
})

test_that("theme_black can change colours", {
  case <- theme_black(base_colour = "green")
  ctrl <- theme_black(base_colour = "white")

  expect_equal(case$text$colour, "green")
  expect_equal(ctrl$text$colour, "white")
})

test_that("theme_black can be added to a plot", {
  p <- ggplot(mpg, aes(cty, hwy)) +
    geom_point(colour = "white") +
    ggtitle("Here be a title") +
    theme_black()
  gt <- ggplotGrob(p)

  bg <- gt$grobs[[grep("background", gt$layout$name)]]
  expect_equal(bg$gp$fill, "black")

  title <- gt$grobs[[grep("^title$", gt$layout$name)]]$children[[1]]
  expect_equal(title$gp$col, "white")
})
