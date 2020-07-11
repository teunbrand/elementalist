test_that("axis2 can render", {
  g <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    scale_x_continuous(breaks = NULL) +
    guides(x.sec = "axis2", x = "axis2",
           y.sec = "axis2", y = "axis2")
  pg <- ggplotGrob(g)
  grobs <- pg$grobs[grepl("axis-", pg$layout$name)]
  classes <- vapply(lapply(grobs, class), `[`, character(1), 1)
  expect_true(all(classes == "absoluteGrob"))
})

test_that("axis2 can render decorated lines", {
  g <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    guides(x = "axis2") +
    theme(
      axis.ticks.x = element_line_wiggle(1, n = 5),
      axis.ticks.length.x = unit(1, "cm")
    )
  # This is just simply a check that it runs, not that it runs correctly
  pg <- ggplotGrob(g)
  expect_s3_class(pg, "gtable")
})
