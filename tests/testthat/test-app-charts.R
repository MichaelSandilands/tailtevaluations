test_that("the waterfall draws five floating bars in the map's colours", {
  steps <- decompose_vs_neighbours(
    list(area_m2 = 300, rate = 130, valuation = 39500),
    data.frame(area_m2 = c(200, 250, 100), rate = c(100, 120, 150))
  )
  built <- plotly::plotly_build(waterfall_plot(steps, "light"))
  bars <- built$x$data[[1]]
  col <- palette_for("light")

  expect_equal(bars$type, "bar")
  expect_equal(as.numeric(bars$y), steps$end - steps$start)
  expect_equal(as.numeric(bars$base), steps$start)
  expect_equal(bars$marker$color[c(1, 5)], c(col$neighbour, col$subject))
  expect_length(built$x$layout$shapes, 4)
})

test_that("dark mode swaps every colour role", {
  light <- palette_for("light")
  dark <- palette_for("dark")
  expect_setequal(names(light), names(dark))
  expect_false(any(unlist(light) == unlist(dark)[names(light)] & names(light) != "muted"))
})
