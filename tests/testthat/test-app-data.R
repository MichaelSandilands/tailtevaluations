test_that("prepare_valuations adds floor totals, labels and filter choices", {
  vals <- example_valuations()
  p <- vals$props
  expect_s3_class(vals, "tailte_valuations")
  expect_equal(nrow(p), 40)
  expect_equal(p$area_m2[1], 81 + 21)
  expect_equal(p$rate[1], (81 * 100 + 21 * 40) / (81 + 21))
  expect_equal(p$n_floors, rep(2L, 40))
  expect_equal(sort(unique(p$draw_rank)), 1:40)
  expect_equal(unname(vals$choices$council), c("CARLOW COUNTY COUNCIL", "KILKENNY COUNTY COUNCIL"))
  expect_equal(names(vals$choices$council), c("Carlow County Council", "Kilkenny County Council"))
  expect_equal(vals$choices$category, c("Office", "Retail (shops)"))
  expect_equal(vals$choices$floor_use, c("Office(s)", "Store"))
  expect_equal(vals$updated, as.Date("2026-10-17"))
  expect_equal(unname(vals$row_of["1005"]), 5L)
})

test_that("the draw order doesn't disturb the caller's random numbers", {
  withr::local_seed(1)
  before <- stats::runif(1)
  withr::local_seed(1)
  example_valuations()
  expect_equal(stats::runif(1), before)
})

test_that("properties without coordinates are counted, not mapped", {
  p <- finalise_properties(tidy_properties(fixture("carlow.geojson"), "CARLOW COUNTY COUNCIL"))
  vals <- prepare_valuations(p, tidy_floors(fixture("carlow.geojson")))
  expect_equal(nrow(vals$props), 2)
  expect_equal(vals$n_unmapped, 2)
})

test_that("load_valuations reads what write_outputs wrote", {
  dir <- withr::local_tempdir()
  p <- finalise_properties(tidy_properties(fixture("carlow.geojson"), "CARLOW COUNTY COUNCIL"))
  f <- finalise_floors(tidy_floors(fixture("carlow.geojson")), p)
  write_outputs(p, f, validate_refresh(p, f, "CARLOW COUNTY COUNCIL"), dir)
  vals <- load_valuations(dir)
  expect_equal(nrow(vals$props), 2)
  expect_false(is.null(vals$updated))
  printed <- capture_messages(print(vals))
  expect_match(printed[1], "2 mapped properties")
  expect_match(printed[2], "Data updated")
})

test_that("a missing data source explains how to fix it", {
  withr::local_envvar(VALUATIONS_DATA = "")
  withr::local_dir(withr::local_tempdir())
  expect_error(data_source(), "No valuation data found")
})
