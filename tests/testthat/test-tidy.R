test_that("properties parse with a fixed schema", {
  p <- tidy_properties(fixture("carlow.geojson"), "CARLOW COUNTY COUNCIL")
  expect_identical(names(p), names(empty_properties()))
  expect_identical(vapply(p, function(x) class(x)[1], ""), vapply(empty_properties(), function(x) class(x)[1], ""))
  expect_equal(nrow(p), 4)

  office <- p[p$property_number == 10030028L, ]
  expect_equal(office$valuation, 21400)
  expect_equal(office$publication_date, as.Date("2024-06-10"))
  expect_equal(office$x_itm, 673274.37)
  expect_equal(office$valuation_basis, "NAV")
})

test_that("null and placeholder geometries become NA, blanks become NA", {
  p <- tidy_properties(fixture("carlow.geojson"), "CARLOW COUNTY COUNCIL")
  expect_true(is.na(p$x_itm[p$property_number == 10099999L])) # null geometry
  expect_true(is.na(p$x_itm[p$property_number == 10088888L])) # [0, 0]
  expect_true(is.na(p$eircode[p$property_number == 10011234L])) # ""
  expect_equal(p$publication_date[p$property_number == 10088888L], as.Date("2024-06-10")) # ISO date
})

test_that("pre-revaluation authorities parse without category or floors", {
  p <- tidy_properties(fixture("cork_city.geojson"), "CORK CITY COUNCIL")
  expect_identical(names(p), names(empty_properties()))
  expect_equal(p$valuation_basis, c("RV", "RV"))
  expect_true(is.na(p$category[1]))
  expect_equal(nrow(tidy_floors(fixture("cork_city.geojson"))), 0)
})

test_that("floors flatten to one row per level", {
  f <- tidy_floors(fixture("carlow.geojson"))
  expect_identical(names(f), names(empty_floors()))
  expect_equal(nrow(f), 3)
  expect_equal(sum(f$nav[f$property_number == 10030028L]), 12772.1 + 8708.25)
  expect_type(f$level, "character")
})

test_that("empty collections return empty tables of the right shape", {
  expect_identical(tidy_properties(fixture("empty.geojson"), "X"), empty_properties())
  expect_identical(tidy_floors(fixture("empty.geojson")), empty_floors())
})

test_that("anything other than a FeatureCollection is an error", {
  expect_error(tidy_properties('{"type": "Feature"}', "X"), "FeatureCollection")
})

test_that("finalise sorts, de-duplicates and adds WGS84 coordinates in Ireland", {
  carlow <- tidy_properties(fixture("carlow.geojson"), "CARLOW COUNTY COUNCIL")
  both <- rbind(carlow, tidy_properties(fixture("cork_city.geojson"), "CORK CITY COUNCIL"), carlow[1, ])
  expect_warning(p <- finalise_properties(both), "repeated property number")
  expect_false(anyDuplicated(p$property_number) > 0)
  expect_false(is.unsorted(p$property_number))
  ok <- !is.na(p$lon)
  expect_true(all(p$lon[ok] > -11 & p$lon[ok] < -5.5))
  expect_true(all(p$lat[ok] > 51 & p$lat[ok] < 55.5))
  office <- p[p$property_number == 10030028L, ] # Carlow town, about 52.84 N, 6.93 W
  expect_equal(c(office$lon, office$lat), c(-6.93, 52.84), tolerance = 0.02)
})

test_that("finalise_floors keeps only floors of surviving properties", {
  p <- finalise_properties(tidy_properties(fixture("carlow.geojson"), "CARLOW COUNTY COUNCIL"))
  f <- tidy_floors(fixture("carlow.geojson"))
  orphan <- f[1, ]
  orphan$property_number <- 1L
  out <- finalise_floors(rbind(f, orphan, f[1, ]), p)
  expect_equal(nrow(out), 3)
  expect_false(1L %in% out$property_number)
})

test_that("as_sf_itm returns ITM points for rows with coordinates", {
  s <- as_sf_itm(tidy_properties(fixture("carlow.geojson"), "CARLOW COUNTY COUNCIL"))
  expect_s3_class(s, "sf")
  expect_equal(sf::st_crs(s)$epsg, 2157L)
  expect_equal(nrow(s), 2)
})
