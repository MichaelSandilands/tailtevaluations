make_props <- function() {
  data.frame(
    property_number = 1:6,
    local_authority = c(
      "CARLOW COUNTY COUNCIL", "CARLOW COUNTY COUNCIL", "CARLOW COUNTY COUNCIL",
      "CORK CITY COUNCIL", "CARLOW COUNTY COUNCIL", "CARLOW COUNTY COUNCIL"
    ),
    category = c("OFFICE", "OFFICE", "RETAIL (SHOPS)", NA, "OFFICE", "OFFICE"),
    category_label = c("Office", "Office", "Retail (shops)", "Not recorded", "Office", "Office"),
    use_label = "X",
    valuation = c(20000, 30000, 15000, 50.5, 10000, 40000),
    valuation_basis = c("NAV", "NAV", "NAV", "RV", "NAV", "NAV"),
    area_m2 = c(200, 250, 100, NA, 100, 400),
    rate = c(100, 120, 150, NA, 100, 100),
    publication_date = as.Date(c("2024-06-10", "2024-06-10", "2023-09-15", "2009-01-01", "2024-06-10", "2025-02-03")),
    car_park = c(0, 5, 0, 0, 2, 10),
    x_itm = c(673000, 673100, 673050, 567000, 674000, 673200),
    y_itm = c(677000, 677000, 677050, 571000, 677000, 677300),
    stringsAsFactors = FALSE
  )
}

test_that("filters combine and treat NULL or NA bounds as unset", {
  p <- make_props()
  expect_equal(sum(filter_properties(p, list())), 6)
  expect_equal(which(filter_properties(p, list(category = "Office"))), c(1, 2, 5, 6))
  expect_false(filter_properties(p, list(nav_min = 0))[4]) # a euro bound drops RV records
  expect_equal(which(filter_properties(p, list(nav_min = 15000, nav_max = 30000))), 1:3)
  expect_equal(which(filter_properties(p, list(car_min = 5))), c(2, 6))
  expect_equal(which(filter_properties(p, list(area_min = 150, rate_max = 110))), c(1, 6))
  expect_equal(sum(filter_properties(p, list(nav_min = NA, area_max = NA))), 6)
  published <- as.Date(c("2024-01-01", "2024-12-31"))
  expect_equal(which(filter_properties(p, list(published = published))), c(1, 2, 5))
})

test_that("the floor-use filter matches properties with any such floor", {
  p <- make_props()
  index <- list(Store = c(2L, 5L), Canteen = 6L)
  expect_equal(which(filter_properties(p, list(floor_use = "Store"), index)), c(2, 5))
  expect_equal(which(filter_properties(p, list(floor_use = c("Store", "Canteen")), index)), c(2, 5, 6))
})

test_that("neighbour candidates respect basis, category and exclude the subject", {
  p <- make_props()
  keep <- rep(TRUE, 6)
  expect_equal(neighbour_candidates(p, keep, subject_row = 1, same_category = TRUE), c(2, 5, 6))
  expect_equal(neighbour_candidates(p, keep, subject_row = 1, same_category = FALSE), c(2, 3, 5, 6))
  expect_equal(neighbour_candidates(p, keep, subject_row = 4), integer()) # the only RV record
  expect_equal(neighbour_candidates(p, keep), 1:6) # pinned location
  expect_equal(neighbour_candidates(p, c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE), 1), c(5, 6))
})

test_that("nngeo returns neighbours in distance order, capped at the pool size", {
  p <- make_props()
  geom <- sf::st_as_sf(p[c("x_itm", "y_itm")], coords = c("x_itm", "y_itm"), crs = 2157)
  nb <- nearest_rows(c(673000, 677000), c(2, 3, 5, 6), geom, k = 3)
  expect_equal(nb$row, c(3, 2, 6))
  expect_equal(nb$distance_m[2], 100)
  expect_false(is.unsorted(nb$distance_m))
  expect_equal(nrow(nearest_rows(c(673000, 677000), c(2, 3), geom, k = 50)), 2)
  expect_equal(nrow(nearest_rows(c(673000, 677000), integer(), geom, k = 5)), 0)
})

test_that("the windowed search returns exactly what a full search would", {
  withr::local_seed(42)
  n <- 5000
  xy <- cbind(
    c(stats::runif(n - 20, 400000, 800000), stats::rnorm(20, 600000, 50)),
    c(stats::runif(n - 20, 500000, 950000), stats::rnorm(20, 700000, 50))
  )
  geom <- sf::st_as_sf(data.frame(x = xy[, 1], y = xy[, 2]), coords = c("x", "y"), crs = 2157)
  candidates <- sort(sample.int(n, 4000))
  for (pt in list(c(600000, 700000), c(410000, 940000), c(650000, 520000))) {
    for (k in c(1, 7, 50)) {
      full <- nearest_rows(pt, candidates, geom, k)
      fast <- nearest_rows(pt, candidates, geom, k, coords = xy, window_m = 200)
      expect_equal(fast$row, full$row)
      expect_equal(fast$distance_m, full$distance_m)
    }
  }
})

test_that("the waterfall steps sum exactly to the subject's valuation", {
  subject <- list(area_m2 = 300, rate = 130, valuation = 39500)
  nbs <- data.frame(area_m2 = c(200, 250, 100, NA), rate = c(100, 120, 150, 90))
  s <- decompose_vs_neighbours(subject, nbs)
  t <- attr(s, "typical")
  expect_equal(c(t$area_m2, t$rate, t$n), c(200, 120, 3))
  expect_equal(s$value[1:4], c(200 * 120, (300 - 200) * 120, 300 * (130 - 120), 39500 - 300 * 130))
  expect_equal(sum(s$value[1:4]), s$value[5])
  expect_equal(s$end[4], s$value[5])
  expect_equal(s$start[2:4], s$end[1:3])
})

test_that("no comparison without enough floor data", {
  subject <- list(area_m2 = 300, rate = 130, valuation = 39500)
  expect_null(decompose_vs_neighbours(subject, data.frame(area_m2 = c(200, NA), rate = c(100, 90))))
  no_floors <- list(area_m2 = NA, rate = NA, valuation = 50)
  expect_null(decompose_vs_neighbours(no_floors, data.frame(area_m2 = 1:5, rate = 1:5)))
})

test_that("points in view are capped by the stable draw rank", {
  p <- data.frame(lon = c(-7, -7, -7, -9), lat = c(53, 53, 53, 53), draw_rank = c(3, 1, 2, 4))
  b <- list(north = 54, south = 52, east = -6, west = -8)
  drawn <- points_in_view(p, rep(TRUE, 4), b, max_points = 2)
  expect_equal(as.vector(drawn), c(2, 3))
  expect_equal(attr(drawn, "counts"), c(matching = 4, in_view = 3, drawn = 2))
  expect_match(map_status_text(attr(drawn, "counts")), "Showing 2 of the 3 in view")
  expect_match(map_status_text(c(matching = 0, in_view = 0, drawn = 0)), "No properties match")
})

test_that("formatting uses euro, a true minus sign and m2", {
  withr::local_locale(LC_CTYPE = "C.UTF-8")
  expect_equal(fmt_eur(21400), "\u20ac21,400")
  expect_equal(fmt_eur(-2300, signed = TRUE), "\u2212\u20ac2,300")
  expect_equal(fmt_eur(4100, signed = TRUE), "+\u20ac4,100")
  expect_equal(fmt_valuation(50.79, "RV"), "RV 50.79")
  expect_equal(fmt_distance(240), "240 m")
  expect_equal(fmt_distance(1460), "1.5 km")
  expect_equal(fmt_rate(110), "\u20ac110/m\u00b2")
  expect_equal(fmt_date(as.Date("2024-06-10")), "10 Jun 2024")
  expect_equal(council_label("DUN LAOGHAIRE RATHDOWN CO CO"), "D\u00fan Laoghaire-Rathdown County Council")
  expect_equal(council_label("LIMERICK CITY AND COUNTY COUNCIL"), "Limerick City and County Council")
})
