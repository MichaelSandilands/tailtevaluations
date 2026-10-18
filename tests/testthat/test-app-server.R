test_that("searching selects a property and finds its nearest neighbours, capped", {
  vals <- example_valuations()
  shiny::testServer(app_server(vals, app_options(max_neighbours = 5)), {
    session$setInputs(n = 50, same_category = TRUE, search = "1001")
    expect_equal(focus(), list(type = "property", row = 1L))

    nb <- neighbours()
    expect_equal(nrow(nb), 5)
    expect_true(all(nb$category == "OFFICE"))
    expect_false(1001L %in% nb$property_number)
    expect_false(is.unsorted(nb$distance_m))
    expect_equal(nb$rank, 1:5)
  })
})

test_that("turning off 'same type' widens the search", {
  vals <- example_valuations()
  shiny::testServer(app_server(vals), {
    session$setInputs(n = 4, same_category = FALSE, search = "1001")
    expect_setequal(unique(neighbours()$category), c("OFFICE", "RETAIL (SHOPS)"))
  })
})

test_that("clicks on markers, empty map and table rows all move the focus", {
  vals <- example_valuations()
  shiny::testServer(app_server(vals), {
    session$setInputs(n = 3, same_category = TRUE)

    session$setInputs(`map-map_marker_click` = list(id = "nb-1009", .nonce = 1))
    expect_equal(focus()$row, 9L)

    session$setInputs(`map-map_click` = list(lng = -6.93, lat = 52.84, .nonce = 2))
    expect_equal(focus()$type, "pin")
    expect_equal(nrow(neighbours()), 3)

    session$setInputs(`detail-pick` = 1012L)
    expect_equal(focus()$row, 12L)
  })
})

test_that("filters feed the neighbour search after the debounce", {
  vals <- example_valuations()
  shiny::testServer(app_server(vals), {
    session$setInputs(n = 50, same_category = FALSE, `filters-council` = "KILKENNY COUNTY COUNCIL")
    session$elapse(500)
    expect_equal(sum(keep()), 20)
    session$setInputs(search = "1001") # a Carlow property; neighbours must be Kilkenny
    expect_true(all(neighbours()$local_authority == "KILKENNY COUNTY COUNCIL"))
  })
})

test_that("an unknown property number leaves the focus alone", {
  vals <- example_valuations()
  shiny::testServer(app_server(vals), {
    session$setInputs(search = "999999")
    expect_null(focus())
  })
})

test_that("app options validate and clamp", {
  expect_equal(app_options(max_neighbours = 5, default_n = 10)$default_n, 5L)
  expect_error(app_options(max_neighbours = 0))
})

test_that("run_app builds a Shiny app from loaded data", {
  app <- run_app(example_valuations())
  expect_s3_class(app, "shiny.appobj")
  html <- htmltools::renderTags(app_ui(example_valuations()))$html
  expect_match(html, "Filter properties")
  expect_match(html, "Irish commercial valuations")
})

test_that("the basemap is OpenStreetMap, credited alongside the valuation data", {
  b <- basemap()
  expect_equal(b$url, "https://tile.openstreetmap.org/{z}/{x}/{y}.png")
  expect_match(b$attribution, "OpenStreetMap</a> contributors", fixed = TRUE)
  expect_match(b$attribution, "Tailte")
  expect_match(b$attribution, "creativecommons.org/licenses/by/4.0", fixed = TRUE)
  expect_equal(b$max_zoom, 19)
})

test_that("dark mode darkens only the tile pane", {
  css <- readLines(system.file("app", "www", "app.css", package = "tailtevaluations"))
  rule <- grep('^\\[data-bs-theme="dark"\\] \\.leaflet-tile-pane', css)
  expect_length(rule, 1)
  expect_match(css[rule + 1], "invert(1) hue-rotate(180deg)", fixed = TRUE)
})

test_that("the app credits the data, links the licence and explains its changes", {
  vals <- example_valuations()
  page <- htmltools::renderTags(app_ui(vals))$html
  expect_match(page, "Contains Irish Public Sector Data")
  expect_match(page, "https://creativecommons.org/licenses/by/4.0/", fixed = TRUE)
  expect_match(page, "not endorsed by Tailte")
  expect_match(page, "About the data")

  about <- htmltools::renderTags(about_data_modal(vals))$html
  for (change in data_attribution()$changes) expect_match(about, change, fixed = TRUE)
  expect_match(about, "not Tailte \u00c9ireann valuations")
  expect_match(about, "mailto:opendata@tailte.ie", fixed = TRUE)
})

test_that("the comparison says it is the app's analysis", {
  vals <- example_valuations()
  steps <- decompose_vs_neighbours(as.list(vals$props[1, c("area_m2", "rate", "valuation")]), neighbours_of(vals, 1L))
  html <- htmltools::renderTags(compare_panel(steps, shiny::NS("detail")))$html
  expect_match(html, "not a Tailte")
})
