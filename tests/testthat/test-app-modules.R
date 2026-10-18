test_that("the filters module returns debounced filter values", {
  vals <- example_valuations()
  shiny::testServer(filters_server, args = list(vals = vals, debounce_ms = 100), {
    session$setInputs(category = "Office", nav_min = 15000)
    session$elapse(200)
    f <- session$returned()
    expect_equal(f$category, "Office")
    expect_equal(f$nav_min, 15000)
    expect_null(f$published) # the full date range means no date filter
  })
})

test_that("the detail panel describes the selected property and compares it", {
  vals <- example_valuations()
  shiny::testServer(detail_server, args = list(
    vals = vals,
    focus = shiny::reactiveVal(list(type = "property", row = 1L)),
    neighbours = shiny::reactive(neighbours_of(vals, 1L, 8)),
    n_requested = shiny::reactive(8L),
    same_category = shiny::reactive(TRUE),
    mode = shiny::reactive("light")
  ), {
    expect_match(output$header$html, "Property 01")
    expect_match(output$header$html, "Carlow County Council")
    expect_match(output$breakdown$html, "Floors add up to")
    expect_match(output$nearest$html, "The 8 nearest office properties")
    expect_match(output$compare$html, "typical neighbour", ignore.case = TRUE)

    steps <- comparison()
    expect_equal(sum(steps$value[1:4]), vals$props$valuation[1])
  })
})

test_that("the detail panel has a sensible empty state and pin state", {
  vals <- example_valuations()
  focus <- shiny::reactiveVal(NULL)
  shiny::testServer(detail_server, args = list(
    vals = vals, focus = focus,
    neighbours = shiny::reactive(neighbours_of(vals, 1L, 4)),
    n_requested = shiny::reactive(4L), same_category = shiny::reactive(FALSE),
    mode = shiny::reactive("light")
  ), {
    expect_match(output$header$html, "Pick a property")
    expect_match(output$compare$html, "Select a property")
    focus(list(type = "pin", lon = -6.93, lat = 52.84))
    session$flushReact()
    expect_match(output$header$html, "Pinned location")
    expect_match(output$breakdown$html, "Select a property")
  })
})

test_that("the map module reports how many dots it drew", {
  vals <- example_valuations()
  shiny::testServer(map_server, args = list(
    vals = vals,
    keep = shiny::reactive(rep(TRUE, 40)),
    focus = shiny::reactiveVal(NULL),
    neighbours = shiny::reactive(neighbours_of(vals, 1L, 4)),
    mode = shiny::reactive("light"),
    refit = shiny::reactive(NULL),
    max_points = 10L
  ), {
    session$setInputs(map_bounds = list(north = 55, south = 51, east = -5, west = -11))
    session$elapse(300)
    expect_equal(output$status, "40 properties match. Showing 10 of the 40 in view; zoom in to see the rest.")
  })
})
