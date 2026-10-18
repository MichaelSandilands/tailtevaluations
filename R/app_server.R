# App server -------------------------------------------------------------------
#
# Owns the one piece of shared state, the focus (nothing, a property or a
# pinned location), and the neighbour search. The modules draw and report.

app_server <- function(vals, options = app_options()) {
  force(vals)
  force(options)

  function(input, output, session) {
    props <- vals$props
    updateSelectizeInput(session, "search", choices = vals$search_choices, selected = character(0), server = TRUE)
    mode <- reactive(if (identical(input$dark_mode, "dark")) "dark" else "light")

    filters <- filters_server("filters", vals)
    keep <- reactive(filter_properties(props, filters(), vals$floor_use_index))

    focus <- reactiveVal(NULL)
    select_property <- function(pn) {
      row <- unname(vals$row_of[as.character(pn)])
      if (length(row) == 1 && !is.na(row)) focus(list(type = "property", row = row))
    }

    n <- reactive({
      requested <- if (is.null(input$n)) options$default_n else as.integer(input$n)
      min(max(1L, requested), options$max_neighbours)
    })
    same_category <- reactive(isTRUE(input$same_category))

    focus_xy <- reactive({
      f <- focus()
      req(f)
      if (f$type == "property") {
        return(c(props$x_itm[f$row], props$y_itm[f$row]))
      }
      pt <- sf::st_transform(sf::st_sfc(sf::st_point(c(f$lon, f$lat)), crs = 4326), ITM)
      as.numeric(sf::st_coordinates(pt))
    })

    neighbours <- reactive({
      f <- focus()
      req(f)
      subject <- if (f$type == "property") f$row
      candidates <- neighbour_candidates(props, keep(), subject, same_category())
      nb <- nearest_rows(focus_xy(), candidates, vals$geom, n(), coords = vals$xy)
      cbind(rank = seq_len(nrow(nb)), nb, props[nb$row, , drop = FALSE])
    })

    map <- map_server(
      "map", vals, keep, focus, neighbours, mode,
      refit = reactive(list(focus(), n(), same_category())),
      max_points = options$max_map_points
    )
    picked <- detail_server("detail", vals, focus, neighbours, n, same_category, mode)

    observeEvent(map$marker_click(), select_property(sub("^(nb|sel)-", "", map$marker_click()$id)))
    observeEvent(map$map_click(), focus(list(type = "pin", lon = map$map_click()$lng, lat = map$map_click()$lat)))
    observeEvent(input$search, if (nzchar(input$search)) select_property(input$search))
    observeEvent(picked(), select_property(picked()))
    observeEvent(input$about_data, showModal(about_data_modal(vals)))
  }
}

app_options <- function(max_neighbours = 50L, max_map_points = 4000L, default_n = 10L) {
  stopifnot(max_neighbours >= 1, max_map_points >= 1, default_n >= 1)
  list(
    max_neighbours = as.integer(max_neighbours),
    max_map_points = as.integer(max_map_points),
    default_n = as.integer(min(default_n, max_neighbours))
  )
}
