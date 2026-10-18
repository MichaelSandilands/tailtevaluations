# Map module: background dots, the focus layer and clicks ----------------------

map_ui <- function(id) {
  ns <- NS(id)
  card(
    class = "map-card", full_screen = TRUE,
    card_body(padding = 0, leafletOutput(ns("map"), height = "100%")),
    card_footer(class = "map-status", textOutput(ns("status"), inline = TRUE))
  )
}

#' @param keep Reactive logical vector of rows passing the filters.
#' @param focus Reactive focus: `NULL`, a property (`row`) or a pin (`lon`, `lat`).
#' @param neighbours Reactive neighbour table for the focus.
#' @param mode Reactive `"light"` or `"dark"`.
#' @param refit Reactive that changes whenever the map should reframe the focus.
#' @param max_points Cap on background dots drawn per view.
#' @return A list of reactives: `marker_click` and `map_click`.
#' @noRd
map_server <- function(id, vals, keep, focus, neighbours, mode, refit, max_points = 4000L) {
  moduleServer(id, function(input, output, session) {
    props <- vals$props

    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(preferCanvas = TRUE, minZoom = 6)) |>
        add_basemap() |>
        fitBounds(-10.6, 51.4, -5.9, 55.4)
    })

    redraw_focus <- function(proxy) {
      draw_focus(proxy, props, focus(), neighbours, palette_for(mode()), vals$point_labels)
    }

    bounds <- debounce(reactive(input$map_bounds), 250)
    counts <- reactiveVal(NULL)

    # Background dots for the current view, capped. The focus layer is redrawn
    # after them so it stays on top on the shared canvas.
    observe({
      b <- bounds()
      req(b)
      drawn <- points_in_view(props, keep(), b, max_points)
      counts(attr(drawn, "counts"))
      proxy <- leafletProxy("map") |> clearGroup("points")
      if (length(drawn)) {
        proxy <- proxy |> addCircleMarkers(
          props$lon[drawn], props$lat[drawn],
          layerId = as.character(props$property_number[drawn]),
          group = "points", radius = 4, stroke = FALSE,
          fillColor = palette_for(mode())$context, fillOpacity = 0.6,
          label = vals$point_labels[drawn], options = pathOptions(bubblingMouseEvents = FALSE)
        )
      }
      isolate(redraw_focus(proxy))
    })

    observe(redraw_focus(leafletProxy("map")))

    # Frame the focus and its neighbours
    observeEvent(refit(), {
      f <- focus()
      req(f)
      nb <- neighbours()
      fx <- focus_lonlat(f, props)
      leafletProxy("map") |> flyToBounds(
        min(fx[1], nb$lon), min(fx[2], nb$lat), max(fx[1], nb$lon), max(fx[2], nb$lat),
        options = list(padding = c(48, 48), maxZoom = 18, duration = 0.6)
      )
    })

    output$status <- renderText({
      req(counts())
      map_status_text(counts())
    })

    list(
      marker_click = reactive(input$map_marker_click),
      map_click = reactive(input$map_click)
    )
  })
}

# Basemap -----------------------------------------------------------------------
#
# OpenStreetMap's standard tiles: no key and nothing to configure at deploy
# time. The tile usage policy asks for the single tile.openstreetmap.org
# address (leaflet's default URL still uses {s} subdomains), visible
# attribution, and light use. Dark mode darkens the tile pane in CSS
# (inst/app/www/app.css) rather than switching tile sets.

basemap <- function() {
  list(
    url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    attribution = paste0(
      '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors | ',
      'Valuations &copy; <a href="https://tailte.ie/home/api/">Tailte \u00c9ireann</a>, ',
      '<a href="https://creativecommons.org/licenses/by/4.0/">CC BY 4.0</a>'
    ),
    max_zoom = 19
  )
}

add_basemap <- function(map) {
  b <- basemap()
  addTiles(map, urlTemplate = b$url, attribution = b$attribution, options = tileOptions(maxZoom = b$max_zoom))
}


focus_lonlat <- function(f, props) {
  if (f$type == "property") c(props$lon[f$row], props$lat[f$row]) else c(f$lon, f$lat)
}

# Spokes, neighbour dots and the selected dot (or pin). `neighbours` is the
# reactive itself, read only when there is a focus.
draw_focus <- function(proxy, props, f, neighbours, col, point_labels) {
  proxy <- proxy |> clearGroup("focus")
  if (is.null(f)) {
    return(invisible(proxy))
  }
  nb <- neighbours()
  fx <- focus_lonlat(f, props)
  no_click <- pathOptions(interactive = FALSE)
  on_top <- pathOptions(bubblingMouseEvents = FALSE)

  if (nrow(nb)) {
    proxy <- proxy |>
      addPolylines(
        as.vector(rbind(fx[1], nb$lon, NA)), as.vector(rbind(fx[2], nb$lat, NA)),
        group = "focus", color = col$neighbour, weight = 1.5, opacity = 0.55, options = no_click
      ) |>
      addCircleMarkers(
        nb$lon, nb$lat,
        group = "focus", layerId = paste0("nb-", nb$property_number),
        radius = 6.5, stroke = TRUE, color = col$surface, weight = 2, opacity = 1,
        fillColor = col$neighbour, fillOpacity = 1, options = on_top,
        label = sprintf(
          "%d. %s, %s (%s)", nb$rank, nb$name,
          fmt_valuation(nb$valuation, nb$valuation_basis), fmt_distance(nb$distance_m)
        )
      )
  }
  if (f$type == "property") {
    proxy |> addCircleMarkers(
      fx[1], fx[2],
      group = "focus", layerId = paste0("sel-", props$property_number[f$row]),
      radius = 9, stroke = TRUE, color = col$surface, weight = 3, opacity = 1,
      fillColor = col$subject, fillOpacity = 1, options = on_top, label = point_labels[f$row]
    )
  } else {
    proxy |> addCircleMarkers(
      fx[1], fx[2],
      group = "focus", radius = 8, stroke = TRUE, color = col$subject, weight = 3,
      opacity = 1, fillColor = col$surface, fillOpacity = 1, options = no_click
    )
  }
  invisible(proxy)
}
