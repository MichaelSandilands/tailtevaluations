# Detail module: the selected property, its breakdown, neighbours and comparison

detail_ui <- function(id) {
  ns <- NS(id)
  card(
    class = "detail-card",
    card_body(
      uiOutput(ns("header")),
      navset_underline(
        id = ns("tabs"),
        nav_panel("Breakdown", value = "breakdown", uiOutput(ns("breakdown"))),
        nav_panel("Nearest", value = "nearest", uiOutput(ns("nearest"))),
        nav_panel("Compare", value = "compare", uiOutput(ns("compare")))
      )
    )
  )
}

#' @param n_requested Reactive number of neighbours asked for.
#' @param same_category Reactive logical.
#' @return A reactive with the property number of a neighbour picked from the
#'   table (an event; `NULL` until a row is clicked).
#' @noRd
detail_server <- function(id, vals, focus, neighbours, n_requested, same_category, mode) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    props <- vals$props

    observeEvent(focus(), if (identical(focus()$type, "pin")) nav_select("tabs", "nearest"))

    output$header <- renderUI({
      f <- focus()
      focus_header(f, props, if (identical(f$type, "pin")) nrow(neighbours()))
    })

    output$breakdown <- renderUI(breakdown_panel(focus(), props, ns))

    output$floors_table <- reactable::renderReactable({
      f <- focus()
      req(is_property(f))
      floors_table(vals$floors[vals$floors$property_number == props$property_number[f$row], , drop = FALSE])
    })

    output$nearest <- renderUI({
      f <- focus()
      if (is.null(f)) {
        return(empty_state("Select a property or click the map to list the nearest properties."))
      }
      nearest_panel(f, props, neighbours(), n_requested(), same_category(), ns)
    })

    output$nearest_table <- reactable::renderReactable({
      nb <- neighbours()
      req(nrow(nb) > 0)
      mixed <- !isTRUE(same_category()) || identical(focus()$type, "pin")
      nearest_table(nb, mixed = mixed, pick_input = ns("pick"))
    })

    comparison <- reactive({
      f <- focus()
      req(is_property(f))
      decompose_vs_neighbours(as.list(props[f$row, c("area_m2", "rate", "valuation")]), neighbours())
    })

    output$compare <- renderUI({
      if (!is_property(focus())) {
        return(empty_state("Select a property to see why its valuation differs from its neighbours."))
      }
      compare_panel(comparison(), ns)
    })

    output$waterfall <- plotly::renderPlotly({
      steps <- comparison()
      req(steps)
      waterfall_plot(steps, mode())
    })

    reactive(input$pick)
  })
}

is_property <- function(f) !is.null(f) && f$type == "property"

breakdown_panel <- function(f, props, ns) {
  if (!is_property(f)) {
    return(empty_state("Select a property to see how its valuation is built up, floor by floor."))
  }
  r <- props[f$row, ]
  if (r$valuation_basis == "RV") {
    return(empty_state(paste(
      "Cork City and County are still on the pre-revaluation list, so there's no floor-by-floor breakdown.",
      "The figure shown is a legacy Rateable Valuation, not a value in euro."
    )))
  }
  if (r$n_floors == 0) {
    return(empty_state(paste(
      "This property is valued as a whole, with no floor-by-floor breakdown.",
      "That's usual for utility, mineral and network properties."
    )))
  }
  tagList(reactable::reactableOutput(ns("floors_table")), p(class = "table-note", breakdown_note(r)))
}

nearest_panel <- function(f, props, nb, n_requested, same_category, ns) {
  scope <- if (is_property(f) && isTRUE(same_category) && !is.na(props$category[f$row])) {
    paste0(tolower(props$category_label[f$row]), " properties")
  } else {
    "properties"
  }
  note <- if (nrow(nb) < n_requested) {
    sprintf("Only %d %s match the current filters.", nrow(nb), scope)
  } else {
    sprintf("The %d nearest %s within the current filters. Click a row to select it.", nrow(nb), scope)
  }
  tagList(p(class = "table-note", note), reactable::reactableOutput(ns("nearest_table")))
}

compare_panel <- function(steps, ns) {
  if (is.null(steps)) {
    return(empty_state(paste(
      "The comparison needs floor areas for this property and at least three of its neighbours.",
      "Cork, utility and mineral properties don't have them."
    )))
  }
  t <- attr(steps, "typical")
  tagList(
    plotly::plotlyOutput(ns("waterfall"), height = "300px"),
    p(class = "table-note", sprintf(
      paste(
        "A typical neighbour is the median floor area (%s) at the median rate (%s)",
        "of the %d nearest with floor data. The steps show how much of the gap",
        "comes from size and how much from the rate per %s. This is the app's analysis of",
        "Tailte's figures, not a Tailte \u00c9ireann valuation."
      ),
      fmt_area(t$area_m2), fmt_rate(t$rate), t$n, SQM
    ))
  )
}

# Pieces ------------------------------------------------------------------------

empty_state <- function(text) p(class = "empty", text)

focus_header <- function(f, props, n_neighbours = NULL) {
  if (is.null(f)) {
    return(div(
      class = "focus-head",
      h2(class = "focus-name", "Pick a property"),
      p(
        class = "focus-lede",
        "Click any dot on the map, or search for a property by name, address, Eircode or number."
      ),
      p(class = "focus-lede", "Click empty map to list the properties nearest that spot.")
    ))
  }
  if (f$type == "pin") {
    return(div(
      class = "focus-head",
      h2(class = "focus-name", "Pinned location"),
      p(class = "focus-lede", sprintf(
        "The %d nearest properties are listed below. Click one for its valuation.", n_neighbours
      ))
    ))
  }
  r <- props[f$row, ]
  facts <- list(
    list(
      if (r$valuation_basis == "RV") "Rateable valuation" else "Valuation",
      fmt_valuation(r$valuation, r$valuation_basis)
    ),
    list("Rate", if (is.finite(r$rate)) fmt_rate(r$rate) else "Not given"),
    list("Floor area", if (is.finite(r$area_m2) && r$area_m2 > 0) fmt_area(r$area_m2) else "Not given")
  )
  use <- if (is.na(r$uses)) r$category_label else paste0(r$category_label, ", ", tolower(r$use_label))
  div(
    class = "focus-head",
    h2(class = "focus-name", r$name),
    p(class = "focus-address", paste(c(r$address, if (!is.na(r$eircode)) r$eircode), collapse = ", ")),
    tags$dl(class = "facts", lapply(facts, function(x) div(tags$dt(x[[1]]), tags$dd(x[[2]])))),
    p(class = "focus-meta", sprintf(
      "%s. %s. Property %s, published %s.",
      use, r$council_label, r$property_number, fmt_date(r$publication_date)
    ))
  )
}

breakdown_note <- function(r) {
  extra <- r$valuation - r$nav_floors
  sprintf(
    "Floors add up to %s. The official valuation of %s %s.",
    fmt_eur(r$nav_floors), fmt_eur(r$valuation),
    if (abs(extra) < 100) {
      "is that figure, rounded down"
    } else {
      sprintf("includes %s for additional items, after rounding", fmt_eur(extra))
    }
  )
}

table_theme <- function() {
  reactable::reactableTheme(
    color = "var(--tv-ink)", backgroundColor = "transparent", borderColor = "var(--tv-hairline)",
    stripedColor = "transparent", highlightColor = "var(--tv-row-hover)",
    headerStyle = list(color = "var(--tv-ink-2)", fontWeight = 600, fontSize = "0.8125rem"),
    footerStyle = list(fontWeight = 600),
    cellPadding = "6px 8px"
  )
}

floors_table <- function(fl) {
  fl <- fl[order(suppressWarnings(as.numeric(fl$level)), fl$level), ]
  df <- data.frame(level = fl$level, use = fl$floor_use_label, area = fl$area_m2, rate = fl$nav_per_m2, nav = fl$nav)
  reactable::reactable(
    df,
    theme = table_theme(), compact = TRUE, sortable = FALSE, pagination = FALSE,
    columns = list(
      level = reactable::colDef("Level", width = 52, footer = "Total"),
      use = reactable::colDef("Floor use", minWidth = 110),
      area = reactable::colDef(SQM,
        align = "right", width = 62, cell = function(v) fmt_int(v),
        footer = fmt_int(sum(df$area, na.rm = TRUE))
      ),
      rate = reactable::colDef(paste0(EURO, "/", SQM),
        align = "right", width = 62, cell = function(v) fmt_int(v),
        footer = fmt_int(sum(df$nav, na.rm = TRUE) / sum(df$area, na.rm = TRUE))
      ),
      nav = reactable::colDef("NAV",
        align = "right", width = 92, cell = function(v) fmt_eur(v),
        footer = fmt_eur(sum(df$nav, na.rm = TRUE))
      )
    )
  )
}

# Mixed types (pinned location, or the same-type filter off): name the type
# under each property rather than spending a column on it
nearest_table <- function(nb, mixed, pick_input) {
  sub <- ifelse(is.na(nb$address), "", nb$address)
  if (mixed) sub <- ifelse(nzchar(sub), paste0(nb$category_label, ". ", sub), nb$category_label)
  df <- data.frame(
    property_number = nb$property_number, rank = nb$rank, distance = nb$distance_m,
    name = nb$name, sub = sub, valuation = nb$valuation, basis = nb$valuation_basis, rate = nb$rate
  )
  on_click <- sprintf(
    "function(row) { Shiny.setInputValue('%s', row.original.property_number, {priority: 'event'}); }",
    pick_input
  )
  reactable::reactable(
    df,
    theme = table_theme(), compact = TRUE, highlight = TRUE, pagination = FALSE,
    height = if (nrow(df) > 12) 440 else "auto",
    onClick = reactable::JS(on_click),
    rowStyle = list(cursor = "pointer"),
    columns = list(
      property_number = reactable::colDef(show = FALSE),
      sub = reactable::colDef(show = FALSE),
      basis = reactable::colDef(show = FALSE),
      rank = reactable::colDef("", width = 30, align = "right"),
      distance = reactable::colDef("Distance", width = 72, align = "right", cell = function(v) fmt_distance(v)),
      name = reactable::colDef("Property", minWidth = 130, html = TRUE, cell = function(v, i) {
        sprintf(
          "<div class='cell-name'>%s</div><div class='cell-sub'>%s</div>",
          htmltools::htmlEscape(v), htmltools::htmlEscape(df$sub[i])
        )
      }),
      valuation = reactable::colDef("Valuation",
        align = "right", width = 88,
        cell = function(v, i) fmt_valuation(v, df$basis[i])
      ),
      rate = reactable::colDef(paste0(EURO, "/", SQM), align = "right", width = 58, cell = function(v) fmt_int(v))
    )
  )
}
