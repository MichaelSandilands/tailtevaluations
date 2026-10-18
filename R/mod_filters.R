# Filters module: the sidebar's property filters --------------------------------

filters_ui <- function(id, vals) {
  ns <- NS(id)
  ch <- vals$choices
  tagList(
    div(
      class = "sidebar-section-head",
      h2("Filter properties"),
      actionLink(ns("reset"), "Reset")
    ),
    selectizeInput(ns("council"), "Council", ch$council, multiple = TRUE, options = multi_options("All councils")),
    selectizeInput(ns("category"), "Property type", ch$category, multiple = TRUE, options = multi_options("All types")),
    selectizeInput(ns("use"), "Use", NULL, multiple = TRUE, options = multi_options("All uses")),
    selectizeInput(ns("floor_use"), "Has a floor used as", ch$floor_use,
      multiple = TRUE, options = multi_options("Any floor use")
    ),
    range_inputs(ns("nav"), paste0("Valuation (", EURO, ")"), step = 1000),
    range_inputs(ns("rate"), paste0("Rate (", EURO, " per ", SQM, ")"), step = 10),
    range_inputs(ns("area"), paste0("Floor area (", SQM, ")"), step = 50),
    dateRangeInput(ns("published"), "Published",
      start = vals$date_range[1], end = vals$date_range[2],
      min = vals$date_range[1], max = vals$date_range[2],
      format = "d M yyyy", separator = "to"
    ),
    numericInput(ns("car_min"), "Car spaces, at least", 0, min = 0, step = 1)
  )
}

#' @return A debounced reactive list of filter values for [filter_properties()].
#' @noRd
filters_server <- function(id, vals, debounce_ms = 350) {
  moduleServer(id, function(input, output, session) {
    props <- vals$props

    # Use choices follow the selected property types
    observeEvent(input$category,
      {
        in_types <- !length(input$category) | props$category_label %in% input$category
        pool <- props$use_label[in_types]
        choices <- sort(unique(pool))
        updateSelectizeInput(session, "use", choices = choices, selected = intersect(input$use, choices))
      },
      ignoreNULL = FALSE
    )

    observeEvent(input$reset, {
      for (id in c("council", "category", "use", "floor_use")) {
        updateSelectizeInput(session, id, selected = character(0))
      }
      for (id in c("nav_min", "nav_max", "rate_min", "rate_max", "area_min", "area_max")) {
        updateNumericInput(session, id, value = NA)
      }
      updateDateRangeInput(session, "published", start = vals$date_range[1], end = vals$date_range[2])
      updateNumericInput(session, "car_min", value = 0)
    })

    filters <- reactive({
      published <- input$published
      if (length(published) == 2 && isTRUE(all(published == vals$date_range))) published <- NULL
      list(
        council = input$council, category = input$category, use = input$use, floor_use = input$floor_use,
        nav_min = input$nav_min, nav_max = input$nav_max,
        rate_min = input$rate_min, rate_max = input$rate_max,
        area_min = input$area_min, area_max = input$area_max,
        published = published, car_min = input$car_min
      )
    })
    debounce(filters, debounce_ms)
  })
}

multi_options <- function(placeholder) {
  list(placeholder = placeholder, plugins = list("remove_button"))
}

range_inputs <- function(id, label, step = 100) {
  div(
    class = "range-inputs",
    tags$label(class = "control-label", label),
    div(
      class = "range-pair",
      numericInput(paste0(id, "_min"), NULL, NA, min = 0, step = step),
      span(class = "range-sep", "to"),
      numericInput(paste0(id, "_max"), NULL, NA, min = 0, step = step)
    )
  )
}
