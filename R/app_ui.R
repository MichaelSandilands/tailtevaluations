# App UI -----------------------------------------------------------------------

app_ui <- function(vals, options = app_options()) {
  page_sidebar(
    title = div(
      class = "app-header",
      h1(class = "app-title", "Irish commercial valuations"),
      div(
        class = "app-search",
        selectizeInput(
          "search", NULL,
          choices = NULL, width = "100%",
          options = list(
            placeholder = "Search by name, address, Eircode or property number",
            searchField = c("label", "address", "eircode", "value"),
            maxOptions = 30,
            render = I(search_render_js())
          )
        )
      ),
      input_dark_mode(id = "dark_mode")
    ),
    theme = app_theme(),
    fillable = TRUE,
    window_title = "Irish commercial valuations",
    sidebar = sidebar(
      width = 300,
      open = list(desktop = "open", mobile = "closed"), # on phones, filters slide over the map
      filters_ui("filters", vals),
      div(class = "sidebar-section-head", h2("Nearest properties")),
      sliderInput("n", "How many",
        min = 1, max = options$max_neighbours, value = options$default_n, step = 1, ticks = FALSE
      ),
      checkboxInput("same_category", "Same property type only", TRUE),
      sidebar_footer(vals)
    ),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(7, 5)),
      fill = TRUE,
      map_ui("map"),
      detail_ui("detail")
    ),
    tags$head(tags$link(rel = "stylesheet", href = "tailte-assets/app.css"))
  )
}

app_theme <- function() {
  bs_theme(
    version = 5,
    primary = "#1c5cab",
    base_font = font_collection(
      font_google("Instrument Sans", local = FALSE),
      "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
    ),
    "border-radius" = "0.375rem",
    "card-box-shadow" = "none"
  )
}

sidebar_footer <- function(vals) {
  a <- data_attribution()
  div(
    class = "sidebar-foot",
    if (!is.null(vals$updated)) p(paste0("Data updated ", fmt_date(vals$updated), ".")),
    p(
      "Contains Irish Public Sector Data (",
      ext_link(a$source_url, "Tailte \u00c9ireann"),
      ") licensed under ",
      ext_link(a$licence_url, a$licence),
      ". Modified by this app; not endorsed by Tailte \u00c9ireann. ",
      actionLink("about_data", "About the data"),
      .noWS = "inside"
    ),
    if (vals$n_unmapped > 0) {
      p(sprintf("%s properties without coordinates aren't shown.", fmt_int(vals$n_unmapped)))
    }
  )
}

about_data_modal <- function(vals) {
  a <- data_attribution()
  modalDialog(
    title = "About the data",
    easyClose = TRUE,
    footer = modalButton("Close"),
    p(paste0(a$statement, " ", a$copyright, ".")),
    p("Source: ", ext_link(a$source_url, a$source), ". Licence: ", ext_link(a$licence_url, a$licence_url), ".",
      .noWS = "inside"
    ),
    h3(class = "modal-subhead", "What this app changes"),
    tags$ul(lapply(a$changes, tags$li)),
    p(paste(
      "Floor areas, rates per m\u00b2, nearest neighbours and the Compare chart are this app's",
      "calculations from Tailte's figures. They are not Tailte \u00c9ireann valuations."
    )),
    p(paste(
      "Cork City and County are still on the pre-revaluation list, so their figures are",
      "legacy Rateable Valuations, not Net Annual Values in euro."
    )),
    h3(class = "modal-subhead", "Accuracy"),
    p(a$disclaimer),
    p(
      "Spotted an error in a property's location or Eircode? Report it to Tailte \u00c9ireann at ",
      tags$a(href = paste0("mailto:", a$errors_contact), a$errors_contact),
      ". Corrections appear here after the next refresh.",
      .noWS = "inside"
    ),
    if (!is.null(vals$updated)) p(class = "focus-meta", paste0("Data last changed ", fmt_date(vals$updated), "."))
  )
}

ext_link <- function(href, text) {
  tags$a(href = href, target = "_blank", rel = "noopener", text, .noWS = "outside")
}

search_render_js <- function() {
  "{
    option: function(item, escape) {
      var meta = [item.address, item.eircode].filter(Boolean).map(escape).join(', ');
      return '<div class=\"opt\"><div class=\"opt-name\">' + escape(item.label) + '</div>' +
             '<div class=\"opt-meta\">' + meta + '</div></div>';
    }
  }"
}
