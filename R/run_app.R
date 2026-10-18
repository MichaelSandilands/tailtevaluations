#' Run the valuations explorer
#'
#' A Shiny app over the pipeline's outputs: filter properties, click one (or
#' search) to see its valuation and floor-by-floor breakdown, list its n
#' nearest neighbours, and see why its valuation differs from theirs.
#'
#' @param data Where the data comes from: a folder or URL holding the
#'   pipeline's outputs (see [load_valuations()]), or an object returned by
#'   [load_valuations()] or [prepare_valuations()]. Defaults to the
#'   `VALUATIONS_DATA` environment variable, then `./data`.
#' @param max_neighbours Cap on the number of neighbours a user can ask for.
#'   Keeps the search, map layer and tables light.
#' @param max_map_points Cap on background dots drawn per map view; zooming
#'   in reveals the rest.
#' @param ... Passed to [shiny::shinyApp()], e.g. `options = list(port = 8080)`.
#' @return A Shiny app object. Printing it, or calling `run_app()` at the
#'   console, starts the app.
#' @export
#' @examplesIf interactive()
#' # After targets::tar_make() has written data/
#' run_app()
#'
#' # Straight from the published bucket
#' run_app("https://data.example.com/tailte-valuations/latest")
run_app <- function(data = NULL, max_neighbours = 50L, max_map_points = 4000L, ...) {
  use_utf8_locale()
  vals <- if (inherits(data, "tailte_valuations")) data else load_valuations(if (is.null(data)) data_source() else data)
  options <- app_options(max_neighbours, max_map_points)
  addResourcePath("tailte-assets", system.file("app", "www", package = "tailtevaluations"))
  shinyApp(app_ui(vals, options), app_server(vals, options), ...)
}

# Labels use the euro sign, m-squared and accented council names. Containers
# often start R in the C locale, which would garble them.
use_utf8_locale <- function() {
  if (!isTRUE(l10n_info()$`UTF-8`)) {
    invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
  }
}
