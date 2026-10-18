#' @section The two halves:
#' * **Pipeline.** `_targets.R` (in the project root, not the installed
#'   package) strings together [local_authorities()], [fetch_authority()],
#'   [tidy_properties()], [tidy_floors()], [validate_refresh()],
#'   [write_outputs()] and [publish_outputs()] into a scheduled refresh.
#' * **App.** [run_app()] serves a Shiny app over the published files:
#'   filters, the n nearest neighbours of any property ([nearest_rows()]) and
#'   a size-versus-rate comparison ([decompose_vs_neighbours()]).
#'
#' @keywords internal
#' @import shiny
#' @import bslib
#' @import leaflet
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' Irish Transverse Mercator (IRENET95 / ITM), the API's coordinate system
#' @noRd
ITM <- 2157L
