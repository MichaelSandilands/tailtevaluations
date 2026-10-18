# Attribution and re-use terms -------------------------------------------------
#
# The Valuation API is published on data.gov.ie under CC BY 4.0, and Tailte
# Eireann's re-use conditions ask re-users to acknowledge the source and its
# copyright, reproduce the data accurately and not mislead. CC BY 4.0 also
# asks for a link to the licence, a note of any changes, and no suggestion of
# endorsement. Everything that credits the data (the app, the manifest and
# the ATTRIBUTION.txt published with the files) reads it from here.

#' How to credit the valuation data
#'
#' The attribution the app shows and the published files carry. The statement
#' is data.gov.ie's recommended wording for CC BY 4.0 public sector data,
#' naming Tailte Eireann as the provider.
#'
#' @return A list with `statement`, `copyright`, `licence`, `licence_url`,
#'   `source`, `source_url`, `changes` (how this project modifies the data),
#'   `disclaimer` and `errors_contact`.
#' @export
#' @examples
#' data_attribution()$statement
#' data_attribution()$changes
data_attribution <- function() {
  list(
    statement = paste(
      "Contains Irish Public Sector Data (Tailte \u00c9ireann) licensed under a",
      "Creative Commons Attribution 4.0 International (CC BY 4.0) licence."
    ),
    copyright = "\u00a9 Tailte \u00c9ireann",
    licence = "CC BY 4.0",
    licence_url = "https://creativecommons.org/licenses/by/4.0/",
    source = "Tailte \u00c9ireann Valuation API",
    source_url = "https://tailte.ie/home/api/",
    changes = c(
      paste(
        "Combined the responses for all 31 rating authorities, and split the floor-by-floor",
        "valuation report into its own table."
      ),
      "Added longitude and latitude (WGS84) by reprojecting the Irish Transverse Mercator coordinates.",
      "Treated zero or missing coordinates as missing, and removed records with no property number or listed twice.",
      "Dated snapshots leave out the occupier name."
    ),
    disclaimer = paste(
      "Tailte \u00c9ireann does not endorse this project, and gives no guarantee that the data are",
      "complete, current or free of errors. The API is a work in progress and may change."
    ),
    errors_contact = "opendata@tailte.ie"
  )
}

# Plain-text notice published alongside the data files
attribution_text <- function(a = data_attribution()) {
  c(
    "Tailte \u00c9ireann commercial valuations",
    "",
    a$statement,
    paste0(a$copyright, ". Source: ", a$source, ", ", a$source_url),
    paste0("Licence: ", a$licence_url),
    "",
    "Changes made by this project:",
    paste("-", a$changes),
    "",
    a$disclaimer,
    paste0("Report errors in the data (for example a property's location or Eircode) to ", a$errors_contact, "."),
    "",
    "Cork City and Cork County records carry a legacy Rateable Valuation (RV), not a Net Annual Value in euro."
  )
}
