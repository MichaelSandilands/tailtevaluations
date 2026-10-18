# Load and prepare the published dataset for the app ---------------------------

#' Load the published valuations for the app
#'
#' Reads the pipeline's outputs and prepares everything the app needs, once
#' per R process. `source` is either a folder or the published `latest` URL
#' in the bucket, e.g. `https://data.example.com/tailte-valuations/latest`.
#'
#' @param source Folder or URL. Defaults to the `VALUATIONS_DATA` environment
#'   variable, then a `data/` folder in the working directory.
#' @return A `tailte_valuations` object (see [prepare_valuations()]).
#' @export
#' @examples
#' path <- system.file("extdata", "carlow_sample.geojson", package = "tailtevaluations")
#' body <- readChar(path, file.size(path), useBytes = TRUE)
#' props <- finalise_properties(tidy_properties(body, "CARLOW COUNTY COUNCIL"))
#' floors <- finalise_floors(tidy_floors(body), props)
#' out <- file.path(tempdir(), "valuations-example")
#' write_outputs(props, floors, validate_refresh(props, floors, "CARLOW COUNTY COUNCIL"), out)
#' load_valuations(out)
load_valuations <- function(source = data_source()) {
  props <- as.data.frame(nanoparquet::read_parquet(fetch_file(source, "properties.parquet")))
  floors <- as.data.frame(nanoparquet::read_parquet(fetch_file(source, "floors.parquet")))
  manifest <- tryCatch(jsonlite::read_json(fetch_file(source, "manifest.json")), error = function(e) NULL)
  prepare_valuations(props, floors, manifest)
}

#' Prepare valuation tables for the app
#'
#' Keeps properties with coordinates, adds per-property floor totals (area,
#' NAV from floors and NAV per m2), display labels, a stable drawing order,
#' an ITM geometry for neighbour searches and the choices for each filter.
#'
#' @param props,floors Tables as written by [write_outputs()].
#' @param manifest The parsed `manifest.json`, or `NULL`.
#' @return A list of class `tailte_valuations`.
#' @export
#' @examples
#' path <- system.file("extdata", "carlow_sample.geojson", package = "tailtevaluations")
#' body <- readChar(path, file.size(path), useBytes = TRUE)
#' props <- finalise_properties(tidy_properties(body, "CARLOW COUNTY COUNCIL"))
#' prepare_valuations(props, finalise_floors(tidy_floors(body), props))
prepare_valuations <- function(props, floors, manifest = NULL) {
  n_all <- nrow(props)
  props <- props[!is.na(props$x_itm) & !is.na(props$y_itm) & !is.na(props$lon), , drop = FALSE]
  rownames(props) <- NULL

  key <- factor(floors$property_number, levels = props$property_number)
  ok <- !is.na(key)
  pn <- as.character(props$property_number)
  area <- tapply(floors$area_m2[ok], key[ok], sum, na.rm = TRUE)
  nav <- tapply(floors$nav[ok], key[ok], sum, na.rm = TRUE)
  n_fl <- tapply(floors$nav[ok], key[ok], length)
  props$area_m2 <- as.numeric(area[pn])
  props$nav_floors <- as.numeric(nav[pn])
  props$n_floors <- as.integer(n_fl[pn])
  props$n_floors[is.na(props$n_floors)] <- 0L
  props$rate <- ifelse(is.finite(props$area_m2) & props$area_m2 > 0, props$nav_floors / props$area_m2, NA_real_)

  props$council_label <- council_label(props$local_authority)
  props$category_label <- ifelse(is.na(props$category), "Not recorded", sentence_case(props$category))
  props$use_label <- ifelse(is.na(props$uses), "Not recorded", sentence_case(props$uses))

  # Stable "which points to draw first" order, so panning doesn't reshuffle
  props$draw_rank <- withr::with_seed(20261017, sample.int(nrow(props)))

  floors <- floors[floors$property_number %in% props$property_number, , drop = FALSE]
  floors$floor_use_label <- sentence_case(floors$floor_use)

  councils <- unique(props[c("local_authority", "council_label")])
  councils <- councils[order(councils$council_label), ]
  dates <- props$publication_date[!is.na(props$publication_date)]

  structure(
    list(
      props = props,
      floors = floors,
      geom = sf::st_as_sf(props[c("x_itm", "y_itm")], coords = c("x_itm", "y_itm"), crs = ITM),
      xy = cbind(props$x_itm, props$y_itm),
      row_of = stats::setNames(seq_len(nrow(props)), pn),
      floor_use_index = lapply(split(floors$property_number, floors$floor_use_label), unique),
      point_labels = paste0(props$name, ", ", fmt_valuation(props$valuation, props$valuation_basis)),
      search_choices = data.frame(
        value = pn,
        label = ifelse(is.na(props$name), paste("Property", pn), props$name),
        address = ifelse(is.na(props$address), "", props$address),
        eircode = ifelse(is.na(props$eircode), "", props$eircode)
      ),
      choices = list(
        council = stats::setNames(councils$local_authority, councils$council_label),
        category = sort(unique(props$category_label)),
        floor_use = sort(unique(floors$floor_use_label))
      ),
      date_range = if (length(dates)) range(dates) else rep(Sys.Date(), 2),
      updated = if (!is.null(manifest$updated_at)) as.Date(substr(manifest$updated_at, 1, 10)),
      n_unmapped = n_all - nrow(props)
    ),
    class = "tailte_valuations"
  )
}

#' @export
print.tailte_valuations <- function(x, ...) {
  cli::cli_text(
    "{.cls tailte_valuations}: {format(nrow(x$props), big.mark = ',')} mapped properties, ",
    "{format(nrow(x$floors), big.mark = ',')} floor lines, ",
    "{length(x$choices$council)} council{?s}."
  )
  if (!is.null(x$updated)) cli::cli_text("Data updated {format(x$updated)}.")
  invisible(x)
}

data_source <- function() {
  src <- Sys.getenv("VALUATIONS_DATA")
  if (nzchar(src)) {
    return(src)
  }
  if (file.exists(file.path("data", "properties.parquet"))) {
    return("data")
  }
  cli::cli_abort(c(
    "No valuation data found.",
    i = "Run the pipeline ({.code targets::tar_make()}) to write {.path data/}, or",
    i = "set {.envvar VALUATIONS_DATA} to the published {.path latest} URL or a folder holding the files."
  ))
}

fetch_file <- function(src, name) {
  if (!grepl("^https?://", src)) {
    return(file.path(src, name))
  }
  dest <- file.path(tempdir(), name)
  utils::download.file(paste0(sub("/+$", "", src), "/", name), dest, mode = "wb", quiet = TRUE)
  dest
}

council_label <- function(x) {
  out <- tools::toTitleCase(tolower(x))
  out <- sub(" Co Co$", " County Council", out)
  out <- gsub(" And ", " and ", out, fixed = TRUE)
  sub("^Dun Laoghaire Rathdown", "D\u00fan Laoghaire-Rathdown", out)
}

sentence_case <- function(x) {
  x <- tolower(x)
  paste0(toupper(substr(x, 1, 1)), substring(x, 2))
}
