# Parse raw GeoJSON into two tidy tables ---------------------------------------
#
# The API's GeoJSON carries Irish Transverse Mercator coordinates but no "crs"
# member. RFC 7946 says GeoJSON without one is WGS84, so sf::st_read() would
# silently label metres as degrees. Parsing with jsonlite and declaring
# EPSG:2157 ourselves avoids that trap.

#' Parse an API response into tidy tables
#'
#' `tidy_properties()` returns one row per property with ITM point
#' coordinates; `tidy_floors()` returns one row per floor line of the
#' valuation report. Both always return the same columns and types, whatever
#' fields a particular authority's response is missing, so per-authority
#' tables combine cleanly.
#'
#' @param body GeoJSON text, as returned by [fetch_authority()].
#' @param authority The authority that was queried. Used as the
#'   `local_authority` value, so counts always match the query.
#' @return A data frame. See [empty_properties()] and [empty_floors()] for
#'   the columns.
#' @export
#' @examples
#' path <- system.file("extdata", "carlow_sample.geojson", package = "tailtevaluations")
#' body <- readChar(path, file.size(path), useBytes = TRUE)
#' tidy_properties(body, "CARLOW COUNTY COUNCIL")
#' tidy_floors(body)
tidy_properties <- function(body, authority) {
  feats <- read_features(body)
  if (NROW(feats) == 0) {
    return(empty_properties())
  }

  p <- lower_names(feats$properties)
  xy <- feature_xy(feats$geometry, nrow(p))

  data.frame(
    property_number = as_int(field(p, "propertynumber")),
    name = as_chr(field(p, "name")),
    address = as_chr(field(p, "address")),
    eircode = as_chr(field(p, "eircode")),
    county = as_chr(field(p, "county")),
    local_authority = authority,
    category = as_chr(field(p, "category")),
    uses = as_chr(field(p, "uses")),
    valuation = as_num(field(p, "valuation")),
    valuation_basis = if (authority %in% pre_revaluation_authorities()) "RV" else "NAV",
    additional_items = as_num(field(p, "additionalitems")),
    car_park = as_num(field(p, "carpark")),
    publication_date = as_date_ie(field(p, "publicationdate")),
    x_itm = xy[, 1],
    y_itm = xy[, 2],
    stringsAsFactors = FALSE
  )
}

#' @rdname tidy_properties
#' @export
tidy_floors <- function(body) {
  feats <- read_features(body)
  if (NROW(feats) == 0) {
    return(empty_floors())
  }

  p <- lower_names(feats$properties)
  vr <- p[["valuationreport"]]
  if (is.null(vr)) {
    return(empty_floors())
  }

  has <- vapply(vr, function(d) is.data.frame(d) && nrow(d) > 0, logical(1))
  if (!any(has)) {
    return(empty_floors())
  }

  d <- data.table::rbindlist(lapply(vr[has], lower_names), fill = TRUE, idcol = "i")
  pn <- as_int(field(p, "propertynumber"))[has]

  data.frame(
    property_number = pn[d$i],
    level = as_chr(field(d, "level")),
    floor_use = as_chr(field(d, "flooruse")),
    area_m2 = as_num(field(d, "area")),
    nav = as_num(field(d, "nav")),
    nav_per_m2 = as_num(field(d, "navperm2")),
    stringsAsFactors = FALSE
  )
}

#' Empty tables with the tidy schema
#'
#' The column names and types every per-authority table has.
#'
#' @return A zero-row data frame.
#' @export
#' @examples
#' str(empty_properties())
#' str(empty_floors())
empty_properties <- function() {
  data.frame(
    property_number = integer(), name = character(), address = character(),
    eircode = character(), county = character(), local_authority = character(),
    category = character(), uses = character(), valuation = double(),
    valuation_basis = character(), additional_items = double(), car_park = double(),
    publication_date = as.Date(character()), x_itm = double(), y_itm = double(),
    stringsAsFactors = FALSE
  )
}

#' @rdname empty_properties
#' @export
empty_floors <- function() {
  data.frame(
    property_number = integer(), level = character(), floor_use = character(),
    area_m2 = double(), nav = double(), nav_per_m2 = double(),
    stringsAsFactors = FALSE
  )
}

#' Combine per-authority tables
#'
#' `finalise_properties()` drops missing and repeated property numbers, sorts
#' by property number (so the result, and its hash, doesn't depend on the
#' order the API returned rows in) and adds WGS84 `lon`/`lat` for web maps.
#' `finalise_floors()` keeps floors of the surviving properties, sorted.
#'
#' @param properties Row-bound output of [tidy_properties()].
#' @param floors Row-bound output of [tidy_floors()].
#' @return A data frame.
#' @export
#' @examples
#' path <- system.file("extdata", "carlow_sample.geojson", package = "tailtevaluations")
#' body <- readChar(path, file.size(path), useBytes = TRUE)
#' props <- finalise_properties(tidy_properties(body, "CARLOW COUNTY COUNCIL"))
#' props[, c("property_number", "lon", "lat")]
#' finalise_floors(tidy_floors(body), props)
finalise_properties <- function(properties) {
  dup <- duplicated(properties$property_number) | is.na(properties$property_number)
  if (any(dup)) {
    cli::cli_warn("Dropping {sum(dup)} row{?s} with a missing or repeated property number.")
  }
  out <- properties[!dup, , drop = FALSE]
  out <- out[order(out$property_number), , drop = FALSE]
  rownames(out) <- NULL

  out$lon <- NA_real_
  out$lat <- NA_real_
  has_xy <- !is.na(out$x_itm) & !is.na(out$y_itm)
  if (any(has_xy)) {
    pts <- sf::st_as_sf(out[has_xy, c("x_itm", "y_itm")], coords = c("x_itm", "y_itm"), crs = ITM)
    ll <- sf::st_coordinates(sf::st_transform(pts, 4326))
    out$lon[has_xy] <- round(ll[, "X"], 6)
    out$lat[has_xy] <- round(ll[, "Y"], 6)
  }
  out
}

#' @rdname finalise_properties
#' @export
finalise_floors <- function(floors, properties) {
  out <- unique(floors[floors$property_number %in% properties$property_number, , drop = FALSE])
  out <- out[order(out$property_number, out$level), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Properties as an sf object in Irish Transverse Mercator
#'
#' ITM is the right CRS for distance work: `nngeo::st_nn()` on projected
#' points takes its fast k-d tree path. Rows without coordinates are dropped.
#'
#' @param properties A table with `x_itm` and `y_itm` columns.
#' @return An `sf` data frame with EPSG:2157 point geometry.
#' @export
#' @examples
#' path <- system.file("extdata", "carlow_sample.geojson", package = "tailtevaluations")
#' body <- readChar(path, file.size(path), useBytes = TRUE)
#' as_sf_itm(tidy_properties(body, "CARLOW COUNTY COUNCIL"))
as_sf_itm <- function(properties) {
  keep <- !is.na(properties$x_itm) & !is.na(properties$y_itm)
  sf::st_as_sf(properties[keep, ], coords = c("x_itm", "y_itm"), crs = ITM, remove = FALSE)
}

# helpers ----------------------------------------------------------------------

read_features <- function(body) {
  fc <- jsonlite::fromJSON(body, simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = FALSE)
  if (!identical(fc$type, "FeatureCollection")) {
    cli::cli_abort("Response is not a GeoJSON FeatureCollection.")
  }
  if (length(fc$features) == 0) {
    return(NULL)
  }
  fc$features
}

# Point coordinates as an n x 2 matrix. Null geometries and 0/negative
# placeholders become NA rather than points off the coast of Africa.
feature_xy <- function(geometry, n) {
  out <- matrix(NA_real_, n, 2)
  co <- if (is.data.frame(geometry)) geometry$coordinates
  if (is.null(co)) {
    return(out)
  }
  ok <- vapply(co, function(pt) is.numeric(pt) && length(pt) >= 2, logical(1))
  if (any(ok)) out[ok, ] <- do.call(rbind, lapply(co[ok], function(pt) as.numeric(pt[1:2])))
  bad <- rowSums(is.na(out) | out <= 0) > 0
  out[bad, ] <- NA_real_
  out
}

lower_names <- function(df) {
  names(df) <- tolower(names(df))
  df
}

field <- function(df, name) {
  if (name %in% names(df)) df[[name]] else rep(NA, NROW(df))
}

as_chr <- function(x) {
  x <- trimws(as.character(unlist_scalar(x)))
  x[x == ""] <- NA_character_
  x
}

as_num <- function(x) suppressWarnings(as.numeric(as.character(unlist_scalar(x))))

as_int <- function(x) suppressWarnings(as.integer(as.character(unlist_scalar(x))))

# Dates arrive as dd/mm/yyyy; accept ISO too in case the API changes.
as_date_ie <- function(x) {
  x <- as_chr(x)
  d <- as.Date(substr(x, 1, 10), format = "%d/%m/%Y")
  iso <- is.na(d) & grepl("^\\d{4}-\\d{2}-\\d{2}", x)
  d[iso] <- as.Date(substr(x[iso], 1, 10))
  d
}

# jsonlite leaves a list column when some values are null; flatten those.
unlist_scalar <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  vapply(x, function(v) if (length(v) == 0) NA_character_ else as.character(v[[1]]), character(1))
}
