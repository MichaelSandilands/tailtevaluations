# Filtering, nearest neighbours and the comparison ----------------------------
# Pure functions: everything here is testable without Shiny.

#' Apply the app's filters
#'
#' @param props The `props` table of a [prepare_valuations()] object.
#' @param f A list of filter values, any of which may be `NULL` or `NA`
#'   (meaning "no filter"): `council`, `category`, `use`, `floor_use`,
#'   `nav_min`/`nav_max`, `rate_min`/`rate_max`, `area_min`/`area_max`,
#'   `published` (two dates) and `car_min`.
#' @param floor_use_index Named list of property numbers by floor use.
#' @return A logical vector, one element per row of `props`.
#' @details Valuation bounds are in euro, so setting either drops Cork's
#'   legacy Rateable Valuation records. Once a bound is set, rows with a
#'   missing value for that field are excluded.
#' @export
#' @examples
#' props <- data.frame(
#'   local_authority = "CARLOW COUNTY COUNCIL", category_label = c("Office", "Retail (shops)"),
#'   use_label = "Shop", property_number = 1:2, valuation = c(20000, 9000),
#'   valuation_basis = "NAV", area_m2 = c(200, 80), rate = c(100, 112),
#'   publication_date = as.Date("2024-06-10"), car_park = c(4, 0)
#' )
#' filter_properties(props, list(category = "Office"))
#' filter_properties(props, list(nav_min = 10000, car_min = NA))
filter_properties <- function(props, f, floor_use_index = list()) {
  keep <- rep(TRUE, nrow(props))
  if (length(f$council)) keep <- keep & props$local_authority %in% f$council
  if (length(f$category)) keep <- keep & props$category_label %in% f$category
  if (length(f$use)) keep <- keep & props$use_label %in% f$use
  if (length(f$floor_use)) {
    with_use <- unique(unlist(floor_use_index[f$floor_use], use.names = FALSE))
    keep <- keep & props$property_number %in% with_use
  }
  if (is_set(f$nav_min) || is_set(f$nav_max)) keep <- keep & props$valuation_basis == "NAV"
  keep <- keep & in_range(props$valuation, f$nav_min, f$nav_max)
  keep <- keep & in_range(props$area_m2, f$area_min, f$area_max)
  keep <- keep & in_range(props$rate, f$rate_min, f$rate_max)
  if (length(f$published) == 2 && !all(is.na(f$published))) {
    keep <- keep & in_range(props$publication_date, f$published[1], f$published[2])
  }
  if (is_set(f$car_min) && f$car_min > 0) {
    keep <- keep & !is.na(props$car_park) & props$car_park >= f$car_min
  }
  keep
}

is_set <- function(x) length(x) == 1 && !is.na(x)

in_range <- function(x, lo, hi) {
  keep <- rep(TRUE, length(x))
  if (is_set(lo)) keep <- keep & !is.na(x) & x >= lo
  if (is_set(hi)) keep <- keep & !is.na(x) & x <= hi
  keep
}

#' Candidate rows for a neighbour search
#'
#' The filtered set, minus the subject, restricted to the subject's valuation
#' basis (never compare RV with NAV) and, optionally, to its category. With no
#' subject (a pinned location) every filtered row is a candidate.
#'
#' @param props The `props` table of a [prepare_valuations()] object.
#' @param keep Logical vector from [filter_properties()].
#' @param subject_row Row of the selected property, or `NULL`.
#' @param same_category Restrict to the subject's category?
#' @return Integer row indices.
#' @export
#' @examples
#' props <- data.frame(
#'   category = c("OFFICE", "OFFICE", "RETAIL (SHOPS)"), valuation_basis = "NAV"
#' )
#' neighbour_candidates(props, rep(TRUE, 3), subject_row = 1)
#' neighbour_candidates(props, rep(TRUE, 3), subject_row = 1, same_category = FALSE)
neighbour_candidates <- function(props, keep, subject_row = NULL, same_category = TRUE) {
  ok <- keep
  if (!is.null(subject_row)) {
    ok[subject_row] <- FALSE
    ok <- ok & props$valuation_basis == props$valuation_basis[subject_row]
    if (same_category && !is.na(props$category[subject_row])) {
      ok <- ok & !is.na(props$category) & props$category == props$category[subject_row]
    }
  }
  which(ok)
}

#' The k nearest candidates to a point
#'
#' Uses `nngeo::st_nn()`, which on projected points builds a k-d tree over
#' whatever it is given. Handing it 200,000 candidates costs about half a
#' second per click, so when `coords` is supplied the candidates are first
#' narrowed to a square window around the point. The answer is still exact:
#' once the k-th nearest point inside a window of half-width R is no more
#' than R away, nothing outside the window can be closer; otherwise the
#' window grows and the search repeats.
#'
#' @param xy The point, as ITM `c(x, y)` in metres.
#' @param candidates Row indices into `geom` to search among.
#' @param geom An `sf` point object in a projected CRS.
#' @param k Number of neighbours; capped at the number of candidates.
#' @param coords Optional n x 2 coordinate matrix matching `geom`, which
#'   enables the windowed search.
#' @param window_m Starting half-width of the window, in metres.
#' @return A data frame with `row` (into `geom`) and `distance_m`, nearest
#'   first.
#' @export
#' @examples
#' xy <- cbind(c(673000, 673100, 673050, 674000), c(677000, 677000, 677050, 677000))
#' geom <- sf::st_as_sf(data.frame(x = xy[, 1], y = xy[, 2]), coords = c("x", "y"), crs = 2157)
#' nearest_rows(c(673000, 677000), 2:4, geom, k = 2)
#' nearest_rows(c(673000, 677000), 2:4, geom, k = 2, coords = xy)
nearest_rows <- function(xy, candidates, geom, k, coords = NULL, window_m = 1000) {
  if (!length(candidates) || k < 1) {
    return(data.frame(row = integer(), distance_m = double()))
  }
  k <- min(k, length(candidates))
  pool <- candidates
  r <- Inf
  if (!is.null(coords)) {
    cx <- coords[candidates, 1]
    cy <- coords[candidates, 2]
    r <- window_m
    repeat {
      pool <- candidates[abs(cx - xy[1]) <= r & abs(cy - xy[2]) <= r]
      if (length(pool) >= k || length(pool) == length(candidates)) break
      r <- r * 4
    }
  }

  pt <- sf::st_sfc(sf::st_point(xy), crs = sf::st_crs(geom))
  nn <- suppressMessages(nngeo::st_nn(pt, geom[pool, ], k = k, returnDist = TRUE, progress = FALSE))
  out <- data.frame(row = pool[nn$nn[[1]]], distance_m = as.numeric(nn$dist[[1]]))

  # k-th neighbour beyond the window edge: something outside could be closer
  if (length(pool) < length(candidates) && max(out$distance_m) > r) {
    return(nearest_rows(xy, candidates, geom, k, coords, window_m = max(out$distance_m)))
  }
  out
}

#' Why is a property valued differently from its neighbours?
#'
#' Starts from a typical neighbour (median floor area times median NAV per m2
#' of the neighbours with floor data) and walks to the subject's official
#' valuation in three additive steps:
#' * floor area: `(A1 - A0) * R0`, the size difference at the typical rate;
#' * rate per m2: `A1 * (R1 - R0)`, the rate difference across the subject's
#'   area;
#' * other items and rounding: `V1 - A1 * R1`.
#'
#' The steps sum exactly to `V1 - A0 * R0`, so they form a true waterfall.
#'
#' @param subject A list with `area_m2`, `rate` and `valuation`.
#' @param neighbours A data frame with `area_m2` and `rate`.
#' @param min_neighbours Minimum neighbours with floor data.
#' @return A data frame of five steps with `value`, `start`, `end` and
#'   `measure`, and a `typical` attribute; or `NULL` when there isn't enough
#'   floor data.
#' @export
#' @examples
#' decompose_vs_neighbours(
#'   subject = list(area_m2 = 300, rate = 130, valuation = 39500),
#'   neighbours = data.frame(area_m2 = c(200, 250, 100), rate = c(100, 120, 150))
#' )
decompose_vs_neighbours <- function(subject, neighbours, min_neighbours = 3) {
  ok <- is.finite(neighbours$area_m2) & neighbours$area_m2 > 0 & is.finite(neighbours$rate)
  a1 <- subject$area_m2
  r1 <- subject$rate
  v1 <- subject$valuation
  if (sum(ok) < min_neighbours || !is.finite(a1) || !is.finite(r1) || !is.finite(v1)) {
    return(NULL)
  }

  a0 <- stats::median(neighbours$area_m2[ok])
  r0 <- stats::median(neighbours$rate[ok])
  steps <- data.frame(
    step = c("Typical neighbour", "Floor area", "Rate per m\u00b2", "Other items and rounding", "This property"),
    value = c(a0 * r0, (a1 - a0) * r0, a1 * (r1 - r0), v1 - a1 * r1, v1),
    measure = c("absolute", "relative", "relative", "relative", "total"),
    stringsAsFactors = FALSE
  )
  run <- cumsum(ifelse(steps$measure == "total", 0, steps$value))
  steps$end <- ifelse(steps$measure == "total", steps$value, run)
  steps$start <- ifelse(steps$measure == "relative", steps$end - steps$value, 0)

  attr(steps, "typical") <- list(area_m2 = a0, rate = r0, n = sum(ok))
  steps
}

# Background dots to draw for the current view: matching rows inside the
# bounds, capped at `max_points` by a fixed random rank.
points_in_view <- function(props, keep, bounds, max_points) {
  in_view <- which(
    keep & props$lat <= bounds$north & props$lat >= bounds$south &
      props$lon <= bounds$east & props$lon >= bounds$west
  )
  n_view <- length(in_view)
  if (n_view > max_points) {
    in_view <- in_view[order(props$draw_rank[in_view])][seq_len(max_points)]
  }
  structure(in_view, counts = c(matching = sum(keep), in_view = n_view, drawn = length(in_view)))
}

map_status_text <- function(counts) {
  if (counts[["matching"]] == 0) {
    return("No properties match these filters. Loosen a filter or press Reset.")
  }
  matching <- sprintf("%s properties match.", fmt_int(counts[["matching"]]))
  if (counts[["in_view"]] > counts[["drawn"]]) {
    sprintf(
      "%s Showing %s of the %s in view; zoom in to see the rest.",
      matching, fmt_int(counts[["drawn"]]), fmt_int(counts[["in_view"]])
    )
  } else {
    sprintf("%s Showing all %s in view.", matching, fmt_int(counts[["in_view"]]))
  }
}
