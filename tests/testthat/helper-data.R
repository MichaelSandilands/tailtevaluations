fixture <- function(name) {
  path <- test_path("fixtures", name)
  readChar(path, file.size(path), useBytes = TRUE)
}

# A small synthetic dataset for app tests: 40 properties on a 50 m grid near
# Carlow town, alternating offices and shops, across two councils, each with
# two floors.
example_valuations <- function(n = 40) {
  i <- seq_len(n)
  props <- data.frame(
    property_number = 1000L + i,
    name = sprintf("Property %02d", i),
    address = sprintf("%d Main Street, Carlow", i),
    eircode = NA_character_,
    county = "CARLOW",
    local_authority = rep(c("CARLOW COUNTY COUNCIL", "KILKENNY COUNTY COUNCIL"), each = n / 2),
    category = rep(c("OFFICE", "RETAIL (SHOPS)"), times = n / 2),
    uses = rep(c("OFFICE (OWN DOOR)", "SHOP"), times = n / 2),
    valuation = 10000 + 500 * i,
    valuation_basis = "NAV",
    additional_items = 0,
    car_park = rep(c(0, 5), times = n / 2),
    publication_date = as.Date("2024-06-10"),
    x_itm = 673000 + 50 * ((i - 1) %% 8),
    y_itm = 677000 + 50 * ((i - 1) %/% 8),
    stringsAsFactors = FALSE
  )
  props <- finalise_properties(props)
  floors <- data.frame(
    property_number = rep(props$property_number, each = 2),
    level = rep(c("0", "1"), n),
    floor_use = rep(c("OFFICE(S)", "STORE"), n),
    area_m2 = rep(c(80, 20), n) + rep(i, each = 2),
    nav_per_m2 = rep(c(100, 40), n),
    stringsAsFactors = FALSE
  )
  floors$nav <- floors$area_m2 * floors$nav_per_m2
  prepare_valuations(props, floors, list(updated_at = "2026-10-17T05:00:00Z"))
}

# What the app server computes for a selected row
neighbours_of <- function(vals, row, k = 8, same_category = TRUE) {
  p <- vals$props
  candidates <- neighbour_candidates(p, rep(TRUE, nrow(p)), row, same_category)
  nb <- nearest_rows(c(p$x_itm[row], p$y_itm[row]), candidates, vals$geom, k, coords = vals$xy)
  cbind(rank = seq_len(nrow(nb)), nb, p[nb$row, , drop = FALSE])
}
