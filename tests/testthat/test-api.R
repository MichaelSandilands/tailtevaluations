test_that("refresh key is stable within a period and changes across periods", {
  mon <- as.POSIXct("2026-10-12 09:00", tz = "Europe/Dublin")
  sun <- as.POSIXct("2026-10-18 23:00", tz = "Europe/Dublin")
  expect_equal(refresh_key("week", mon), refresh_key("week", sun))
  expect_equal(refresh_key("week", mon), "week-of-2026-10-12")
  expect_false(refresh_key("day", mon) == refresh_key("day", sun))
  expect_error(refresh_key("hourly"), "must be")
})

test_that("there are 31 authorities, including the two pre-revaluation ones", {
  expect_length(local_authorities(), 31)
  expect_true(all(pre_revaluation_authorities() %in% local_authorities()))
})

test_that("fetch_authority asks for one authority's GeoJSON and returns the body", {
  seen <- NULL
  mock <- function(req) {
    seen <<- req$url
    httr2::response(200, body = charToRaw(fixture("empty.geojson")))
  }
  body <- suppressMessages(httr2::with_mocked_responses(
    mock,
    fetch_authority("DUN LAOGHAIRE RATHDOWN CO CO", "key", url = "https://example.test/api")
  ))
  expect_match(seen, "LocalAuthority=DUN%20LAOGHAIRE%20RATHDOWN%20CO%20CO", fixed = TRUE)
  expect_match(seen, "Format=geojson", fixed = TRUE)
  expect_match(seen, "Fields=%2A|Fields=\\*")
  expect_identical(body, fixture("empty.geojson"))
})

test_that("fetch_authority rejects a body that isn't GeoJSON", {
  mock <- function(req) httr2::response(200, body = charToRaw("<html>Down for maintenance</html>"))
  expect_error(
    suppressMessages(httr2::with_mocked_responses(mock, fetch_authority("X", url = "https://example.test"))),
    "not a GeoJSON FeatureCollection"
  )
})

test_that("fetch_authority surfaces HTTP errors", {
  mock <- function(req) httr2::response(404)
  expect_error(
    suppressMessages(httr2::with_mocked_responses(mock, fetch_authority("X", url = "https://example.test"))),
    class = "httr2_http_404"
  )
})

test_that("the client identifies itself and, when configured, a contact", {
  expect_match(tailte_user_agent(""), "^tailtevaluations/")
  expect_no_match(tailte_user_agent(""), "contact")
  expect_match(tailte_user_agent("https://github.com/me/repo"), "contact: https://github.com/me/repo", fixed = TRUE)

  withr::local_envvar(TAILTE_CONTACT = "ops@example.ie")
  req <- tailte_request("CARLOW COUNTY COUNCIL", url = "https://example.test")
  expect_match(req$options$useragent, "contact: ops@example.ie", fixed = TRUE)
})

test_that("requests are paced, retried only when transient, and honour Retry-After", {
  req <- tailte_request("CARLOW COUNTY COUNCIL", url = "https://example.test", requests_per_minute = 20)
  expect_true(any(startsWith(names(req$policies), "throttle"))) # name differs across httr2 versions
  expect_equal(req$policies$retry_max_tries, 4)
  transient <- req$policies$retry_is_transient
  expect_true(transient(httr2::response(429)))
  expect_true(transient(httr2::response(503)))
  expect_false(transient(httr2::response(404)))
  # No custom `after`, so httr2 waits as long as the server's Retry-After asks
  expect_null(req$policies$retry_after)
})
