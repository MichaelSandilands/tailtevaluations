# Tailte Eireann Valuation API -------------------------------------------------
# Explorer: https://tailte.ie/home/api/  Licence: CC BY 4.0

#' The Valuation API endpoint
#'
#' Defaults to the live `GetProperties` endpoint. Set the `TAILTE_API_URL`
#' environment variable to point the pipeline at a mock for testing.
#'
#' @return A URL string.
#' @export
#' @examples
#' tailte_api_url()
tailte_api_url <- function() {
  Sys.getenv("TAILTE_API_URL", "https://opendata.tailte.ie/api/Property/GetProperties")
}

#' Rating authorities served by the API
#'
#' Spelled exactly as the API's `LocalAuthority` parameter expects. A
#' misspelling returns an empty collection rather than an error, which is why
#' [validate_refresh()] fails on any authority with no rows.
#'
#' @return A character vector of 31 authority names.
#' @export
#' @examples
#' head(local_authorities())
local_authorities <- function() {
  c(
    "CARLOW COUNTY COUNCIL", "CAVAN COUNTY COUNCIL", "CLARE COUNTY COUNCIL",
    "CORK CITY COUNCIL", "CORK COUNTY COUNCIL", "DONEGAL COUNTY COUNCIL",
    "DUBLIN CITY COUNCIL", "DUN LAOGHAIRE RATHDOWN CO CO", "FINGAL COUNTY COUNCIL",
    "GALWAY CITY COUNCIL", "GALWAY COUNTY COUNCIL", "KERRY COUNTY COUNCIL",
    "KILDARE COUNTY COUNCIL", "KILKENNY COUNTY COUNCIL", "LAOIS COUNTY COUNCIL",
    "LEITRIM COUNTY COUNCIL", "LIMERICK CITY AND COUNTY COUNCIL",
    "LONGFORD COUNTY COUNCIL", "LOUTH COUNTY COUNCIL", "MAYO COUNTY COUNCIL",
    "MEATH COUNTY COUNCIL", "MONAGHAN COUNTY COUNCIL", "OFFALY COUNTY COUNCIL",
    "ROSCOMMON COUNTY COUNCIL", "SLIGO COUNTY COUNCIL", "SOUTH DUBLIN COUNTY COUNCIL",
    "TIPPERARY COUNTY COUNCIL", "WATERFORD CITY AND COUNTY COUNCIL",
    "WESTMEATH COUNTY COUNCIL", "WEXFORD COUNTY COUNCIL", "WICKLOW COUNTY COUNCIL"
  )
}

#' Authorities still on a pre-revaluation list
#'
#' Their `valuation` is a legacy Rateable Valuation (RV), not a Net Annual
#' Value in euro, and they come back without category, use or floor detail.
#' Remove Cork from here once Tailte publishes its revalued list.
#'
#' @return A character vector of authority names.
#' @export
#' @examples
#' pre_revaluation_authorities()
pre_revaluation_authorities <- function() {
  c("CORK CITY COUNCIL", "CORK COUNTY COUNCIL")
}

#' A key that changes once per refresh period
#'
#' The fetch targets take this as an argument, so they re-run when the period
#' rolls over and are skipped otherwise: `targets::tar_make()` can run as
#' often as you like without re-pulling the API.
#'
#' @param period `"day"`, `"week"` or `"always"`. Defaults to the
#'   `REFRESH_PERIOD` environment variable, or `"week"`.
#' @param now The current time.
#' @return A string, e.g. `"2026-10-17"` or `"week-of-2026-10-12"`.
#' @export
#' @examples
#' refresh_key("week")
#' refresh_key("day", as.POSIXct("2026-10-17 09:00", tz = "Europe/Dublin"))
refresh_key <- function(period = Sys.getenv("REFRESH_PERIOD", "week"), now = Sys.time()) {
  today <- as.Date(format(now, "%Y-%m-%d", tz = "Europe/Dublin"))
  switch(period,
    day = format(today),
    week = paste0("week-of-", today - (as.POSIXlt(today)$wday + 6) %% 7),
    always = format(now, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    cli::cli_abort("{.arg period} must be {.val day}, {.val week} or {.val always}, not {.val {period}}.")
  )
}

#' Pull every record for one authority as GeoJSON text
#'
#' Returns the raw body unchanged, so the targets store keeps an exact copy of
#' what the API sent. If the body is byte-identical to last period's, targets
#' sees the same hash and skips everything downstream.
#'
#' The client is deliberately polite to a free public service: it names itself
#' and a contact in the User-Agent (see [tailte_user_agent()]), spaces its
#' requests (`requests_per_minute`, shared across all calls in the session),
#' and on 429 or 5xx responses waits as long as any `Retry-After` header asks,
#' otherwise backing off exponentially, before giving up after `max_tries`.
#'
#' @param authority One of [local_authorities()].
#' @param refresh A [refresh_key()]; used only so that a new key forces a
#'   new pull.
#' @param url The endpoint, see [tailte_api_url()].
#' @param requests_per_minute Ceiling on the request rate. Defaults to the
#'   `TAILTE_REQUESTS_PER_MINUTE` environment variable, or 20.
#' @param max_tries,backoff Retry policy for 429 and 5xx responses; `backoff`
#'   takes the attempt number and returns seconds to wait.
#' @return The response body as a string.
#' @export
#' @examplesIf interactive()
#' body <- fetch_authority("CARLOW COUNTY COUNCIL", refresh_key())
#' nrow(tidy_properties(body, "CARLOW COUNTY COUNCIL"))
fetch_authority <- function(authority, refresh = NULL, url = tailte_api_url(),
                            requests_per_minute = env_num("TAILTE_REQUESTS_PER_MINUTE", 20),
                            max_tries = 4, backoff = function(attempt) 10 * 2^(attempt - 1)) {
  force(refresh)
  cli::cli_inform("Fetching {authority}")

  resp <- tailte_request(authority, url, requests_per_minute, max_tries, backoff) |>
    httr2::req_perform()

  body <- httr2::resp_body_string(resp, encoding = "UTF-8")
  if (!grepl("FeatureCollection", substr(body, 1, 500), fixed = TRUE)) {
    cli::cli_abort(c(
      "{authority}: response is not a GeoJSON FeatureCollection.",
      i = "First 200 characters: {substr(body, 1, 200)}"
    ))
  }
  body
}

# The request, built separately so tests can inspect it without sending it
tailte_request <- function(authority, url = tailte_api_url(), requests_per_minute = 20,
                           max_tries = 4, backoff = function(attempt) 10 * 2^(attempt - 1)) {
  httr2::request(url) |>
    httr2::req_url_query(
      Fields = "*",
      LocalAuthority = authority,
      Format = "geojson",
      Download = "false"
    ) |>
    httr2::req_user_agent(tailte_user_agent()) |>
    httr2::req_timeout(600) |>
    httr2::req_throttle(rate = requests_per_minute / 60, realm = "tailte-valuation-api") |>
    httr2::req_retry(
      max_tries = max_tries,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504),
      backoff = backoff
    )
}

#' The User-Agent the pipeline sends to the API
#'
#' Names the client and, when the `TAILTE_CONTACT` environment variable is set
#' (a repository URL or an email address), how to reach whoever runs it. That
#' lets the API's operators get in touch rather than block an unknown client
#' if the pipeline ever misbehaves. The scheduled GitHub workflow sets it to
#' the repository URL.
#'
#' @param contact How to reach the operator of this pipeline.
#' @return A User-Agent string.
#' @export
#' @examples
#' tailte_user_agent("https://github.com/you/tailtevaluations")
tailte_user_agent <- function(contact = Sys.getenv("TAILTE_CONTACT")) {
  version <- tryCatch(as.character(utils::packageVersion("tailtevaluations")), error = function(e) "dev")
  agent <- sprintf("tailtevaluations/%s (R; httr2; scheduled refresh of the Valuation open data)", version)
  if (nzchar(contact)) paste0(agent, "; contact: ", contact) else agent
}
