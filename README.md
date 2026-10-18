# tailtevaluations

Every commercial property on the Tailte Éireann valuation lists, refreshed on a schedule and explorable in a Shiny app. It's an R package with three parts:

- a [{targets}](https://docs.ropensci.org/targets/) **pipeline** that pulls all 31 rating authorities from the Valuation API, checks the result and publishes it to object storage;
- a **Shiny app**, `run_app()`, for filtering properties, finding the n nearest neighbours of any property and seeing why its valuation differs from theirs;
- the **tests, checks and CI** that keep both honest.

## Layout

```         
R/                     package code: pipeline steps, app logic, Shiny modules
  api.R tidy.R validate.R publish.R      pipeline steps
  app_logic.R app_data.R app_charts.R    app logic, testable without Shiny
  mod_filters.R mod_map.R mod_detail.R   Shiny modules
  app_ui.R app_server.R run_app.R        the app
inst/app/www/          app stylesheet
inst/extdata/          a small API response used by examples
tests/testthat/        unit, mocked-HTTP, mocked-S3 and testServer() tests
_targets.R             the pipeline (not part of the installed package)
app.R                  deployment entry point for shinyapps.io / Posit Connect
dev/usethis-setup.R    the usethis calls that scaffolded the project
.github/workflows/     R CMD check, coverage, lint, pkgdown, scheduled refresh
```

## Quick start

``` r
# Opening R in the project activates renv (via .Rprofile); then:
renv::restore()       # install the exact versions in renv.lock

targets::tar_make()   # pull, validate and write data/
pkgload::load_all()
run_app()             # reads data/
```

To run the app against the published copy instead, set `VALUATIONS_DATA` to its `latest` URL, or pass it: `run_app("https://<your-bucket-domain>/tailte-valuations/latest")`.

## Reproducible environment (renv)

`renv.lock` pins R and every package the project uses, at the versions the test suite and `R CMD check` passed with. It records everything in `DESCRIPTION`, development dependencies included (`snapshot.type = "explicit"`, `snapshot.dev = true` in `renv/settings.json`), so the pipeline runner and test tools are pinned too.

- **Restore:** `renv::restore()`. The scheduled refresh does the same in CI, on the R version the lockfile names.
- **Add a dependency:** `usethis::use_package("pkg")`, then `renv::install("pkg")` and `renv::snapshot()`.
- **Upgrade:** `renv::update()`, run the tests and check, then `renv::snapshot()`.
- **Dev tools** that aren't in `DESCRIPTION` (devtools, usethis, styler, lintr) don't belong in the lockfile: `renv::install()` them into the project library, and they stay out of it.

The lockfile was built on R 4.3.3 with the current CRAN release of every package that supports it. If you're on a newer R, `renv::restore()` still works (renv will mention the version difference); run `renv::update()` and `renv::snapshot()` to move the lockfile to your R once the checks pass.

The check, coverage, lint and pkgdown workflows set `RENV_ACTIVATE_PROJECT=false`, so they keep testing the package against current CRAN, as a package should be.

## Development

The usual devtools loop:

``` r
devtools::load_all()    # Ctrl/Cmd + Shift + L
devtools::document()    # roxygen2 -> man/ and NAMESPACE
devtools::test()        # testthat 3e
devtools::check()       # R CMD check: 0 errors, 0 warnings, 0 notes
covr::report()          # line-by-line coverage
styler::style_pkg()     # tidyverse style
lintr::lint_package()   # rules in .lintr
spelling::spell_check_package()
```

New code goes in with `usethis::use_r("name")` and `usethis::use_test("name")`, new dependencies with `usethis::use_package("pkg")`. `NEWS.md` records changes; bump with `usethis::use_version()`.

A few conventions worth knowing:

- **Functions are namespaced** (`sf::st_transform()`), except shiny, bslib and leaflet, which are imported whole because UI code is mostly their functions.
- **The app gets its data injected.** `app_server(vals)` and the modules take a prepared dataset rather than reading files, so tests hand them a small synthetic one.
- **Logic lives outside Shiny.** Filtering, the neighbour search and the waterfall maths are plain functions in `app_logic.R`; modules only wire them to inputs and outputs.
- **Source is ASCII.** The euro sign, m² and accented names are `\u` escapes, so `R CMD check` passes everywhere.

### Tests

| File | Covers |
|----|----|
| `test-api.R` | refresh keys; the API request, its query, User-Agent, pacing and retry policy, and error handling, with mocked HTTP (`httr2::with_mocked_responses()`) |
| `test-attribution.R` | the licence statement, link, changes and error contact |
| `test-tidy.R` | parsing, a fixed schema across authorities, null and placeholder geometry, Cork's missing fields |
| `test-validate.R` | every quality-gate rule; reading the previous manifest from disk or a mocked bucket |
| `test-publish.R` | written files, checksums and attribution; snapshots without occupier names; upload keys and order (manifest last) with a mocked S3 client |
| `test-app-logic.R` | filters, neighbour candidates, the windowed search giving the same answer as a full one, the waterfall summing exactly |
| `test-app-data.R`, `test-app-charts.R` | data preparation, the plot's bars and colours |
| `test-app-modules.R`, `test-app-server.R` | each module and the whole server through `shiny::testServer()`: search, map clicks, pins, table picks, debounced filters, the neighbour cap; basemap selection and the app's attribution |

## The pipeline

```         
refresh key --+
authority ----+--> raw (31 branches, one per council)
                     |--> properties_la --> properties --+
                     +--> floors_la ------> floors ------+--> qa --> outputs --> published
                               previous_manifest --------+        (files)     (S3 / R2)
```

- **One API call per rating authority**, as dynamic branches, so one slow or failing council retries alone. Requests are paced and polite; see [Using the API and data responsibly](#using-the-api-and-data-responsibly).
- **One pull per period.** The fetch depends on a key that changes once a day or week (`REFRESH_PERIOD`), so `tar_make()` only hits the API when the period rolls over.
- **Nothing rewritten on quiet days.** Raw responses stay in the store; if the API sends the same bytes, every downstream hash matches and nothing is re-uploaded.
- **Code changes are tracked.** `_targets.R` loads the package from source and lists it in `imports`, so editing a step function re-runs exactly the targets that use it.
- **CRS handled explicitly.** The API's GeoJSON has Irish Transverse Mercator coordinates but no `crs` member, which `sf::st_read()` would read as WGS84. The parser declares EPSG:2157 itself.
- **Quality gate before publishing.** Every authority must return rows, at least half need coordinates, at most 1% may fall outside Ireland, and no authority may shrink by more than 20% since the last refresh.
- **Cork is flagged.** Cork City and County are still on pre-revaluation lists: their `valuation` is a legacy Rateable Valuation, not euro. `valuation_basis` says which is which.

Outputs, in `data/` and in the bucket under `<prefix>/latest/` (5-minute cache):

| File | Contents |
|----|----|
| `properties.parquet` | One row per property, with ITM `x_itm`/`y_itm` and WGS84 `lon`/`lat` |
| `floors.parquet` | One row per floor line: level, use, area, NAV, NAV/m². Join on `property_number` |
| `properties.geojson` | WGS84 points for web maps and other tools |
| `ATTRIBUTION.txt` | Licence notice, source, the changes made, disclaimer and where to report errors |
| `manifest.json` | Update time, counts per authority, checksums and attribution. Uploaded last |

Dated copies go to `<prefix>/snapshots/<date>/` whenever the data changes: the Parquet files without the occupier name, plus the attribution and manifest. Set `SNAPSHOTS=false` to keep none, and add an R2 lifecycle rule if you want them to expire.

### Settings

| Variable | Default | Purpose |
|----|----|----|
| `REFRESH_PERIOD` | `week` | `day`, `week` or `always` |
| `TAILTE_CONTACT` | unset; the repo URL in CI | How Tailte can reach you, sent in the User-Agent |
| `TAILTE_REQUESTS_PER_MINUTE` | `20` | Ceiling on the API request rate |
| `SNAPSHOTS` | `true` | `false` keeps no dated copies |
| `S3_BUCKET` | unset | Bucket name; unset means local output only |
| `S3_ENDPOINT` | unset | Custom endpoint, for R2 or MinIO |
| `S3_PREFIX` | `tailte-valuations` | Key prefix inside the bucket |
| `AWS_REGION` | `auto` | `auto` for R2, a real region for AWS |
| `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY` | unset | Bucket credentials |
| `OUTPUT_DIR` | `data` | Local output folder |
| `MAX_DROP_SHARE`, `MIN_XY_SHARE` | `0.2`, `0.5` | Quality-gate thresholds |
| `TAILTE_API_URL` | live endpoint | Point at a mock for testing |
| `VALUATIONS_DATA` | `data` | Where the app reads from: a folder or the published `latest` URL |

Locally, put these in a project `.Renviron` (gitignored).

## The app

- **Filters** for council, property type, use, floor use, valuation, rate per m², floor area, publication date and car spaces.
- **Click a property**, or search by name, address, Eircode or number, for its valuation, floor-by-floor breakdown and n nearest neighbours, drawn as spokes on the map. Click a neighbour, on the map or in the table, to move to it. Click empty map to see what's nearest a spot.
- **Compare** starts from a typical neighbour (median floor area × median rate) and steps to the official valuation through the size difference, the rate difference and other items. The steps sum exactly.
- **Caps:** at most 50 neighbours and 4,000 background dots per view (`run_app(max_neighbours =, max_map_points =)`).
- **Fast, exact neighbour search:** candidates are narrowed to a window around the click before `nngeo::st_nn()` runs, growing the window until the answer is provably the same. About 50 ms per click on 200,000 properties, against 500 ms without it.

Deploy with `rsconnect::deployApp()` from the project root; it uses `renv.lock`. Set `VALUATIONS_DATA` in the app's environment; that's the only setting the app needs. Data loads once per R process, so a refresh appears when the process restarts.

## Continuous integration

| Workflow | When | What |
|----|----|----|
| `R-CMD-check.yaml` | push, pull request | `R CMD check` on Linux (release, devel, oldrel), macOS and Windows |
| `test-coverage.yaml` | push, pull request | coverage with covr, uploaded to Codecov |
| `lint.yaml` | push, pull request | `lintr::lint_package()` |
| `pkgdown.yaml` | push to main, release | builds the documentation site |
| `refresh.yml` | daily 04:23 UTC, manual | restores `renv.lock`, runs the pipeline (pulling the API once a week by default), caches the store, publishes on success |

## First-time setup on GitHub

``` r
usethis::use_git()
usethis::use_github()                               # creates the repo and pushes
usethis::use_github_actions_badge("R-CMD-check.yaml")
usethis::use_coverage("codecov")                    # codecov.yml and badge
usethis::use_pkgdown_github_pages()                 # site URL and gh-pages branch
```

Then add the repo **secrets** `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` and **variables** `S3_BUCKET` and `S3_ENDPOINT` for the scheduled refresh (and `TAILTE_CONTACT` if you'd rather give an email than the repo URL). With Cloudflare R2: create a bucket, an API token with Object Read & Write on it, use `https://<account-id>.r2.cloudflarestorage.com` as the endpoint, and turn on public access if the app should read over plain HTTPS. Update the author in `DESCRIPTION` and `LICENSE` too.

## Other destinations

- **PostGIS** (Supabase, Neon): to have the database do filtering and nearest-neighbour queries (`ORDER BY geom <-> point`) instead of loading everything into memory.
- **pins on Posit Connect**: `pins::pin_reactive_read()` handles reloads.
- **DuckDB**: reads `properties.parquet` straight from the bucket for ad-hoc SQL.
- **PMTiles**: vector tiles, to map every property at once instead of capping the dots.

## Using the API and data responsibly {#using-the-api-and-data-responsibly}

The Valuation API is free and open, and the data is published on data.gov.ie under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). Tailte Éireann's re-use conditions ask re-users to acknowledge the source and its copyright, reproduce the data accurately, not mislead, and not use it mainly to advertise. Tailte states no rate limits, and says the API is a work in progress that may change. Here is how the project meets that, and what's left to you.

**Attribution (CC BY 4.0).** Credit, a licence link, a note of changes and no implied endorsement are required wherever the data appears:

- The app shows data.gov.ie's attribution statement, naming Tailte Éireann, in the sidebar and on the map. Its "About the data" notice lists the changes this project makes, says the derived figures (rates, neighbours, the Compare chart) are the app's analysis, not Tailte valuations, and points to `opendata@tailte.ie` for errors.
- Every published copy carries `ATTRIBUTION.txt` and an `attribution` block in `manifest.json`, because a public bucket redistributes the data. The wording lives in one place, `data_attribution()`.

**Being a good API client.**

- Pulls weekly by default, one request per authority, paced to at most 20 a minute.
- Retries only on 429 and 5xx, waiting as long as a `Retry-After` header asks, then gives up after 4 tries.
- Identifies itself in the User-Agent with a contact (the repo URL in CI; set `TAILTE_CONTACT`). If anything ever misbehaves, Tailte can contact you rather than block an unknown client.
- Skips re-publishing when nothing changed.

**Personal data.** The data.gov.ie licence doesn't cover personal data, and the `name` field is the occupier, who can be a sole trader. The project keeps names only where the official list does:

- The `latest` copy mirrors Tailte's publication and refreshes weekly, so their corrections and removals flow through.
- Dated snapshots leave names out, so the history can't track occupiers. Turn snapshots off entirely with `SNAPSHOTS=false`.
- Don't use the data to profile or contact individual occupiers.

**Basemap.** The app uses OpenStreetMap's standard tiles: no key, nothing to configure. Following the OSM tile usage policy, it requests the single `tile.openstreetmap.org` address and credits OpenStreetMap on the map. The service is free and best-effort, meant for light use like a portfolio app; for heavy or commercial traffic, switch `basemap()` to a commercial or self-hosted tile provider. OSM has no dark style, so dark mode darkens the tiles with a CSS filter.

This is engineering practice, not legal advice. If the app goes commercial or gets heavy use, read the terms below in full and revisit the basemap, and ask `opendata@tailte.ie` if in doubt.

Sources: [Tailte Éireann Open Data](https://tailte.ie/services/open-data/), [Valuation API on data.gov.ie](https://data.gov.ie/en_GB/dataset/tailte_eireann_valuation_api/resource/caf91f9d-c3eb-49d2-95bd-ffcc474d359b), [data.gov.ie licence](https://data.gov.ie/pages/opendatalicence), [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/), [OSM tile usage policy](https://operations.osmfoundation.org/policies/tiles/).

## Licences

The code is MIT-licensed (`LICENSE`). The valuation data is © Tailte Éireann under CC BY 4.0, and isn't covered by the MIT licence.
