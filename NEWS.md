# tailtevaluations 0.4.0

## Reproducibility

* renv manages the project library. `renv.lock` pins R and every package
  (including development dependencies) at versions the test suite and
  `R CMD check` passed with. The scheduled refresh restores that exact
  environment; the check, coverage, lint and pkgdown workflows still test
  against current CRAN.

## Using the API and data responsibly

* The client identifies itself with a versioned User-Agent and, through
  `TAILTE_CONTACT`, a way to reach whoever runs it (the scheduled workflow
  uses the repository URL). New `tailte_user_agent()`.
* Requests are paced (`TAILTE_REQUESTS_PER_MINUTE`, default 20), retried only
  on 429 and 5xx, and wait as long as any `Retry-After` header asks.
* The scheduled refresh pulls weekly by default instead of daily.
* Every published copy of the data carries its licence: `ATTRIBUTION.txt`
  and an `attribution` block in `manifest.json` give data.gov.ie's CC BY 4.0
  statement, the licence link, the changes this project makes, and that
  Tailte Eireann doesn't endorse it. New `data_attribution()`.
* Dated snapshots leave out the occupier name and the GeoJSON; set
  `SNAPSHOTS=false` to keep none.
* The app credits Tailte Eireann on the map and in the sidebar, links the
  licence, and has an "About the data" notice covering changes, derived
  figures, accuracy and where to report errors. The comparison chart says
  it is the app's analysis, not a Tailte valuation.

## Basemap

* The app now uses OpenStreetMap's standard tiles instead of CARTO, which
  stopped serving tiles without an API key on 25 September 2026. No key or
  setting is needed to deploy. Dark mode darkens the tiles with a CSS filter.

# tailtevaluations 0.3.0

* Restructured as an R package. Pipeline steps and the app are package
  functions with roxygen2 documentation and examples; `_targets.R` loads the
  package and tracks its functions through `imports`, so editing a function
  re-runs exactly the targets that depend on it.
* The app is now `run_app()`, built from three Shiny modules (filters, map,
  detail) with the data injected, and deployable through the root `app.R`.
* Tests cover the API client (mocked HTTP), publishing (mocked S3, upload
  order), the quality gate, the app's logic, every module and the app server
  through `shiny::testServer()`.
* GitHub Actions for `R CMD check` on Linux, macOS and Windows, test coverage,
  lint and a pkgdown site, alongside the scheduled refresh.

# tailtevaluations 0.2.0

* Shiny app: filters, nearest neighbours on click with a cap, floor-by-floor
  breakdown and a size-versus-rate waterfall against the neighbours.
* Windowed nearest-neighbour search: about 10 times faster than handing every
  candidate to `nngeo::st_nn()`, with identical results.

# tailtevaluations 0.1.0

* `targets` pipeline: one API pull per authority per period, a quality gate
  before publishing, Parquet, GeoJSON and a manifest published to
  S3-compatible storage, and a scheduled GitHub Actions refresh.
