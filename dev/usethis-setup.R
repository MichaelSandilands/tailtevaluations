# One-off scaffolding, kept so the project's setup is reproducible.
# Run from the parent folder: Rscript dev/usethis-setup.R <path-to-new-package>
# (Already applied; you don't need to run it again.)

args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args)) args[[1]] else "tailtevaluations"

usethis::create_package(
  path,
  fields = list(
    Title = "Tailte Eireann Commercial Valuations: Refresh Pipeline and Shiny App",
    Description = paste(
      "Pulls every rating authority from the Tailte Eireann Valuation API,",
      "validates the result and publishes Parquet, GeoJSON and a manifest to",
      "S3-compatible object storage on a schedule, using a 'targets' pipeline.",
      "Includes a 'shiny' app for filtering properties, finding the nearest",
      "neighbours of any property and explaining how its valuation compares."
    ),
    `Authors@R` = 'person("First", "Last", , "msand91@gmail.com", role = c("aut", "cre"))',
    Language = "en-GB",
    Version = "0.3.0"
  ),
  rstudio = FALSE,
  open = FALSE
)
usethis::local_project(path, force = TRUE)

usethis::use_mit_license("tailtevaluations authors")
usethis::use_package_doc()
usethis::use_testthat(3)
usethis::use_news_md()
# use_spell_check() asks before writing the word list, so do its steps directly
usethis::use_package("spelling", type = "Suggests")
writeLines(c(
  'if (requireNamespace("spelling", quietly = TRUE)) {',
  "  spelling::spell_check_test(vignettes = TRUE, error = FALSE, skip_on_cran = TRUE)",
  "}"
), "tests/spelling.R")

# Runtime dependencies: pipeline, then app
usethis::use_package("R", type = "Depends", min_version = "4.1.0")
for (pkg in c(
  "cli", "data.table", "httr2", "jsonlite", "nanoparquet", "paws.storage", "sf",
  "htmltools", "leaflet", "nngeo", "plotly", "reactable", "stats", "tools", "utils"
)) {
  usethis::use_package(pkg)
}
usethis::use_package("bslib", min_version = "0.8.0")
usethis::use_package("shiny", min_version = "1.8.0")

# Development and pipeline-runner dependencies
for (pkg in c("covr", "pkgload", "targets", "withr")) {
  usethis::use_package(pkg, type = "Suggests")
}

# Files that belong to the project, not the package
usethis::use_build_ignore(c("_targets.R", "_targets", "data", "app.R", "dev", "codecov.yml"))

# Continuous integration
# On a machine with GitHub API access this is just use_github_action("check-standard")
# etc. Here the templates are downloaded first and passed in as local files.
use_r_lib_action <- function(name) {
  local_copy <- file.path(tempdir(), name)
  utils::download.file(
    sprintf("https://raw.githubusercontent.com/r-lib/actions/v2/examples/%s", name),
    local_copy,
    quiet = TRUE
  )
  usethis::use_github_action(url = local_copy, badge = FALSE)
}
use_r_lib_action("check-standard.yaml")
use_r_lib_action("test-coverage.yaml")
use_r_lib_action("lint.yaml")
use_r_lib_action("pkgdown.yaml")
usethis::use_pkgdown()
lintr::use_lintr(type = "tidyverse")

usethis::use_tidy_description()

# After writing code and docs: accept the project's vocabulary into inst/WORDLIST
# spelling::update_wordlist(confirm = FALSE)
