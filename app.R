# Entry point for shinyapps.io and Posit Connect: rsconnect::deployApp()
# deploys this folder, installs the dependencies listed in DESCRIPTION and
# runs this file. Set VALUATIONS_DATA in the app's environment to the
# published "latest" URL.
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
tailtevaluations::run_app()
