# Tailte Eireann valuations: scheduled refresh --------------------------------
#
#   targets::tar_make()          run (fetches only if the refresh period rolled over)
#   targets::tar_visnetwork()    see the graph (needs visNetwork)
#   REFRESH_PERIOD=always        force a fresh pull on the next run
#
# The step functions live in the package (R/). Loading it from source, and
# listing it in `imports`, means targets tracks the package's functions: edit
# tidy_properties() and exactly the targets that depend on it re-run.

library(targets)
pkgload::load_all(quiet = TRUE, export_all = FALSE, helpers = FALSE)

tar_option_set(
  imports = "tailtevaluations",
  memory = "transient", # Dublin City's raw response is large; release after use
  garbage_collection = TRUE
)

out_dir <- Sys.getenv("OUTPUT_DIR", "data")

list(
  # Changes once per day or week (REFRESH_PERIOD); re-checked on every run
  tar_target(refresh, refresh_key(), cue = tar_cue(mode = "always")),
  tar_target(authority, local_authorities()),

  # One branch per authority, so one slow or failing council is retried alone
  tar_target(raw, fetch_authority(authority, refresh), pattern = map(authority), iteration = "list"),
  tar_target(properties_la, tidy_properties(raw, authority), pattern = map(raw, authority)),
  tar_target(floors_la, tidy_floors(raw), pattern = map(raw)),
  tar_target(properties, finalise_properties(properties_la)),
  tar_target(floors, finalise_floors(floors_la, properties)),

  # Where to publish (no secrets); re-read every run so config changes apply
  tar_target(destination, publish_config(), cue = tar_cue(mode = "always")),

  # Counts from the last successful refresh, for the drop check
  tar_target(previous_manifest, read_previous_manifest(out_dir, destination), cue = tar_cue(mode = "always")),
  tar_target(qa, validate_refresh(properties, floors, authority, previous_manifest)),
  tar_target(outputs, write_outputs(properties, floors, qa, out_dir), format = "file"),
  tar_target(published, publish_outputs(outputs, destination))
)
