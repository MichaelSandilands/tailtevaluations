# Quality gate -----------------------------------------------------------------
#
# Runs before anything is written or published. If a check fails, the target
# errors, tar_make() stops, and the last good copy stays in place for the app.
# A quiet API failure (an authority suddenly returning 40 rows instead of
# 40,000) is far worse for an app than a stale-but-complete dataset.

# Generous Irish Transverse Mercator envelope for the island of Ireland
itm_bounds <- list(x = c(400000, 800000), y = c(500000, 1000000))

#' Check a refresh before it is published
#'
#' Fails with a list of every problem found when:
#' * any authority returned no records;
#' * fewer than `min_xy_share` of records have coordinates;
#' * more than `max_oob_share` of coordinates fall outside Ireland (usually a
#'   CRS mix-up);
#' * any authority's count fell by more than `max_drop_share` since the
#'   `previous` refresh.
#'
#' The thresholds default to the `MIN_XY_SHARE`, `MAX_OUT_OF_BOUNDS_SHARE` and
#' `MAX_DROP_SHARE` environment variables.
#'
#' @param properties Output of [finalise_properties()].
#' @param floors Output of [finalise_floors()].
#' @param authorities The authorities that were queried.
#' @param previous The last published manifest, from
#'   [read_previous_manifest()], or `NULL` for a first run.
#' @param min_xy_share,max_oob_share,max_drop_share Thresholds, as proportions.
#' @return On success, a summary list with no timestamps, so an unchanged
#'   dataset produces an unchanged hash and the write and publish steps are
#'   skipped.
#' @export
#' @examples
#' path <- system.file("extdata", "carlow_sample.geojson", package = "tailtevaluations")
#' body <- readChar(path, file.size(path), useBytes = TRUE)
#' props <- finalise_properties(tidy_properties(body, "CARLOW COUNTY COUNCIL"))
#' str(validate_refresh(props, tidy_floors(body), "CARLOW COUNTY COUNCIL"))
#'
#' # A council that suddenly shrinks fails the gate
#' previous <- list(rows = list(by_authority = list(`CARLOW COUNTY COUNCIL` = 100L)))
#' try(validate_refresh(props, tidy_floors(body), "CARLOW COUNTY COUNCIL", previous))
validate_refresh <- function(properties, floors, authorities, previous = NULL,
                             min_xy_share = env_num("MIN_XY_SHARE", 0.5),
                             max_oob_share = env_num("MAX_OUT_OF_BOUNDS_SHARE", 0.01),
                             max_drop_share = env_num("MAX_DROP_SHARE", 0.2)) {
  problems <- character()

  counts <- table(factor(properties$local_authority, levels = authorities))
  counts <- stats::setNames(as.integer(counts), names(counts))

  empty <- names(counts)[counts == 0]
  if (length(empty)) {
    problems <- c(problems, paste("No records returned for:", toString(empty)))
  }

  has_xy <- !is.na(properties$x_itm) & !is.na(properties$y_itm)
  xy_share <- if (nrow(properties)) mean(has_xy) else 0
  if (xy_share < min_xy_share) {
    problems <- c(problems, sprintf("Only %.1f%% of records have coordinates.", 100 * xy_share))
  }

  oob <- has_xy & (
    properties$x_itm < itm_bounds$x[1] | properties$x_itm > itm_bounds$x[2] |
      properties$y_itm < itm_bounds$y[1] | properties$y_itm > itm_bounds$y[2]
  )
  oob_share <- if (any(has_xy)) sum(oob) / sum(has_xy) else 0
  if (oob_share > max_oob_share) {
    problems <- c(problems, sprintf(
      "%.1f%% of coordinates fall outside Ireland; is the CRS still ITM?", 100 * oob_share
    ))
  }

  prev_counts <- unlist(previous$rows$by_authority)
  if (length(prev_counts)) {
    common <- intersect(names(counts), names(prev_counts))
    drop <- 1 - counts[common] / pmax(prev_counts[common], 1)
    shrunk <- drop[drop > max_drop_share]
    if (length(shrunk)) {
      problems <- c(problems, paste0(
        "Record count fell by more than ", 100 * max_drop_share, "% since the last refresh: ",
        toString(sprintf(
          "%s (%d -> %d)", names(shrunk), prev_counts[names(shrunk)], counts[names(shrunk)]
        ))
      ))
    }
  }

  if (length(problems)) {
    cli::cli_abort(c(
      "Refresh failed validation; nothing was published.",
      stats::setNames(problems, rep("x", length(problems)))
    ))
  }

  list(
    total = nrow(properties),
    floors = nrow(floors),
    with_coordinates = sum(has_xy),
    out_of_bounds = sum(oob),
    by_authority = as.list(counts)
  )
}

#' The manifest from the last successful refresh
#'
#' Reads the local copy if there is one, otherwise the published copy (for
#' example after a CI cache was evicted), so the drop check in
#' [validate_refresh()] still has something to compare against.
#'
#' @param out_dir The local output folder.
#' @param config A [publish_config()].
#' @return The parsed manifest, or `NULL` if there is none.
#' @export
#' @examples
#' read_previous_manifest(tempdir(), list(bucket = ""))
read_previous_manifest <- function(out_dir, config) {
  local <- read_manifest(file.path(out_dir, "manifest.json"))
  if (!is.null(local) || !nzchar(config$bucket)) {
    return(local)
  }
  tryCatch(
    {
      obj <- s3_client(config)$get_object(
        Bucket = config$bucket,
        Key = sprintf("%s/latest/manifest.json", config$prefix)
      )
      jsonlite::parse_json(rawToChar(obj$Body))
    },
    error = function(e) NULL
  )
}

read_manifest <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  tryCatch(jsonlite::read_json(path), error = function(e) NULL)
}

env_num <- function(name, default) {
  value <- suppressWarnings(as.numeric(Sys.getenv(name, NA)))
  if (is.na(value)) default else value
}
