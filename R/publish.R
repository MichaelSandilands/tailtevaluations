# Write and publish ------------------------------------------------------------

#' Write the refresh outputs
#'
#' Writes, into `out_dir`:
#' * `properties.parquet`: one row per property, with `x_itm`/`y_itm`
#'   (EPSG:2157) and `lon`/`lat` (EPSG:4326);
#' * `floors.parquet`: one row per floor line, joined on `property_number`;
#' * `properties.geojson`: WGS84 points per RFC 7946;
#' * `ATTRIBUTION.txt`: the licence notice, source and changes, which must
#'   travel with the data (see [data_attribution()]);
#' * `snapshot/`: the copies kept by date. They leave out the occupier name
#'   (data minimisation: history is for following valuations, not
#'   occupiers) and the GeoJSON;
#' * `manifest.json`: counts, checksums, attribution and the update time.
#'
#' @param properties,floors The finalised tables.
#' @param qa The summary from [validate_refresh()].
#' @param out_dir Output folder; created if needed.
#' @return The file paths, with `manifest.json` last (the order
#'   [publish_outputs()] uploads them in).
#' @export
#' @examples
#' path <- system.file("extdata", "carlow_sample.geojson", package = "tailtevaluations")
#' body <- readChar(path, file.size(path), useBytes = TRUE)
#' props <- finalise_properties(tidy_properties(body, "CARLOW COUNTY COUNCIL"))
#' floors <- finalise_floors(tidy_floors(body), props)
#' qa <- validate_refresh(props, floors, "CARLOW COUNTY COUNCIL")
#' basename(write_outputs(props, floors, qa, file.path(tempdir(), "valuations")))
write_outputs <- function(properties, floors, qa, out_dir = "data") {
  dir.create(file.path(out_dir, "snapshot"), recursive = TRUE, showWarnings = FALSE)
  path <- function(...) file.path(out_dir, ...)

  nanoparquet::write_parquet(properties, path("properties.parquet"))
  nanoparquet::write_parquet(floors, path("floors.parquet"))
  write_geojson(properties, path("properties.geojson"))
  write_utf8(attribution_text(), path("ATTRIBUTION.txt"))

  nanoparquet::write_parquet(properties[setdiff(names(properties), "name")], path("snapshot", "properties.parquet"))
  file.copy(path("floors.parquet"), path("snapshot", "floors.parquet"), overwrite = TRUE)

  data_files <- c(
    path(c("properties.parquet", "floors.parquet", "properties.geojson", "ATTRIBUTION.txt")),
    path("snapshot", c("properties.parquet", "floors.parquet"))
  )
  manifest <- list(
    schema_version = 2L,
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    attribution = data_attribution(),
    crs = list(x_itm_y_itm = "EPSG:2157", lon_lat = "EPSG:4326"),
    notes = "valuation is NAV in EUR except where valuation_basis is 'RV' (pre-revaluation Cork).",
    rows = qa,
    files = lapply(data_files, function(f) {
      list(name = substring(f, nchar(out_dir) + 2), bytes = file.size(f), md5 = unname(tools::md5sum(f)))
    })
  )
  jsonlite::write_json(manifest, path("manifest.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)

  c(data_files, path("manifest.json"))
}

write_geojson <- function(properties, path) {
  pts <- properties[!is.na(properties$lon) & !is.na(properties$lat), , drop = FALSE]
  pts <- sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  if (file.exists(path)) unlink(path)
  sf::st_write(
    pts, path,
    driver = "GeoJSON",
    layer_options = c("RFC7946=YES", "COORDINATE_PRECISION=6", "WRITE_NAME=NO"),
    quiet = TRUE
  )
  invisible(path)
}

write_utf8 <- function(lines, path) {
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
}

#' Where to publish
#'
#' Reads the destination from environment variables. Secrets
#' (`AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`) are deliberately not
#' included, so this is safe to store in the targets cache.
#'
#' @return A list with `bucket` (`S3_BUCKET`; empty means publish nothing),
#'   `prefix` (`S3_PREFIX`), `endpoint` (`S3_ENDPOINT`, for R2 or MinIO),
#'   `region` (`AWS_REGION`) and `snapshots` (`SNAPSHOTS`; set it to `false`
#'   to keep no dated copies).
#' @export
#' @examples
#' publish_config()
publish_config <- function() {
  list(
    bucket = Sys.getenv("S3_BUCKET"),
    prefix = sub("/+$", "", Sys.getenv("S3_PREFIX", "tailte-valuations")),
    endpoint = Sys.getenv("S3_ENDPOINT"),
    region = Sys.getenv("AWS_REGION", "auto"),
    snapshots = !identical(tolower(Sys.getenv("SNAPSHOTS", "true")), "false")
  )
}

#' Upload the outputs to S3-compatible storage
#'
#' Top-level files go to `<prefix>/latest/` (overwritten, 5-minute cache).
#' The `snapshot/` copies, with `ATTRIBUTION.txt` and `manifest.json`, go to
#' `<prefix>/snapshots/<date>/` (kept) unless snapshots are turned off.
#' `manifest.json` is uploaded last, so a reader that sees a new manifest can
#' always fetch the files it describes. With no bucket configured, nothing is
#' uploaded.
#'
#' @param files Paths from [write_outputs()].
#' @param config A [publish_config()].
#' @return A data frame of what was published.
#' @export
#' @examples
#' f <- tempfile(fileext = ".json")
#' writeLines("{}", f)
#' publish_outputs(f, list(bucket = ""))
publish_outputs <- function(files, config) {
  if (!nzchar(config$bucket)) {
    cli::cli_inform("S3_BUCKET is not set; outputs stay in {.path {dirname(files[1])}}.")
    return(data.frame(file = basename(files), key = NA_character_, bytes = file.size(files)))
  }

  s3 <- s3_client(config)
  day <- format(Sys.Date(), "%Y-%m-%d")
  in_snapshot <- basename(dirname(files)) == "snapshot"
  top <- files[!in_snapshot]
  is_manifest <- basename(top) == "manifest.json"
  latest_files <- c(top[!is_manifest], top[is_manifest])
  snapshot_files <- c(files[in_snapshot], top[basename(top) %in% c("ATTRIBUTION.txt", "manifest.json")])

  upload <- function(f, folder, cache) {
    key <- sprintf("%s/%s/%s", config$prefix, folder, basename(f))
    put_file(s3, config$bucket, key, f, cache = cache)
    data.frame(file = basename(f), key = key, bytes = file.size(f))
  }
  kept <- if (!isFALSE(config$snapshots)) {
    lapply(snapshot_files, upload, folder = paste0("snapshots/", day), cache = "public, max-age=31536000, immutable")
  }
  latest <- lapply(latest_files, upload, folder = "latest", cache = "public, max-age=300")

  cli::cli_inform("Published {length(latest_files)} files to s3://{config$bucket}/{config$prefix}/latest/")
  do.call(rbind, c(kept, latest))
}

s3_client <- function(config) {
  cfg <- list(region = config$region)
  if (nzchar(config$endpoint)) {
    cfg$endpoint <- config$endpoint
    cfg$s3_force_path_style <- TRUE
  }
  # Credentials come from AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY
  paws.storage::s3(config = cfg)
}

put_file <- function(s3, bucket, key, path, cache) {
  type <- switch(tools::file_ext(path),
    parquet = "application/vnd.apache.parquet",
    geojson = "application/geo+json",
    json = "application/json",
    txt = "text/plain; charset=utf-8",
    "application/octet-stream"
  )
  s3$put_object(
    Bucket = bucket,
    Key = key,
    Body = readBin(path, "raw", file.size(path)),
    ContentType = type,
    CacheControl = cache
  )
  invisible(key)
}
