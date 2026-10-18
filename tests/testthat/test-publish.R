outputs_in <- function(dir) {
  p <- finalise_properties(tidy_properties(fixture("carlow.geojson"), "CARLOW COUNTY COUNCIL"))
  f <- finalise_floors(tidy_floors(fixture("carlow.geojson")), p)
  write_outputs(p, f, validate_refresh(p, f, "CARLOW COUNTY COUNCIL"), dir)
}

test_that("write_outputs writes data, attribution and snapshot copies, manifest last", {
  dir <- withr::local_tempdir()
  files <- outputs_in(dir)
  rel <- substring(files, nchar(dir) + 2)
  expect_equal(rel, c(
    "properties.parquet", "floors.parquet", "properties.geojson", "ATTRIBUTION.txt",
    "snapshot/properties.parquet", "snapshot/floors.parquet", "manifest.json"
  ))
  expect_true(all(file.exists(files)))

  m <- jsonlite::read_json(files[7])
  expect_equal(m$rows$total, 4)
  expect_equal(m$crs$x_itm_y_itm, "EPSG:2157")
  expect_equal(m$attribution$licence, "CC BY 4.0")
  expect_equal(vapply(m$files, `[[`, "", "name"), rel[1:6])
  expect_equal(vapply(m$files, `[[`, "", "md5"), unname(tools::md5sum(files[1:6])))

  notice <- readLines(files[4], encoding = "UTF-8")
  expect_true(data_attribution()$statement %in% notice)
})

test_that("snapshots leave out the occupier name, the latest copy keeps it", {
  dir <- withr::local_tempdir()
  files <- outputs_in(dir)
  latest <- nanoparquet::read_parquet(files[1])
  kept <- nanoparquet::read_parquet(files[5])
  expect_true("name" %in% names(latest))
  expect_false("name" %in% names(kept))
  expect_equal(kept$property_number, latest$property_number)
  expect_equal(kept$valuation, latest$valuation)
})

test_that("Parquet round-trips and GeoJSON is WGS84 points with coordinates only", {
  dir <- withr::local_tempdir()
  files <- outputs_in(dir)
  p <- nanoparquet::read_parquet(files[1])
  expect_equal(nrow(p), 4)
  expect_s3_class(p$publication_date, "Date")

  g <- sf::st_read(files[3], quiet = TRUE)
  expect_equal(sf::st_crs(g)$epsg, 4326L)
  expect_equal(nrow(g), 2)
})

test_that("with no bucket nothing is uploaded", {
  dir <- withr::local_tempdir()
  files <- outputs_in(dir)
  local_mocked_bindings(put_file = function(...) stop("should not upload"))
  out <- expect_message(publish_outputs(files, list(bucket = "")), "S3_BUCKET is not set")
  expect_true(all(is.na(out$key)))
})

test_that("uploads go to snapshots and latest, with the manifest last", {
  dir <- withr::local_tempdir()
  files <- outputs_in(dir)
  keys <- character()
  local_mocked_bindings(
    s3_client = function(config) "fake client",
    put_file = function(s3, bucket, key, path, cache) {
      expect_equal(bucket, "valuations")
      keys <<- c(keys, key)
      invisible(key)
    }
  )
  config <- list(bucket = "valuations", prefix = "tailte", endpoint = "", region = "auto", snapshots = TRUE)
  out <- suppressMessages(publish_outputs(files, config))

  day <- sprintf("tailte/snapshots/%s/", format(Sys.Date(), "%Y-%m-%d"))
  expect_setequal(keys[startsWith(keys, day)], paste0(day, c(
    "properties.parquet", "floors.parquet", "ATTRIBUTION.txt", "manifest.json"
  )))
  expect_setequal(keys[startsWith(keys, "tailte/latest/")], paste0("tailte/latest/", c(
    "properties.parquet", "floors.parquet", "properties.geojson", "ATTRIBUTION.txt", "manifest.json"
  )))
  expect_equal(tail(keys, 1), "tailte/latest/manifest.json")
  expect_true(max(which(startsWith(keys, day))) < min(which(startsWith(keys, "tailte/latest/"))))
  expect_equal(out$key, keys)

  keys <- character()
  suppressMessages(publish_outputs(files, utils::modifyList(config, list(snapshots = FALSE))))
  expect_true(all(startsWith(keys, "tailte/latest/")))
})

test_that("each upload carries the file's bytes, content type and cache policy", {
  dir <- withr::local_tempdir()
  files <- outputs_in(dir)
  sent <- list()
  fake_s3 <- list(put_object = function(...) sent[[length(sent) + 1]] <<- list(...))
  for (f in files) put_file(fake_s3, "b", basename(f), f, cache = "max-age=300")

  types <- vapply(sent, `[[`, "", "ContentType")
  expect_equal(types, c(
    "application/vnd.apache.parquet", "application/vnd.apache.parquet", "application/geo+json",
    "text/plain; charset=utf-8", "application/vnd.apache.parquet", "application/vnd.apache.parquet",
    "application/json"
  ))
  expect_equal(sent[[7]]$Body, readBin(files[7], "raw", file.size(files[7])))
  expect_equal(sent[[1]]$CacheControl, "max-age=300")
})

test_that("a custom endpoint switches the S3 client to path-style addressing", {
  captured <- NULL
  local_mocked_bindings(s3 = function(config) captured <<- config, .package = "paws.storage")
  s3_client(list(region = "auto", endpoint = "https://acct.r2.cloudflarestorage.com"))
  expect_equal(captured$endpoint, "https://acct.r2.cloudflarestorage.com")
  expect_true(captured$s3_force_path_style)
  s3_client(list(region = "eu-west-1", endpoint = ""))
  expect_null(captured$endpoint)
})

test_that("publish_config reads the environment and trims the prefix", {
  withr::local_envvar(S3_BUCKET = "b", S3_PREFIX = "some/prefix/", S3_ENDPOINT = "", AWS_REGION = "", SNAPSHOTS = "")
  cfg <- publish_config()
  expect_equal(cfg$bucket, "b")
  expect_equal(cfg$prefix, "some/prefix")
  expect_true(cfg$snapshots)
  withr::local_envvar(SNAPSHOTS = "FALSE")
  expect_false(publish_config()$snapshots)
})
