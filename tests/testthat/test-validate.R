carlow <- function() {
  p <- finalise_properties(tidy_properties(fixture("carlow.geojson"), "CARLOW COUNTY COUNCIL"))
  list(props = p, floors = finalise_floors(tidy_floors(fixture("carlow.geojson")), p))
}

test_that("a healthy refresh passes with a deterministic summary", {
  d <- carlow()
  qa <- validate_refresh(d$props, d$floors, "CARLOW COUNTY COUNCIL")
  expect_equal(qa$by_authority$`CARLOW COUNTY COUNCIL`, 4L)
  expect_equal(qa$total, 4)
  expect_equal(qa$with_coordinates, 2)
  expect_identical(qa, validate_refresh(d$props, d$floors, "CARLOW COUNTY COUNCIL"))
})

test_that("an authority with no records fails", {
  d <- carlow()
  expect_error(
    validate_refresh(d$props, d$floors, c("CARLOW COUNTY COUNCIL", "CAVAN COUNTY COUNCIL")),
    "No records returned for: CAVAN COUNTY COUNCIL"
  )
})

test_that("a big fall since the last refresh fails, a small one passes", {
  d <- carlow()
  big <- list(rows = list(by_authority = list(`CARLOW COUNTY COUNCIL` = 100L)))
  small <- list(rows = list(by_authority = list(`CARLOW COUNTY COUNCIL` = 5L)))
  expect_error(validate_refresh(d$props, d$floors, "CARLOW COUNTY COUNCIL", big), "fell by more than 20%")
  expect_no_error(validate_refresh(d$props, d$floors, "CARLOW COUNTY COUNCIL", small))
})

test_that("coordinates outside Ireland fail, as from a CRS mix-up", {
  d <- carlow()
  d$props$x_itm[!is.na(d$props$x_itm)] <- -6.9 # degrees where metres belong
  expect_error(validate_refresh(d$props, d$floors, "CARLOW COUNTY COUNCIL"), "outside Ireland")
})

test_that("thresholds come from the environment", {
  d <- carlow()
  withr::local_envvar(MIN_XY_SHARE = "0.9")
  expect_error(validate_refresh(d$props, d$floors, "CARLOW COUNTY COUNCIL"), "have coordinates")
})

test_that("the previous manifest comes from disk, then the bucket", {
  dir <- withr::local_tempdir()
  expect_null(read_previous_manifest(dir, list(bucket = "")))

  jsonlite::write_json(list(rows = list(total = 7)), file.path(dir, "manifest.json"), auto_unbox = TRUE)
  expect_equal(read_previous_manifest(dir, list(bucket = ""))$rows$total, 7)

  fake_s3 <- list(get_object = function(Bucket, Key) { # nolint: object_name_linter. paws argument names
    expect_equal(Key, "pre/latest/manifest.json")
    list(Body = charToRaw('{"rows": {"total": 9}}'))
  })
  local_mocked_bindings(s3_client = function(config) fake_s3)
  empty <- withr::local_tempdir()
  got <- read_previous_manifest(empty, list(bucket = "b", prefix = "pre", endpoint = "", region = "auto"))
  expect_equal(got$rows$total, 9)
})
