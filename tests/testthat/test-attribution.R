test_that("the statement follows data.gov.ie's CC BY 4.0 wording and names the provider", {
  a <- data_attribution()
  expect_match(a$statement, "^Contains Irish Public Sector Data \\(Tailte \u00c9ireann\\) licensed under a")
  expect_match(a$statement, "Creative Commons Attribution 4.0 International (CC BY 4.0) licence.", fixed = TRUE)
  expect_equal(a$licence_url, "https://creativecommons.org/licenses/by/4.0/")
  expect_true(length(a$changes) >= 3)
  expect_match(a$disclaimer, "does not endorse")
})

test_that("the published notice carries licence, source, changes and how to report errors", {
  txt <- paste(attribution_text(), collapse = "\n")
  expect_match(txt, "https://creativecommons.org/licenses/by/4.0/", fixed = TRUE)
  expect_match(txt, "https://tailte.ie/home/api/", fixed = TRUE)
  expect_match(txt, "Changes made by this project")
  expect_match(txt, "opendata@tailte.ie", fixed = TRUE)
  expect_match(txt, "Rateable Valuation")
})
