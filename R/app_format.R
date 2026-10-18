# Number formatting ------------------------------------------------------------
# Non-ASCII characters are escaped so the package source stays portable.

EURO <- "\u20ac"
MINUS <- "\u2212"
SQM <- "m\u00b2"

fmt_int <- function(x) ifelse(is.na(x), "", formatC(round(x), format = "d", big.mark = ","))

fmt_eur <- function(x, signed = FALSE) {
  out <- paste0(EURO, fmt_int(abs(x)))
  sign <- ifelse(x < 0, MINUS, if (signed) "+" else "")
  ifelse(is.na(x), "", paste0(sign, out))
}

fmt_valuation <- function(x, basis) {
  rv <- paste("RV", formatC(x, format = "f", digits = 2, big.mark = ","))
  ifelse(is.na(x), "", ifelse(basis == "RV", rv, fmt_eur(x)))
}

fmt_rate <- function(x) ifelse(is.finite(x), paste0(EURO, fmt_int(x), "/", SQM), "")

fmt_area <- function(x) ifelse(is.finite(x) & x > 0, paste(fmt_int(x), SQM), "")

fmt_distance <- function(m) {
  km <- formatC(m / 1000, format = "f", digits = 1)
  ifelse(m < 1000, paste(fmt_int(signif(m, 2)), "m"), paste(km, "km"))
}

fmt_date <- function(d) ifelse(is.na(d), "", trimws(format(d, "%e %b %Y")))
