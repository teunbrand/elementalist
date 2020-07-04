unit_type <- NULL
.onLoad <- function(...) {
  theme_registration()
  if (as.numeric(version$major) >= 4) {
    unit_type <- getFromNamespace("unitType", "grid")
  } else {
    unit_type <- function(x) {attr(x, "unit", exact = TRUE)}
  }
  utils::assignInMyNamespace("unit_type", unit_type)
}
