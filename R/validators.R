# validators.R
# Defensive programming utilities used throughout the package.
# These functions are internal — users never call them directly.
# Every public function calls the relevant validator before doing any work.

#' @noRd
validate_numeric <- function(val, name, min_val = -Inf, max_val = Inf) {
  if (!is.numeric(val) || length(val) != 1) {
    stop(sprintf(
      "'%s' must be a single numeric value, not %s of length %d.",
      name, class(val)[1], length(val)
    ), call. = FALSE)
  }
  if (is.na(val)) {
    stop(sprintf("'%s' must not be NA.", name), call. = FALSE)
  }
  if (val < min_val || val > max_val) {
    stop(sprintf(
      "'%s' must be between %g and %g, got %g.",
      name, min_val, max_val, val
    ), call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
validate_character <- function(val, name) {
  if (!is.character(val) || length(val) != 1) {
    stop(sprintf(
      "'%s' must be a single character string, not %s.",
      name, class(val)[1]
    ), call. = FALSE)
  }
  if (is.na(val) || nchar(trimws(val)) == 0) {
    stop(sprintf("'%s' must be a non-empty string.", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
validate_numeric_vector <- function(vec, name, min_val = -Inf, max_val = Inf) {
  if (!is.numeric(vec)) {
    stop(sprintf(
      "'%s' must be a numeric vector, not %s.", name, class(vec)[1]
    ), call. = FALSE)
  }
  if (length(vec) == 0) {
    stop(sprintf("'%s' must not be empty.", name), call. = FALSE)
  }
  if (anyNA(vec)) {
    stop(sprintf("'%s' must not contain NA values.", name), call. = FALSE)
  }
  if (any(vec < min_val) || any(vec > max_val)) {
    stop(sprintf(
      "All values of '%s' must be between %g and %g.",
      name, min_val, max_val
    ), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate a location data frame
#'
#' Checks that a data frame is suitable for TSP solving: must have columns
#' \code{name}, \code{lat}, \code{lng}, at least \code{min_n} rows, valid
#' coordinate ranges, and no NA or empty names.
#'
#' @param df    A data frame to validate.
#' @param min_n Minimum number of rows required (default 2).
#' @return \code{invisible(TRUE)} on success; throws an informative error otherwise.
#' @export
#' @examples
#' df <- data.frame(name = c("Warsaw", "Paris"),
#'                  lat  = c(52.23, 48.85),
#'                  lng  = c(21.01,  2.35))
#' validate_location_df(df)
validate_location_df <- function(df, min_n = 2L) {
  if (!is.data.frame(df)) {
    stop("Input must be a data.frame.", call. = FALSE)
  }
  required <- c("name", "lat", "lng")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(sprintf(
      "Data frame must contain columns: %s.  Missing: %s.",
      paste(required, collapse = ", "),
      paste(missing,  collapse = ", ")
    ), call. = FALSE)
  }
  if (nrow(df) < min_n) {
    stop(sprintf(
      "At least %d locations required, got %d.", min_n, nrow(df)
    ), call. = FALSE)
  }
  if (!is.character(df$name)) {
    stop("Column 'name' must be character.", call. = FALSE)
  }
  if (anyNA(df$name) || any(nchar(trimws(df$name)) == 0)) {
    stop("Column 'name' must not contain NA or empty strings.", call. = FALSE)
  }
  validate_numeric_vector(df$lat, "lat", -90,   90)
  validate_numeric_vector(df$lng, "lng", -180, 180)
  invisible(TRUE)
}
