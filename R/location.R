# location.R
# S3 classes for individual locations and sets of locations.
# new_location()     → single city as a typed object
# new_location_set() → collection of cities, validates and stores as a unit
# Both classes get print methods so they display cleanly in the console.

#' Create a single Location object (S3)
#'
#' @param name Character. Human-readable place name.
#' @param lat  Numeric. Latitude in decimal degrees (-90 to 90).
#' @param lng  Numeric. Longitude in decimal degrees (-180 to 180).
#' @return Object of class \code{tsp_location}.
#' @export
#' @examples
#' loc <- new_location("Warsaw", 52.23, 21.01)
#' print(loc)
new_location <- function(name, lat, lng) {
  validate_character(name, "name")
  validate_numeric(lat, "lat", -90,   90)
  validate_numeric(lng, "lng", -180, 180)

  structure(
    list(name = trimws(name), lat = lat, lng = lng),
    class = "tsp_location"
  )
}

#' @export
print.tsp_location <- function(x, ...) {
  cat(sprintf(
    "<tsp_location>  %s  [%.4f N, %.4f E]\n",
    x$name, x$lat, x$lng
  ))
  invisible(x)
}

#' Create a LocationSet from a data frame (S3)
#'
#' Validates and wraps a data frame of locations into a typed object.
#' All downstream functions (distance matrix, solver) accept this type.
#'
#' @param df Data frame with columns \code{name}, \code{lat}, \code{lng}.
#' @return Object of class \code{tsp_location_set}.
#' @export
#' @examples
#' df <- data.frame(
#'   name = c("Warsaw", "Paris", "London"),
#'   lat  = c(52.23, 48.85, 51.51),
#'   lng  = c(21.01,  2.35,  -0.12)
#' )
#' ls <- new_location_set(df)
#' print(ls)
new_location_set <- function(df) {
  validate_location_df(df, min_n = 2L)

  # Keep only the three required columns in a fixed order
  # Defensive: drops any extra columns the user may have passed in
  df       <- df[, c("name", "lat", "lng")]
  df$name  <- trimws(df$name)

  structure(
    list(locations = df, n = nrow(df)),
    class = "tsp_location_set"
  )
}

#' @export
print.tsp_location_set <- function(x, ...) {
  cat(sprintf("<tsp_location_set> with %d locations:\n", x$n))
  print(x$locations, row.names = TRUE)
  invisible(x)
}

#' @export
as.data.frame.tsp_location_set <- function(x, ...) {
  x$locations
}

#' Build a distance matrix from a LocationSet
#'
#' Convenience wrapper: extracts coordinates and calls
#' \code{haversine_matrix()}.
#'
#' @param loc_set A \code{tsp_location_set} object.
#' @return Symmetric numeric matrix of distances in kilometres.
#' @export
#' @examples
#' df <- data.frame(
#'   name = c("Warsaw", "Paris", "London"),
#'   lat  = c(52.23, 48.85, 51.51),
#'   lng  = c(21.01,  2.35,  -0.12)
#' )
#' ls <- new_location_set(df)
#' location_set_dist_matrix(ls)
location_set_dist_matrix <- function(loc_set) {
  if (!inherits(loc_set, "tsp_location_set")) {
    stop("'loc_set' must be a tsp_location_set object.", call. = FALSE)
  }
  haversine_matrix(loc_set$locations$lat, loc_set$locations$lng)
}
