# distance.R
# Vectorised geographic distance calculations.
# haversine_matrix() is the vectorisation showcase of the package —
# it computes all n^2 pairwise distances simultaneously using matrix
# outer products, with zero R-level loops.

#' Compute a haversine distance matrix (vectorised R)
#'
#' Calculates pairwise great-circle distances between all locations using
#' the haversine formula. The entire computation is vectorised using matrix
#' outer products — no R-level loops anywhere.
#'
#' @param lat Numeric vector of latitudes  (decimal degrees, -90 to 90).
#' @param lng Numeric vector of longitudes (decimal degrees, -180 to 180).
#' @return Symmetric n x n numeric matrix of distances in kilometres.
#' @export
#' @examples
#' lat <- c(52.23, 48.85, 51.51)
#' lng <- c(21.01,  2.35,  -0.12)
#' haversine_matrix(lat, lng)
haversine_matrix <- function(lat, lng) {
  validate_numeric_vector(lat, "lat", -90,   90)
  validate_numeric_vector(lng, "lng", -180, 180)
  if (length(lat) != length(lng)) {
    stop("'lat' and 'lng' must have the same length.", call. = FALSE)
  }

  n <- length(lat)
  R <- 6371.0   # Earth mean radius in km

  # Convert degrees to radians once — applied to the whole vector at once
  lat_r <- lat * (pi / 180)
  lng_r <- lng * (pi / 180)

  # Build n x n coordinate matrices using matrix()
  # lat_i[i,j] = latitude of point i  (same value repeated across columns)
  # lat_j[i,j] = latitude of point j  (same value repeated down rows)
  # This lets us compute all (i,j) differences in one vectorised step
  lat_i <- matrix(lat_r, nrow = n, ncol = n, byrow = FALSE)
  lat_j <- matrix(lat_r, nrow = n, ncol = n, byrow = TRUE)
  lng_i <- matrix(lng_r, nrow = n, ncol = n, byrow = FALSE)
  lng_j <- matrix(lng_r, nrow = n, ncol = n, byrow = TRUE)

  dlat <- lat_j - lat_i
  dlng <- lng_j - lng_i

  # Haversine formula — applied element-wise to full n x n matrices at once
  a <- sin(dlat / 2)^2 + cos(lat_i) * cos(lat_j) * sin(dlng / 2)^2

  # pmin(1, ...) guards against floating-point values just above 1.0
  # which would produce NaN inside asin()
  d <- matrix(2 * R * asin(pmin(1.0, sqrt(a))), nrow = n, ncol = n)

  diag(d) <- 0.0   # self-distance must be exactly zero
  d
}

#' Compute the total distance of a closed tour
#'
#' Sums the distances between consecutive cities in the tour, including the
#' return leg from the last city back to the first. Uses vectorised matrix
#' indexing — no loop.
#'
#' @param tour     Integer vector of location indices (1-indexed).
#' @param dist_mat Square numeric distance matrix.
#' @return Single numeric value: total tour distance in kilometres.
#' @export
#' @examples
#' lat <- c(52.23, 48.85, 51.51)
#' lng <- c(21.01,  2.35,  -0.12)
#' dm  <- haversine_matrix(lat, lng)
#' tour_distance(c(1L, 2L, 3L), dm)
tour_distance <- function(tour, dist_mat) {
  if (!is.numeric(tour) || anyNA(tour)) {
    stop("'tour' must be a numeric vector with no NAs.", call. = FALSE)
  }
  if (!is.matrix(dist_mat) || !is.numeric(dist_mat)) {
    stop("'dist_mat' must be a numeric matrix.", call. = FALSE)
  }

  n    <- length(tour)
  from <- tour
  to   <- c(tour[-1], tour[1])

  # cbind(from, to) creates a 2-column index matrix.
  # dist_mat[cbind(from, to)] extracts one element per row — fully vectorised.
  sum(dist_mat[cbind(from, to)])
}
