# distance.R — vectorized geographic distance calculations

#' Compute a full haversine distance matrix (vectorized)
#'
#' Uses matrix outer products to compute all pairwise great-circle distances
#' simultaneously — no R-level loops. This is the vectorisation showcase of the package.
#'
#' @param lat Numeric vector of latitudes  (decimal degrees, -90 to 90).
#' @param lng Numeric vector of longitudes (decimal degrees, -180 to 180).
#' @return Symmetric n x n matrix of distances in kilometres.
#' @export
#' @examples
#' lat <- c(52.23, 48.85, 51.51)   # Warsaw, Paris, London
#' lng <- c(21.01,  2.35,  -0.12)
#' haversine_matrix(lat, lng)
haversine_matrix <- function(lat, lng) {
  validate_numeric_vector(lat, "lat", -90,  90)
  validate_numeric_vector(lng, "lng", -180, 180)
  if (length(lat) != length(lng)) {
    stop("'lat' and 'lng' must have the same length.", call. = FALSE)
  }

  n   <- length(lat)
  R   <- 6371.0                  # Earth mean radius in km

  # Convert degrees → radians once (vectorized)
  lat_r <- lat * (pi / 180)
  lng_r <- lng * (pi / 180)

  # Build n×n coordinate matrices using matrix() — the key vectorisation step.
  # Each cell (i,j) holds the coordinate of point i (byrow=FALSE) or point j (byrow=TRUE).
  lat_i <- matrix(lat_r, nrow = n, ncol = n, byrow = FALSE)
  lat_j <- matrix(lat_r, nrow = n, ncol = n, byrow = TRUE)
  lng_i <- matrix(lng_r, nrow = n, ncol = n, byrow = FALSE)
  lng_j <- matrix(lng_r, nrow = n, ncol = n, byrow = TRUE)

  dlat <- lat_j - lat_i
  dlng <- lng_j - lng_i

  # Haversine formula applied element-wise to entire matrix at once
  a <- sin(dlat / 2)^2 + cos(lat_i) * cos(lat_j) * sin(dlng / 2)^2
  # pmin guards against floating-point values slightly above 1 that would give NaN
  d <- 2 * R * asin(pmin(1.0, sqrt(a)))

  diag(d) <- 0.0
  d
}

#' Compute the total distance of a closed tour
#'
#' Uses vectorized matrix indexing — no loop.
#'
#' @param tour     Integer vector of location indices (1-indexed).
#' @param dist_mat Square numeric distance matrix.
#' @return Total distance in km (closed tour: last city returns to first).
#' @export
tour_distance <- function(tour, dist_mat) {
  if (!is.numeric(tour) || anyNA(tour)) {
    stop("'tour' must be a numeric vector with no NAs.", call. = FALSE)
  }
  n    <- length(tour)
  from <- tour
  to   <- c(tour[-1], tour[1])          # shift by one, wrap last→first
  sum(dist_mat[cbind(from, to)])         # vectorized 2-column matrix index
}
