# algorithms.R
# R wrappers around the C++ solvers.

#' Nearest Neighbour TSP heuristic
#'
#' Builds a tour greedily: always moves to the closest unvisited city.
#' Implemented in C++ via \code{nn_heuristic_cpp()}.
#'
#' @param dist_mat Numeric n x n distance matrix.
#' @param start    Integer. Starting city index, 1-indexed (default 1).
#' @return Integer vector: city visit order, 1-indexed.
#' @export
nn_heuristic <- function(dist_mat, start = 1L) {
  if (!is.matrix(dist_mat) || !is.numeric(dist_mat)) {
    stop("'dist_mat' must be a numeric matrix.", call. = FALSE)
  }
  if (nrow(dist_mat) != ncol(dist_mat)) {
    stop("'dist_mat' must be square.", call. = FALSE)
  }
  if (nrow(dist_mat) < 2) {
    stop("'dist_mat' must have at least 2 cities.", call. = FALSE)
  }
  validate_numeric(as.numeric(start), "start", 1, nrow(dist_mat))
  nn_heuristic_cpp(dist_mat, as.integer(start))
}

#' 2-opt local search improvement
#'
#' Improves a tour by trying all edge swap pairs. Implemented in C++
#' via \code{two_opt_cpp()}.
#'
#' @param tour     Integer vector: current tour (1-indexed).
#' @param dist_mat Numeric n x n distance matrix.
#' @return Improved integer tour vector.
#' @export
two_opt <- function(tour, dist_mat) {
  if (!is.matrix(dist_mat) || !is.numeric(dist_mat)) {
    stop("'dist_mat' must be a numeric matrix.", call. = FALSE)
  }
  if (!is.numeric(tour) || anyNA(tour)) {
    stop("'tour' must be a numeric vector with no NAs.", call. = FALSE)
  }
  if (length(tour) != nrow(dist_mat)) {
    stop(sprintf(
      "'tour' length (%d) must equal number of cities (%d).",
      length(tour), nrow(dist_mat)
    ), call. = FALSE)
  }
  two_opt_cpp(as.integer(tour), dist_mat)
}
