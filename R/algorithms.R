# algorithms.R
# R wrappers around the C++ solvers and a pure-R simulated annealing implementation.
#
# Design decision: the C++ functions (nn_heuristic_cpp, two_opt_cpp) are never
# called directly by users. These R wrappers sit in front of them and handle
# all input validation before handing off to C++. This way the C++ code stays
# clean and fast with no defensive overhead.

#' Nearest Neighbour TSP heuristic
#'
#' Builds an initial tour greedily: starting from \code{start}, always moves
#' to the closest unvisited city. Fast O(n^2) construction heuristic.
#' The inner loop is implemented in C++ via \code{nn_heuristic_cpp()}.
#'
#' @param dist_mat Numeric n x n distance matrix.
#' @param start    Integer. Starting city index, 1-indexed (default 1).
#' @return Integer vector: city visit order, 1-indexed, length n.
#' @export
#' @examples
#' df <- data.frame(
#'   name = c("Warsaw", "Paris", "London"),
#'   lat  = c(52.23, 48.85, 51.51),
#'   lng  = c(21.01,  2.35,  -0.12)
#' )
#' dm <- haversine_matrix(df$lat, df$lng)
#' nn_heuristic(dm)
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
#' Iteratively improves a tour by trying all pairs of edge swaps.
#' If reversing the segment between edges (i, i+1) and (j, j+1) reduces
#' total distance, the swap is applied. Repeats until no improvement exists.
#' The inner loop is implemented in C++ via \code{two_opt_cpp()}.
#'
#' @param tour     Integer vector: current tour (1-indexed).
#' @param dist_mat Numeric n x n distance matrix.
#' @return Improved integer tour vector (1-indexed).
#' @export
#' @examples
#' df <- data.frame(
#'   name = c("Warsaw", "Paris", "London"),
#'   lat  = c(52.23, 48.85, 51.51),
#'   lng  = c(21.01,  2.35,  -0.12)
#' )
#' dm   <- haversine_matrix(df$lat, df$lng)
#' tour <- nn_heuristic(dm)
#' two_opt(tour, dm)
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
  if (any(tour < 1) || any(tour > nrow(dist_mat))) {
    stop("All 'tour' values must be valid city indices.", call. = FALSE)
  }

  two_opt_cpp(as.integer(tour), dist_mat)
}

#' Simulated Annealing TSP solver (pure R)
#'
#' A metaheuristic that escapes local optima by occasionally accepting
#' worse solutions with probability exp(-delta / temp). Temperature decreases
#' each iteration via the cooling rate, reducing exploration over time.
#'
#' Uses a 2-opt swap as the neighbourhood move: two random positions are
#' selected and the segment between them is reversed.
#'
#' @param dist_mat    Numeric n x n distance matrix.
#' @param start_tour  Integer vector: starting tour. If NULL, uses
#'                    \code{nn_heuristic()} to build one.
#' @param temp        Numeric. Initial temperature — higher values allow
#'                    more exploration early on (default 1000).
#' @param cooling     Numeric. Multiplicative cooling rate between 0 and 1
#'                    (default 0.995). Values closer to 1 cool more slowly.
#' @param iterations  Integer. Number of swap attempts per temperature
#'                    level (default 100).
#' @return Integer vector: optimised tour (1-indexed).
#' @export
#' @examples
#' df <- data.frame(
#'   name = c("Warsaw", "Paris", "London", "Berlin", "Amsterdam"),
#'   lat  = c(52.23, 48.85, 51.51, 52.52, 52.37),
#'   lng  = c(21.01,  2.35, -0.12, 13.40,  4.90)
#' )
#' dm <- haversine_matrix(df$lat, df$lng)
#' simulated_annealing(dm, temp = 500, cooling = 0.99)
simulated_annealing <- function(dist_mat,
                                start_tour = NULL,
                                temp       = 1000,
                                cooling    = 0.995,
                                iterations = 100L) {
  if (!is.matrix(dist_mat) || !is.numeric(dist_mat)) {
    stop("'dist_mat' must be a numeric matrix.", call. = FALSE)
  }
  validate_numeric(temp,       "temp",       min_val = 1e-6)
  validate_numeric(cooling,    "cooling",    min_val = 1e-6, max_val = 1 - 1e-6)
  validate_numeric(iterations, "iterations", min_val = 1)

  n <- nrow(dist_mat)

  # Build starting tour from NN heuristic if none provided
  tour <- if (is.null(start_tour)) {
    nn_heuristic(dist_mat)
  } else {
    as.integer(start_tour)
  }

  best_tour <- tour
  best_dist <- tour_distance(tour, dist_mat)
  cur_dist  <- best_dist

  while (temp > 1e-3) {
    for (i in seq_len(iterations)) {

      # Pick two random positions and reverse the segment between them
      idx      <- sort(sample.int(n, 2L))
      new_tour <- tour
      new_tour[idx[1]:idx[2]] <- rev(tour[idx[1]:idx[2]])

      new_dist <- tour_distance(new_tour, dist_mat)
      delta    <- new_dist - cur_dist

      # Always accept improvements; accept worse solutions with
      # probability exp(-delta / temp) — the Boltzmann acceptance criterion
      if (delta < 0 || runif(1L) < exp(-delta / temp)) {
        tour     <- new_tour
        cur_dist <- new_dist

        # Track the best solution seen across all iterations
        if (cur_dist < best_dist) {
          best_tour <- tour
          best_dist <- cur_dist
        }
      }
    }

    temp <- temp * cooling   # reduce temperature each pass
  }

  best_tour
}
