#' @importFrom R6 R6Class
NULL

#' TSP Solver
#'
#' R6 class that solves the Traveling Salesman Problem using the
#' Nearest Neighbour heuristic followed by 2-opt local search improvement.
#' Both algorithms are implemented in C++ via Rcpp.
#'
#' @export
#' @examples
#' df <- data.frame(
#'   name = c("Warsaw", "Paris", "London", "Berlin"),
#'   lat  = c(52.23, 48.85, 51.51, 52.52),
#'   lng  = c(21.01,  2.35, -0.12, 13.40)
#' )
#' solver <- TSPSolver$new(df)
#' route  <- solver$solve()
#' print(route)
TSPSolver <- R6::R6Class(
  classname = "TSPSolver",

  private = list(
    .locations = NULL,
    .dist_mat  = NULL
  ),

  active = list(
    #' @field n_locations Number of loaded locations (read-only).
    n_locations = function(value) {
      if (!missing(value)) stop("'n_locations' is read-only.", call. = FALSE)
      if (is.null(private$.locations)) 0L else private$.locations$n
    },

    #' @field locations The loaded LocationSet (read-only).
    locations = function(value) {
      if (!missing(value)) stop("'locations' is read-only.", call. = FALSE)
      private$.locations
    }
  ),

  public = list(

    #' @description
    #' Load locations and precompute the distance matrix.
    #' @param df Data frame with columns: name, lat, lng.
    initialize = function(df) {
      validate_location_df(df, min_n = 2L)
      private$.locations <- new_location_set(df)
      private$.dist_mat  <- location_set_dist_matrix(private$.locations)
      message(sprintf("TSPSolver ready: %d locations loaded.",
                      private$.locations$n))
      invisible(self)
    },

    #' @description
    #' Solve the TSP using Nearest Neighbour + 2-opt (both in C++).
    #' @return A \code{tsp_route} object.
    solve = function() {
      if (is.null(private$.locations)) {
        stop("No locations loaded.", call. = FALSE)
      }

      dm <- private$.dist_mat
      t0 <- proc.time()[["elapsed"]]

      # Step 1 — build initial tour greedily (C++)
      tour <- nn_heuristic_cpp(dm, 1L)

      # Step 2 — improve with 2-opt local search (C++)
      tour <- two_opt_cpp(as.integer(tour), dm)

      elapsed <- proc.time()[["elapsed"]] - t0

      new_tsp_route(
        tour      = tour,
        loc_set   = private$.locations,
        dist_mat  = dm,
        algorithm = "nn+2opt",
        elapsed   = elapsed
      )
    },

    #' @description Print solver state.
    print = function(...) {
      cat("<TSPSolver>\n")
      cat("  Locations:", self$n_locations, "\n")
      invisible(self)
    }
  )
)
