# solver.R
# R6 TSPSolver class — the main interface users interact with.
#
# Design decision: strategy pattern.
# The algorithm is a swappable component set via set_algorithm().
# solve() always returns the same tsp_route type regardless of which
# algorithm is active. This means the Shiny app and any other code
# only needs to know about TSPSolver and tsp_route — not about the
# individual algorithm functions.

#' @importFrom R6 R6Class
NULL

#' TSP Solver
#'
#' An R6 class that wraps all TSP algorithms behind a single consistent
#' interface. Load locations once, then switch between algorithms freely
#' using \code{set_algorithm()} and call \code{solve()} to get a result.
#'
#' @export
#' @examples
#' df <- data.frame(
#'   name = c("Warsaw", "Paris", "London", "Berlin", "Amsterdam"),
#'   lat  = c(52.23, 48.85, 51.51, 52.52, 52.37),
#'   lng  = c(21.01,  2.35, -0.12, 13.40,  4.90)
#' )
#' solver <- TSPSolver$new(df)
#' solver$set_algorithm("nn+2opt")
#' route  <- solver$solve()
#' print(route)
TSPSolver <- R6::R6Class(
  classname = "TSPSolver",

  # Private fields — cannot be accessed or modified directly from outside
  # the class. Only public methods can touch these.
  private = list(
    .locations  = NULL,   # tsp_location_set object
    .dist_mat   = NULL,   # precomputed n x n distance matrix
    .algorithm  = "nn+2opt",
    .params     = list()  # extra parameters forwarded to the algorithm
  ),

  # Active bindings — behave like read-only public fields.
  # Users can read solver$n_locations but cannot assign to it.
  active = list(

    #' @field n_locations Number of locations currently loaded (read-only).
    n_locations = function(value) {
      if (!missing(value)) stop("'n_locations' is read-only.", call. = FALSE)
      if (is.null(private$.locations)) 0L else private$.locations$n
    },

    #' @field algorithm Currently selected algorithm name (read-only).
    algorithm = function(value) {
      if (!missing(value)) stop("'algorithm' is read-only.", call. = FALSE)
      private$.algorithm
    },

    #' @field locations The loaded LocationSet (read-only).
    locations = function(value) {
      if (!missing(value)) stop("'locations' is read-only.", call. = FALSE)
      private$.locations
    }
  ),

  public = list(

    #' @description
    #' Create a new TSPSolver and load locations.
    #' @param df Data frame with columns: name, lat, lng.
    initialize = function(df) {
      validate_location_df(df, min_n = 2L)
      private$.locations <- new_location_set(df)
      private$.dist_mat  <- location_set_dist_matrix(private$.locations)
      message(sprintf(
        "TSPSolver ready: %d locations loaded.", private$.locations$n
      ))
      invisible(self)
    },

    #' @description
    #' Set the solving algorithm and any extra parameters.
    #' @param algo One of: \code{"nn"}, \code{"two_opt"},
    #'   \code{"nn+2opt"}, \code{"sa"}.
    #' @param ... Extra parameters forwarded to the algorithm.
    #'   For \code{"sa"}: \code{temp}, \code{cooling}, \code{iterations}.
    set_algorithm = function(algo, ...) {
      valid <- c("nn", "two_opt", "nn+2opt", "sa")
      if (!algo %in% valid) {
        stop(sprintf(
          "Unknown algorithm '%s'. Choose from: %s.",
          algo, paste(valid, collapse = ", ")
        ), call. = FALSE)
      }
      private$.algorithm <- algo
      private$.params    <- list(...)
      message(sprintf("Algorithm set to '%s'.", algo))
      invisible(self)
    },

    #' @description
    #' Solve the TSP with the selected algorithm.
    #' @return A \code{tsp_route} object.
    solve = function() {
      if (is.null(private$.locations)) {
        stop("No locations loaded. Initialise with TSPSolver$new(df).",
             call. = FALSE)
      }

      dm   <- private$.dist_mat
      algo <- private$.algorithm
      t0   <- proc.time()[["elapsed"]]

      tour <- switch(algo,

                     "nn" = {
                       nn_heuristic(dm)
                     },

                     "two_opt" = {
                       # 2-opt alone starts from NN — 2-opt is an improver, not a constructor
                       init <- nn_heuristic(dm)
                       two_opt(init, dm)
                     },

                     "nn+2opt" = {
                       # Most balanced option: fast greedy construction + local search polish
                       init <- nn_heuristic(dm)
                       two_opt(init, dm)
                     },

                     "sa" = {
                       # SA starts from NN tour for a warm start — reduces computation time
                       init <- nn_heuristic(dm)
                       do.call(
                         simulated_annealing,
                         c(list(dist_mat = dm, start_tour = init), private$.params)
                       )
                     }
      )

      elapsed <- proc.time()[["elapsed"]] - t0

      new_tsp_route(
        tour      = tour,
        loc_set   = private$.locations,
        dist_mat  = dm,
        algorithm = algo,
        elapsed   = elapsed
      )
    },

    #' @description
    #' Run all algorithms and return a comparison data frame.
    #' Useful for demonstrating performance differences between algorithms.
    #' @return Data frame with columns: algorithm, distance, elapsed.
    benchmark = function() {
      if (is.null(private$.locations)) {
        stop("No locations loaded.", call. = FALSE)
      }

      algos   <- c("nn", "nn+2opt", "sa")
      results <- vector("list", length(algos))

      for (i in seq_along(algos)) {
        self$set_algorithm(algos[i])
        r          <- self$solve()
        results[[i]] <- data.frame(
          algorithm = algos[i],
          distance  = round(r$distance, 2),
          elapsed   = round(r$elapsed,  5),
          stringsAsFactors = FALSE
        )
      }

      do.call(rbind, results)
    },

    #' @description Print the solver's current state.
    print = function(...) {
      cat("<TSPSolver>\n")
      cat("  Locations :", self$n_locations, "\n")
      cat("  Algorithm :", private$.algorithm, "\n")
      if (length(private$.params) > 0) {
        cat("  Params    :", paste(names(private$.params),
                                   unlist(private$.params),
                                   sep = "=", collapse = ", "), "\n")
      }
      invisible(self)
    }
  )
)
