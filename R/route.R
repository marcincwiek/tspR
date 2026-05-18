# route.R
# S3 class for a completed TSP solution.
# new_tsp_route() is internal — only the solver creates route objects.
# Users interact with them via print(), summary(), and plot().

#' Create a TSP route solution object (S3)
#'
#' @param tour      Integer vector: city visit order (1-indexed).
#' @param loc_set   A \code{tsp_location_set} object.
#' @param dist_mat  The distance matrix used to solve.
#' @param algorithm Character. Name of the algorithm used.
#' @param elapsed   Numeric. Computation time in seconds.
#' @return Object of class \code{tsp_route}.
#' @keywords internal
new_tsp_route <- function(tour, loc_set, dist_mat, algorithm, elapsed) {
  structure(
    list(
      tour      = tour,
      loc_set   = loc_set,
      dist_mat  = dist_mat,
      algorithm = algorithm,
      distance  = tour_distance(tour, dist_mat),
      elapsed   = elapsed
    ),
    class = "tsp_route"
  )
}

#' Print a TSP route solution
#'
#' Shows the algorithm used, total distance, computation time,
#' and the city visit order.
#'
#' @param x   A \code{tsp_route} object.
#' @param ... Ignored.
#' @return \code{x} invisibly.
#' @export
print.tsp_route <- function(x, ...) {
  names  <- x$loc_set$locations$name
  # Build the route string: A -> B -> C -> A (closed loop)
  order  <- names[x$tour]
  cat("=== TSP Solution ===\n")
  cat("Algorithm :", x$algorithm, "\n")
  cat("Distance  :", round(x$distance, 2), "km\n")
  cat("Time      :", round(x$elapsed,  4), "s\n")
  cat("Route     :", paste(order, collapse = " -> "), "\n")
  invisible(x)
}

#' Summarise a TSP route solution
#'
#' Prints a leg-by-leg breakdown of the tour showing the distance
#' of each individual segment.
#'
#' @param object A \code{tsp_route} object.
#' @param ...    Ignored.
#' @return \code{object} invisibly.
#' @export
summary.tsp_route <- function(object, ...) {
  locs <- object$loc_set$locations
  n    <- length(object$tour)
  tour <- object$tour

  cat("=== Leg-by-leg breakdown ===\n")
  cat(sprintf("%-30s  %8s\n", "Leg", "km"))
  cat(strrep("-", 40), "\n")

  for (i in seq_len(n - 1)) {
    from  <- tour[i]
    to    <- tour[i + 1L]
    d     <- object$dist_mat[from, to]
    label <- paste(locs$name[from], "->", locs$name[to])
    cat(sprintf("%-30s  %8.2f\n", label, d))
  }

  cat(strrep("-", 40), "\n")
  cat(sprintf("%-30s  %8.2f\n", "TOTAL", object$distance))
  invisible(object)
}

#' Plot a TSP route
#'
#' Draws the cities and the optimised route on a base R scatter plot.
#' Green square marks the starting city.
#'
#' @param x   A \code{tsp_route} object.
#' @param ... Passed to \code{plot()}.
#' @return \code{x} invisibly.
#' @export
plot.tsp_route <- function(x, ...) {
  locs  <- x$loc_set$locations
  route <- locs[x$tour, ]   # ordered rows, no return leg

  plot(
    locs$lng, locs$lat,
    pch  = 19, col  = "steelblue", cex  = 1.4,
    xlab = "Longitude", ylab = "Latitude",
    main = sprintf("TSP Route — %s (%.1f km)", x$algorithm, x$distance),
    ...
  )

  lines(route$lng, route$lat, col = "tomato", lwd = 2)

  text(
    locs$lng, locs$lat,
    labels = locs$name,
    pos = 3, cex = 0.8, col = "gray30"
  )

  points(
    locs$lng[x$tour[1]], locs$lat[x$tour[1]],
    pch = 15, col = "green3", cex = 1.8
  )

  invisible(x)
}
