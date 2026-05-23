#include <Rcpp.h>
#include <vector>
#include <limits>
using namespace Rcpp;

// Nearest Neighbour TSP
// [[Rcpp::export]]
IntegerVector nn_heuristic_cpp(NumericMatrix dist_mat, int start) {
  // Number of rows in distance matrix = number of cities
   int n = dist_mat.nrow();

   // Tracks visited cities
   std::vector<bool> visited(n, false);

   // Final vector
   IntegerVector tour(n);

   // Convert 1-based index to C++'s 0-based
   int current     = start - 1;
   visited[current] = true;
   tour[0]          = current + 1;

   for (int step = 1; step < n; ++step) {
     double best_dist = std::numeric_limits<double>::infinity();
     int    best_next = -1;

      // Look at the distance between current location every unvisited one -
      // find the closest one
     // O(n) = n^2
     for (int j = 0; j < n; ++j) {
       if (!visited[j] && dist_mat(current, j) < best_dist) {
         best_dist = dist_mat(current, j);
         best_next = j;
       }
     }

     // mark city as visited, move there, save the path
     visited[best_next] = true;
     current            = best_next;
     tour[step]         = current + 1;
   }

   return tour;
 }
