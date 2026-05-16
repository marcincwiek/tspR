#include <Rcpp.h>
#include <vector>
#include <limits>
using namespace Rcpp;

// Nearest Neighbour TSP heuristic (C++)
// Internal — called by nn_heuristic() in algorithms.R which validates inputs.
// Do not call directly from R.
// [[Rcpp::export]]
IntegerVector nn_heuristic_cpp(NumericMatrix dist_mat, int start) {
   int n = dist_mat.nrow();

   // Track which cities have been visited
   std::vector<bool> visited(n, false);

   IntegerVector tour(n);

   // Convert R's 1-based index to C++'s 0-based
   int current     = start - 1;
   visited[current] = true;
   tour[0]          = current + 1;   // store back as 1-indexed for R

   for (int step = 1; step < n; ++step) {
     double best_dist = std::numeric_limits<double>::infinity();
     int    best_next = -1;

     // Scan every unvisited city — this inner loop is why we use C++.
     // In R, a for-loop over n cities inside another for-loop over n steps
     // pays interpreter overhead n^2 times. Here it is pure machine code.
     for (int j = 0; j < n; ++j) {
       if (!visited[j] && dist_mat(current, j) < best_dist) {
         best_dist = dist_mat(current, j);
         best_next = j;
       }
     }

     visited[best_next] = true;
     current            = best_next;
     tour[step]         = current + 1;   // back to 1-indexed
   }

   return tour;
 }
