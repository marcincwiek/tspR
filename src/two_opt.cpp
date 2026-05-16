#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// 2-opt local search improvement (C++)
// Internal — called by two_opt() in algorithms.R which validates inputs.
// The double nested loop is the TSP bottleneck; C++ gives ~50-100x speedup over R loops.
// [[Rcpp::export]]
IntegerVector two_opt_cpp(IntegerVector tour, NumericMatrix dist_mat) {
   int n = tour.size();

   // Work on a copy so we never modify the input vector
   IntegerVector best = clone(tour);

   bool improved = true;

   while (improved) {
     improved = false;

     for (int i = 0; i < n - 1; ++i) {
       for (int j = i + 2; j < n; ++j) {

         // Convert 1-indexed tour positions to 0-indexed matrix coordinates
         int a = best[i]           - 1;   // end of first edge
         int b = best[i + 1]       - 1;   // start of first edge
         int c = best[j]           - 1;   // end of second edge
         int d = best[(j + 1) % n] - 1;   // start of second edge
         //  (j+1) % n handles the wrap-around when j is the last city

         double current_cost = dist_mat(a, b) + dist_mat(c, d);
         double new_cost     = dist_mat(a, c) + dist_mat(b, d);

         // Accept only strict improvements; epsilon prevents infinite loops
         // caused by floating-point rounding near zero delta
         if (new_cost < current_cost - 1e-10) {
           // Reverse the segment from position i+1 to j (inclusive)
           // This is the defining operation of 2-opt
           std::reverse(best.begin() + i + 1,
                        best.begin() + j + 1);
           improved = true;
         }
       }
     }
   }

   return best;
 }
