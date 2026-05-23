#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// 2-opt local search improvement
// [[Rcpp::export]]
IntegerVector two_opt_cpp(IntegerVector tour, NumericMatrix dist_mat) {
  int n = tour.size();
  // deep copy so we don't reference the same memory point
  IntegerVector best = clone(tour);
  bool improved = true;

  // Loop over till there are no improvements
  while (improved) {
    improved = false;

    // every possible pair of non-adjacent edges (edges that do not share a common point)
    for (int i = 0; i < n - 1; ++i) {
      for (int j = i + 2; j < n; ++j) {
        int a = best[i]           - 1;
        int b = best[i + 1]       - 1;
        int c = best[j]           - 1;
        int d = best[(j + 1) % n] - 1;

        double current_cost = dist_mat(a, b) + dist_mat(c, d);
        double new_cost     = dist_mat(a, c) + dist_mat(b, d);

        // fix floating point error of accumulating tiny rounding errors. - make sure the loop terminates
        if (new_cost < current_cost - 1e-10) {
          std::reverse(best.begin() + i + 1,
                       best.begin() + j + 1);
          improved = true;
        }
      }
    }
  }

  return best;
}
