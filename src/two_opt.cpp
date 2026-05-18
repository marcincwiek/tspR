#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// 2-opt local search improvement (C++)
// [[Rcpp::export]]
IntegerVector two_opt_cpp(IntegerVector tour, NumericMatrix dist_mat) {
  int n = tour.size();
  IntegerVector best = clone(tour);
  bool improved = true;

  while (improved) {
    improved = false;

    for (int i = 0; i < n - 1; ++i) {
      for (int j = i + 2; j < n - 1; ++j) {  // n-1 avoids wrap-around
        int a = best[i]     - 1;
        int b = best[i + 1] - 1;
        int c = best[j]     - 1;
        int d = best[j + 1] - 1;

        double current_cost = dist_mat(a, b) + dist_mat(c, d);
        double new_cost     = dist_mat(a, c) + dist_mat(b, d);

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
