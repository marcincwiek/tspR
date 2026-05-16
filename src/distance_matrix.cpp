#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Haversine distance matrix (C++)
// Internal — used for benchmarking against the vectorised R version in distance.R.
// [[Rcpp::export]]
NumericMatrix haversine_matrix_cpp(NumericVector lat, NumericVector lng) {
   int    n = lat.size();
   double R = 6371.0;   // Earth mean radius in km

   NumericMatrix dist_mat(n, n);

   // Pre-convert all coordinates to radians once — avoids repeating the
   // conversion inside the inner loop (a minor but real optimisation)
   NumericVector lat_r(n), lng_r(n);
   for (int i = 0; i < n; ++i) {
     lat_r[i] = lat[i] * M_PI / 180.0;
     lng_r[i] = lng[i] * M_PI / 180.0;
   }

   // Only compute the upper triangle; copy to lower triangle.
   // This halves the number of haversine evaluations.
   for (int i = 0; i < n; ++i) {
     dist_mat(i, i) = 0.0;

     for (int j = i + 1; j < n; ++j) {
       double dlat = lat_r[j] - lat_r[i];
       double dlng = lng_r[j] - lng_r[i];

       double a = std::sin(dlat / 2.0) * std::sin(dlat / 2.0)
         + std::cos(lat_r[i])   * std::cos(lat_r[j])
         * std::sin(dlng / 2.0) * std::sin(dlng / 2.0);

         double d = 2.0 * R * std::asin(std::min(1.0, std::sqrt(a)));

         dist_mat(i, j) = d;
         dist_mat(j, i) = d;   // matrix is symmetric
     }
   }

   return dist_mat;
 }
