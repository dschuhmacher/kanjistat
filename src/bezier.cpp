#include <Rcpp.h>   


using namespace Rcpp;

/* Point on a cubic Bézier curve. Numerical stability could be improved with a Horner scheme.
 * The scaling factor one hundreth is due to the fact that the kvec SVG commands assume a range
 * between 0 and 100, while the kvec point clouds are normalised between 0 and 1, which we reproduce here. */
// [[Rcpp::export]]
NumericVector cubic_bezier_point_cpp (double t, NumericVector p0, NumericVector p1, NumericVector p2, NumericVector p3) {
  return pow(1 - t, 3) * p0 + 3 * pow(1 - t, 2) * t * p1 + 3 * (1 - t) * pow(t, 2) * p2 + pow(t, 3) * p3;
}

// [[Rcpp::export]]
NumericMatrix cubic_bezier_curve_cpp (NumericVector t, NumericVector p0, NumericVector p1, NumericVector p2, NumericVector p3) {
  int n = t.length();
  NumericMatrix res(n,2); // (much) faster than appending stuff
  for (int i = 0; i < n; i++)
    res(i,_) = pow(1 - t[i], 3) * p0 + 3 * pow(1 - t[i], 2) * t[i] * p1 + 3 * (1 - t[i]) * pow(t[i], 2) * p2 + pow(t[i], 3) * p3;
  return res;
}

/* Of course the above could be more intuitively written via matrix multiplication. However, there is
 * no such functionality in standard Rcpp. If we need linear algebra stuff more than once we should
 * (also) use RcppArmadillo, which allows for intuitive linear algebra syntax in C++; see
 * https://cran.r-project.org/web/packages/RcppArmadillo/vignettes/RcppArmadillo-intro.pdf
 */

