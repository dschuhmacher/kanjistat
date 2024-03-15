#include <Rcpp.h>   
  
using namespace Rcpp;

/* Point on a cubic Bézier curve. Numerical stability could be improved with a Horner scheme.
 * The scaling factor one hundreth is due to the fact that the kvec SVG commands assume a range
 * between 0 and 100, while the kvec point clouds are normalised between 0 and 1, which we reproduce here. */
// [[Rcpp::export]]
NumericVector cubic_bezier_point_cpp (float t, NumericVector p0, NumericVector p1, NumericVector p2, NumericVector p3) {
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

NumericVector cubic_bezier_arc_lengths(int n, NumericVector p0, NumericVector p1, NumericVector p2, NumericVector p3) {
  NumericMatrix der(n,2); 
  
  for (int i = 0; i < n; i++) {
    float t = i/n;
    der(i,_) = (3 * pow(1 - t, 2) * (p1 - p0) + 6 * (1 - t) * t * (p2 - p1) + 3 * pow(t, 2) * (p3 - p2))/100;
  }
    
  
  // A Riemann Sum over the speed values. We might want to try Simpson's rule?
  NumericVector lengths(n);
  lengths(0) = 0;
  float length = 0;
  for (int i = 1; i < n; i++) {
    length += pow(pow(der(i, 0), 2) + pow(der(i, 1), 2), 0.5);
    lengths(i) = length;
  }
  
  return lengths;
}

NumericVector cubic_bezier_arc_lengths2(int n, NumericVector p0, NumericVector p1, NumericVector p2, NumericVector p3) {
  NumericMatrix points(n,2); 
  
  for (float i = 0.; i < n; i++) {
    float t = i/(n-1);
    points(i,_) = (pow(1 - t, 3) * p0 + 3 * pow(1 - t, 2) * t * p1 + 3 * (1 - t) * pow(t, 2) * p2 + pow(t, 3) * p3)/100;
  }
  
  NumericVector lengths(n);
  lengths(0) = 0;
  float length = 0;
  for (int i = 1; i < n; i++) {
    length += pow(pow(points(i, 0) - points(i-1, 0), 2) + pow(points(i, 1) - points(i-1, 1), 2), 0.5);
    lengths(i) = length;
  }
  
  return lengths;
}

// Adapted from https://gamedev.stackexchange.com/questions/5373/moving-ships-between-two-planets-along-a-bezier-missing-some-equations-for-acce/5427#5427
// [[Rcpp::export]]
NumericMatrix cubic_bezier_spaced_curve_cpp (int num_points, NumericVector p0, NumericVector p1, NumericVector p2, NumericVector p3) {
  int n = 50;
  
  NumericVector lengths = cubic_bezier_arc_lengths2(n, p0, p1, p2, p3);
  float total_length = lengths[n-1];
  NumericMatrix out(num_points,2);
  for (float i = 0.; i < num_points; i++) {
    float target_length = total_length * i / (num_points-1);
    int low = 0;
    int high = n;
    int k = 0;
    float t = 0;
    // Next, a binary search to determine closest indices to desired length:
    while (low < high) {
      k = low + ((high - low) / 2);
      if (lengths[k] < target_length) {
        low = k + 1;
      } else {
        high = k;
      }
    }
    if (lengths[k] > target_length) {
      k--;
    }

    float length_before = lengths[k];
    if (length_before == target_length) {
      t = k / n;
    } else { // We interpolate linearly between the two closest points.
      t = (k + (target_length - length_before) / (lengths[k + 1] - length_before)) / n;
    }
    
    out(i,_) = (pow(1 - t, 3) * p0 + 3 * pow(1 - t, 2) * t * p1 + 3 * (1 - t) * pow(t, 2) * p2 + pow(t, 3) * p3)/100;
  }
  
  return out;
}

/* Of course the above could be more intuitively written via matrix multiplication. However, there is
 * no such functionality in standard Rcpp. If we need linear algebra stuff more than once we should
 * (also) use RcppArmadillo, which allows for intuitive linear algebra syntax in C++; see
 * https://cran.r-project.org/web/packages/RcppArmadillo/vignettes/RcppArmadillo-intro.pdf
 */

