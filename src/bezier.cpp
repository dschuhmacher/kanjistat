#include <Rcpp.h>   
  
using namespace Rcpp;

// Point on a cubic Bézier curve.
// [[Rcpp::export]]
NumericVector cubic_bezier_point_cpp (double t, NumericVector p0, NumericVector p1, NumericVector p2, NumericVector p3) {
  return p0 + t*(-3*p0 + 3*p1 + t*(3*p0 - 6*p1 + 3*p2 + t*(-p0 + 3*p1 - 3*p2 + p3)));
}

// [[Rcpp::export]]
NumericMatrix cubic_bezier_curve_cpp (NumericVector t, NumericVector p0, NumericVector p1, NumericVector p2, NumericVector p3) {
  int n = t.length();
  NumericMatrix res(n,2); // (much) faster than appending stuff
  
  for (int i = 0; i < n; i++) // The Horner scheme for pow(1 - t[i], 3) * p0 + 3 * pow(1 - t[i], 2) * t[i] * p1 + 3 * (1 - t[i]) * pow(t[i], 2) * p2 + pow(t[i], 3) * p3;
    res(i,_) = p0 + t[i]*(-3*p0 + 3*p1 + t[i]*(3*p0 - 6*p1 + 3*p2 + t[i]*(-p0 + 3*p1 - 3*p2 + p3)));
  
  return res;
}

// Calculating a running vector with length n of a cubic Bézier curve at
// uniformly sampled time points.
NumericVector cubic_bezier_arc_lengths(int n, NumericVector p0, NumericVector p1, NumericVector p2, NumericVector p3) {
  NumericMatrix points(n,2); 
  
  double nf1 = static_cast<double>(n-1);
  for (int i = 0; i < n; i++) {  
    double t = i/nf1; 
      // DS: this is (to my own surprise) even 10-15% faster than the old version with the float loop and appears safer
      // neither doing -1 before the loop nor changing from float to double has any measurable
      // impact, even for n=200000 (also NumericVectors store doubles anyway AFAIK)
    points(i,_) = p0 + t*(-3*p0 + 3*p1 + t*(3*p0 - 6*p1 + 3*p2 + t*(-p0 + 3*p1 - 3*p2 + p3)));
  }
  
  NumericVector lengths(n);
  lengths(0) = 0;
  double length = 0;
  for (int i = 1; i < n; i++) {
    length += pow(pow(points(i, 0) - points(i-1, 0), 2) + pow(points(i, 1) - points(i-1, 1), 2), 0.5);
    lengths(i) = length;
  }
  
  return lengths;
}

// Adapted from https://gamedev.stackexchange.com/questions/5373/moving-ships-between-two-planets-along-a-bezier-missing-some-equations-for-acce/5427#5427
// density represents a spatial point density. I assume we do not need to expose
// a way to get an exact number of points instead? In any case, the length of
// the curve is only known inside this function, so for now we implement it like
// this.
// n is the number of samples used for binary search and interpolation.
// it seems n is typically even smaller than num_points, so the binary search within a loop is a bit questionable
// [[Rcpp::export]]
NumericMatrix cubic_bezier_curve_eqspaced_cpp (double density, int n, NumericVector p0, NumericVector p1, NumericVector p2, NumericVector p3) {
  NumericVector lengths = cubic_bezier_arc_lengths(n, p0, p1, p2, p3);
  double total_length = lengths[n-1];
  // Rcout << "l0 " << lengths[0] << "\n";
  // Rcout << "l1 " << lengths[1] << "\n";
  // Rcout << total_length << "\n";
  
  int num_points = 1 + R::fround(density * total_length, 0);
  // int num_points = 2 + static_cast<int>(density * total_length);
  // Rcout << num_points << "\n";
  // the algo below makes most sense if n >> num_points, whereas we have
  // n \approx num_points at best: I guess linear search for all points should be faster(?)
  NumericMatrix out(num_points,2);
  double num_points_f1 = static_cast<double>(num_points-1);
  for (int i = 0; i < num_points; i++) {
    double target_length = total_length * i / num_points_f1;
    // Rcout << "t" << target_length << " ";
    int low = 0;
    int high = n-1;
    int k = 0;
    double t = 0;
    // Next, a binary search to determine closest indices to desired length:
    while (low + 1 < high) {
      k = (low + high) / 2;
      // k = low + ((high - low) / 2); // was the same  (truncation considered)
      if (lengths[k] <= target_length) {
        low = k;
      } else {
        high = k;
      }
    }
    // low = index of largest of the original sample point smaller-equal to i-th (current) eqspaced point 
    // Rcout << low << "+++" << lengths[low] << " ";

    double length_before = lengths[low];
    t = (low + (target_length - length_before) / (lengths[low + 1] - length_before)) / (n-1);
    // Rcout << t << "  ";
    
    out(i,_) = p0 + t*(-3*p0 + 3*p1 + t*(3*p0 - 6*p1 + 3*p2 + t*(-p0 + 3*p1 - 3*p2 + p3)));
  }
  
  return out;
}


// part of the alternative pipeline
// [[Rcpp::export]]
NumericMatrix bezier_curve_cpp(NumericMatrix beziermat, int ncurves, double point_density, bool eqspaced) {
  NumericMatrix curve_points;
  NumericVector stroke_points = beziermat(_,0);  // we make a matrix out of it in the end (Matrix operations are insufficient)
  int npoints_tot = 1;
  for (int i = 0; i < ncurves; i++) {
    NumericVector p0 = beziermat(_,3*i);
    NumericVector p1 = beziermat(_,3*i+1);
    NumericVector p2 = beziermat(_,3*i+2);
    NumericVector p3 = beziermat(_,3*i+3);
    if (eqspaced) {
      // Rcout << point_density << " " << p0 << " " << p1 << " " << p2 << " " << p3 << std::endl;
      curve_points = transpose(cubic_bezier_curve_eqspaced_cpp(point_density, 25, p0, p1, p2, p3));
      /* 25 "check points" is fully sufficient (so let's keep this hard wired):
       * when point_density is just around 20-30, the
       * computed theoretical distance between different strokes will vary a lot due to rounding
       * (so errors in no of check point is not seen), when point_density is high (e.g. 100),
       * the distances turn pretty perfect even with only 25 check points. */
    } else {
      NumericVector d = p3 - p0;
      double approxlen = sqrt(d[1]*d[1] + d[2]*d[2]);
      int numpoints = 2 + R::fround(point_density * approxlen, 0);  //  to match bezier.cpp l.83, but with 2+ instead
      /* of 1+ to slightly help the short very curved lines, where we get the length
       * very wrong with our approximate method (creates slight inefficiency for 
       * short but more or less straight lines) */
      NumericVector t(numpoints);
      for (int j = 0; j < numpoints; j++) {
        t[j] = j/(numpoints-1);
      }
      curve_points = transpose(cubic_bezier_curve_cpp(t, p0, p1, p2, p3));
    }
    for (NumericVector::iterator it = curve_points.begin()+2; it < curve_points.end(); ++it) {
      stroke_points.push_back(*it);
    }
    npoints_tot += curve_points.ncol() - 1;
  }
  
  stroke_points.attr("dim") = Dimension(2, npoints_tot);
  return transpose(as<NumericMatrix>(stroke_points));
}


// [[Rcpp::export]]
NumericVector ftestloop(int n) {
  NumericVector vec(n);
  for (float i = 0.; i < n; i++) {
    vec[i] = i/(n-1);
  }
  return vec;
}

// [[Rcpp::export]]
NumericVector itestloop(int n) {
  NumericVector vec(n);
  float nf = static_cast<float>(n);
  for (int i = 0; i < n; i++) {
    vec[i] = i/(nf-1);
  }
  return vec;
}

// [[Rcpp::export]]
NumericVector idtestloop(int n) {
  NumericVector vec(n);
  double nf1 = static_cast<double>(n-1);
  for (int i = 0; i < n; i++) {
    vec[i] = i/nf1;
  }
  return vec;
}



  