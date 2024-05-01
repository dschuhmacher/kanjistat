#include <Rcpp.h>

using namespace Rcpp;

/* From the original kanjidist.
 * Of course we hardly need C++ here, but for more complicated functions to come
 * C++ is very likely to speed things up */
// match_diagonal_trafo becomes we are finding an affine transform with 
// diagonal matrix for the linear part
// [[Rcpp::export]]
List match_diagonal_trafo (NumericMatrix points1, NumericMatrix points2) {
  /* location and scale based on min/max dimension of stroke info is extreme but
   * not considerably worse than anything else and much more efficient than 
   * creating a bitmap first points of all strokes in a single matrix */
  double min_1x = min(points1(_,0));
  double max_1x = max(points1(_,0));
  double min_1y = min(points1(_,1));
  double max_1y = max(points1(_,1));
  double min_2x = min(points2(_,0));
  double max_2x = max(points2(_,0));
  double min_2y = min(points2(_,1));
  double max_2y = max(points2(_,1));
  
  double cen_1x = (min_1x + max_1x) / 2.0;
  double cen_1y = (min_1y + max_1y) / 2.0;
  double cen_2x = (min_2x + max_2x) / 2.0;
  double cen_2y = (min_2y + max_2y) / 2.0;

  double sca_1x = (max_1x - min_1x);
  double sca_1y = (max_1y - min_1y);
  double sca_2x = (max_2x - min_2x);
  double sca_2y = (max_2y - min_2y);
  
  double meansca_x = sqrt(sca_1x*sca_2x);
  double meansca_y = sqrt(sca_1y*sca_2y);
  /* The geometric mean is nicer here because going from ele1 to ele2 (the elements given
   * by which) we can multiply twice by sqrt(sca2/sca1) and arrive after the first multipl. at the mean
   * and after the second multipl. at ele2. In contrast to, say, the arithmetic mean this operation
   * is consistent with the natural penalty of sca2/sca1 we return below. */

  /* the `fact` tell us how to scale (coordinatewise) in order to have the 
   * (geometric) mean scale *and* then (by division with upfact)
   * everything blown up such that the larger sidelength is 1 */
  /* this is one way of doing it, meaning if the aspect ratio is not "the same
   * way round" for both kanji we do not make the kanji the same aspect ratio in the end
   * the other way would be instead of fact1, fact2 below to set both directions to size 1 */
  double upfact = std::max(meansca_x, meansca_y);
  double fact_1x = sqrt(sca_2x/sca_1x)/upfact;
  double fact_1y = sqrt(sca_2y/sca_1y)/upfact;
  double fact_2x = sqrt(sca_1x/sca_2x)/upfact;
  double fact_2y = sqrt(sca_1y/sca_2y)/upfact;

  NumericVector cen1 = {cen_1x, cen_1y};
  NumericVector cen2 = {cen_2x, cen_2y};
  NumericVector sca1 = {sca_1x, sca_1y};
  NumericVector sca2 = {sca_2x, sca_2y};
  NumericVector fact1 = {fact_1x, fact_1y};
  NumericVector fact2 = {fact_2x, fact_2y};
  List res = List::create(_("cen1") = cen1 , _["cen2"] = cen2,
                          _("sca1") = sca1 , _["sca2"] = sca2,
                          _("fact1") = fact1 , _["fact2"] = fact2);
  return res;
}


