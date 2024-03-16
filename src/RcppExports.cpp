// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cubic_bezier_point_cpp
NumericVector cubic_bezier_point_cpp(float t, NumericVector p0, NumericVector p1, NumericVector p2, NumericVector p3);
RcppExport SEXP _kanjistat_cubic_bezier_point_cpp(SEXP tSEXP, SEXP p0SEXP, SEXP p1SEXP, SEXP p2SEXP, SEXP p3SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< float >::type t(tSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p0(p0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p1(p1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p2(p2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p3(p3SEXP);
    rcpp_result_gen = Rcpp::wrap(cubic_bezier_point_cpp(t, p0, p1, p2, p3));
    return rcpp_result_gen;
END_RCPP
}
// cubic_bezier_curve_cpp
NumericMatrix cubic_bezier_curve_cpp(NumericVector t, NumericVector p0, NumericVector p1, NumericVector p2, NumericVector p3);
RcppExport SEXP _kanjistat_cubic_bezier_curve_cpp(SEXP tSEXP, SEXP p0SEXP, SEXP p1SEXP, SEXP p2SEXP, SEXP p3SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type t(tSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p0(p0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p1(p1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p2(p2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p3(p3SEXP);
    rcpp_result_gen = Rcpp::wrap(cubic_bezier_curve_cpp(t, p0, p1, p2, p3));
    return rcpp_result_gen;
END_RCPP
}
// cubic_bezier_curve_eqspaced_cpp
NumericMatrix cubic_bezier_curve_eqspaced_cpp(float density, int n, NumericVector p0, NumericVector p1, NumericVector p2, NumericVector p3);
RcppExport SEXP _kanjistat_cubic_bezier_curve_eqspaced_cpp(SEXP densitySEXP, SEXP nSEXP, SEXP p0SEXP, SEXP p1SEXP, SEXP p2SEXP, SEXP p3SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< float >::type density(densitySEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p0(p0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p1(p1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p2(p2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type p3(p3SEXP);
    rcpp_result_gen = Rcpp::wrap(cubic_bezier_curve_eqspaced_cpp(density, n, p0, p1, p2, p3));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_kanjistat_cubic_bezier_point_cpp", (DL_FUNC) &_kanjistat_cubic_bezier_point_cpp, 5},
    {"_kanjistat_cubic_bezier_curve_cpp", (DL_FUNC) &_kanjistat_cubic_bezier_curve_cpp, 5},
    {"_kanjistat_cubic_bezier_curve_eqspaced_cpp", (DL_FUNC) &_kanjistat_cubic_bezier_curve_eqspaced_cpp, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_kanjistat(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
