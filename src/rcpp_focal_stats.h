#ifndef FOCAL_STATS_H
#define FOCAL_STATS_H
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

IntegerMatrix rcpp_focal_stats(const IntegerMatrix x,
                               const arma::imat directions,
                               const IntegerVector classes);

std::map<int, unsigned> get_class_index_map(const std::vector<int> &classes);

#endif // FOCAL_STATS_H
