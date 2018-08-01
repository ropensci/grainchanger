#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
IntegerVector freq_c(const IntegerVector& dat, const IntegerVector& lc_class) {
  std::size_t num_lc = lc_class.size();
  IntegerVector counts(num_lc);
  std::size_t n = dat.size();
  for (std::size_t i = 0; i < n; i++) {
    for (std::size_t j = 0; j < num_lc; j++) {
      if (dat[i] == lc_class[j])
        counts[j]++;
    }
  }
  return counts;
}

