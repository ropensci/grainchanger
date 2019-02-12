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
std::map<int, unsigned> get_class_index_map(const std::vector<int> &classes)
{
    std::map<int, unsigned> class_index;
    for (unsigned i = 0; i < classes.size(); i++) {
        class_index.insert(std::make_pair(classes[i], i));
    }
    return class_index;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
get_class_index_map(1:4)
*/
