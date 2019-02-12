#include <RcppArmadillo.h>
#include "rcpp_focal_stats.h"
#include "rcpp_create_neighborhood.h"
#include "get_class_index_map.h"
#include "rcpp_get_unique_values.h"

// [[Rcpp::export]]
NumericMatrix rcpp_focal_stats(const IntegerMatrix x,
                               const arma::imat directions,
                               const std::vector<int> classes) {
    const int na = NA_INTEGER;
    const unsigned ncols = x.ncol();
    const unsigned nrows = x.nrow();

    //std::vector<int> classes = rcpp_get_unique_values(x);
    std::map<int, unsigned> class_index = get_class_index_map(classes);

    unsigned n_classes = class_index.size();

    //create neighbors coordinates
    IntegerMatrix tmp = rcpp_create_neighborhood(directions);
    int neigh_len = tmp.nrow();
    std::vector<std::vector<int> > neig_coords;
    for (int row = 0; row < neigh_len; row++) {
        IntegerVector a = tmp.row(row);
        std::vector<int> b(a.begin(), a.end());
        neig_coords.push_back(b);
    }

    NumericMatrix out(nrows, ncols);
    
    for (unsigned col = 0; col < ncols; col++) {
        for (unsigned row = 0; row < nrows; row++) {
            std::vector<unsigned> class_count(n_classes);
            const int tmp = x[col * nrows + row];
            if (tmp == na)
                continue;
            unsigned get_class = class_index[tmp];
            class_count[get_class]++;
            for (int h = 0; h < neigh_len; h++) {
                int neig_col = neig_coords[h][0] + col;
                int neig_row = neig_coords[h][1] + row;
                if (neig_col >= 0 &&
                        neig_row >= 0 &&
                        neig_col < ncols &&
                        neig_row < nrows) {
                    const int tmp = x[neig_col * nrows + neig_row];
                    if (tmp == na)
                        continue;
                    unsigned get_class = class_index[tmp];
                    class_count[get_class]++;
                }
            }
            
            // this just does Shannon evenness for now, but we can build in others
            double H = 0;
            for (unsigned cl = 0; cl < n_classes; cl++) {
              double p = static_cast<double>(class_count[cl]) / static_cast<double>(neigh_len);
              if (p > 0) {
                H += -p * log(p);  
              } else {
                H += 0;
              }
            }
            
            out(row, col) = H / log(n_classes);
        }
    }

    return out;
}

/*** R

library(raster)
library(dplyr)
test <- landscapemetrics::augusta_nlcd
mat <- raster::as.matrix(test)
nb <- matrix(1, 3, 3)
nb[2, 2] <- 0
mat_test <- rcpp_focal_stats(mat, nb, rcpp_get_unique_values(mat))
*/
