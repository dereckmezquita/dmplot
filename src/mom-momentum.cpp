#include <Rcpp.h>

#include "mom-momentum.h"

using namespace Rcpp;

// https://bookdown.org/kochiuyu/technical-analysis-with-r-second-edition2/momentum.html
// M_t(n) = P_t - P_(t-n)

// [[Rcpp::export]]
std::vector<double> mom(std::vector<double> price, int n) {
    std::vector<double> result(price.size(), NA_REAL);

    for (int i = n; i < price.size(); i++) {
        result[i] = price[i] - price[i - n];
    }

    return result;
}
