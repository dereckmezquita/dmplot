#include <Rcpp.h>
#include <math.h>

#include "roc-rate-of-change.h"

using namespace Rcpp;

// https://bookdown.org/kochiuyu/technical-analysis-with-r-second-edition2/rate-of-change-roc.html
// discrete type
// ROC_t(K) = (price_t - price_(t-K)) / price_(t-K)
// continuous type - TTR uses continuous by default
// ROC_t(K) = log(P_t) - log(P_(t-K))

//' Rate of Change (ROC)
//'
//' @param price A numeric vector of prices
//' @param n The period for ROC calculation
//' @param type The type of ROC calculation: 'c' for continuous (default) or 'd' for discrete
//' @return A numeric vector containing the ROC values
//' @export
// [[Rcpp::export]]
std::vector<double> roc(std::vector<double> price, int n, char type = 'c') {
    std::vector<double> result(price.size(), NA_REAL);

    for (int i = n; i < price.size(); i++) {
        if (type == 'c') {
            result[i] = std::log(price[i]) - std::log(price[i - n]);
        } else {
            result[i] = (price[i] - price[i - n]) / price[i - n];
        }
    }

    return result;
}
