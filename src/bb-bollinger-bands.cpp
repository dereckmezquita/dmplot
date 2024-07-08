#include <Rcpp.h>
#include <math.h>

#include "bb-bollinger-bands.h"
#include "sma-simple-moving-average.h"

using namespace Rcpp;

// https://gallery.rcpp.org/articles/creating-a-datatable-in-rcpp/

// [[Rcpp::export]]
Rcpp::List bb(std::vector<double> price, int n, int sd = 2) {
    // calculate the simple moving average
    std::vector<double> mavg = sma(price, n);

    // pre-allocate std::vector with 0 values for the standard deviation
    std::vector<double> std_dev(price.size(), 0);

    // calculate the standard deviation
    for (int i = n - 1; i < price.size(); i++) {
        // population standard deviation is used
        // delta = sqrt(sum((x_i - mean) * (x_i - mean)) / n)
        double sum = 0;
        for (int j = i - n + 1; j <= i; j++) {
            sum += std::pow(price[j] - mavg[i], 2);
        }
        std_dev[i] = std::sqrt(sum / (double) n);
    }

    // calculate the upper and lower bands
    std::vector<double> upper_bound(price.size(), 0);
    std::vector<double> lower_bound(price.size(), 0);
    std::vector<double> pct(price.size(), 0);

    for (int i = 0; i < mavg.size(); i++) {
        upper_bound[i] = mavg[i] + sd * std_dev[i];
        lower_bound[i] = mavg[i] - sd * std_dev[i];
        pct[i] = (price[i] - lower_bound[i]) / (upper_bound[i] - lower_bound[i]);
    }

    List result = List::create(
        _["bb_lower"] = lower_bound,
        _["bb_mavg"] = mavg,
        _["bb_upper"] = upper_bound,
        _["bb_pct"] = pct
    );

    return result;
}
