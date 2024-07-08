#include <Rcpp.h>
#include <iostream>

#include "rsi-relative-strength-index.h"
#include "ema-exponential-moving-average.h"
#include "sma-simple-moving-average.h"

using namespace Rcpp;

// [[Rcpp::export]]
std::vector<double> rsi(std::vector<double> price, int n, char method = 'e') {
    int price_length = price.size();
    // create result vectors
    std::vector<double> up(price_length, 0.0);
    std::vector<double> down(price_length, 0.0);

    for (int i = 1; i < price_length; i++) {
        double price_diff = price[i] - price[i - 1];
        if (price_diff > 0) {
            up[i] = price[i] - price[i - 1];
        } else {
            down[i] = price[i - 1] - price[i];
        }
    }

    // smoothed averages
    std::vector<double> smoothed_average_gain(price_length, NA_REAL);
    std::vector<double> smoothed_average_loss(price_length, NA_REAL);

    if (method == 'e') {
        smoothed_average_gain = ema(up, n, true);
        smoothed_average_loss = ema(down, n, true);
    } else if (method == 's') {
        smoothed_average_gain = sma(up, n);
        smoothed_average_loss = sma(down, n);
    } else {
        // throw c++ error
        throw std::invalid_argument("method must be 'e' or 's'");
    }

    // calculate the relative strength
    std::vector<double> result(price_length, NA_REAL);

    for (int i = 0; i < price_length; i++) {
        double relative_strength_value = smoothed_average_gain[i] / smoothed_average_loss[i];
        result[i] = 100.0 - 100.0 / (1.0 + relative_strength_value);
    }

    return result;
}
