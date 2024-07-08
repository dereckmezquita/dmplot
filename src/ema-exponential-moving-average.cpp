#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// conclusion ema_rcpp is faster

// algorithm inspired from:
// https://bookdown.org/kochiuyu/technical-analysis-with-r-second-edition2/exponential-moving-average-ema.html
// https://github.com/joshuaulrich/TTR/blob/master/src/moving_averages.c


// [[Rcpp::export]]
std::vector<double> ema(std::vector<double> price, int n, bool wilder = false) {
    // define beta
    // for EMA, wilder=FALSE (the default) uses an exponential smoothing ratio of 2/(n+1), while wilder=TRUE uses Welles Wilder's exponential smoothing ratio of 1/n
    double beta = wilder ? 1.0 / n : 2.0 / ((double) n + 1.0);

    // pre-allocate the vector with NA values
    std::vector<double> result(price.size(), NA_REAL);

    // check for non-leading NAs and get first non-NA location
    int first_non_na = 0;
    for (int i = 0; i < price.size(); i++) {
        if (!std::isnan(price[i])) {
            first_non_na = i;
            break;
        }
    }

    // if first value larger than n then throw error
    if (n + first_non_na > price.size()) {
        stop("Not enough non-NA values");
    }

    // calculate the first value as the average of the first n values
    double seed = 0.0;
    for (int i = first_non_na; i < first_non_na + n; i++) {
        // std::cout << price[i] << std::endl;
        seed += price[i] / (double) n;
    }

    result[first_non_na + n - 1] = seed;

    // calculate the ema
    for (int i = first_non_na + n; i < price.size(); i++) {
        result[i] = beta * price[i] + (1.0 - beta) * result[i - 1];
    }
    
    return result;
}