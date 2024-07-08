#include <Rcpp.h>
#include <vector>

#include "macd-moving-average-convergence-divergence.h"
#include "ema-exponential-moving-average.h"

using namespace Rcpp;

// [[Rcpp::export]]
List macd(std::vector<double> price, int s, int l, int k, bool percent = true) {
    std::vector<double> mavg_fast = ema(price, s);
    std::vector<double> mavg_slow = ema(price, l);

    // calculate the macd as the difference between mavg_fast and mavg_slow
    std::vector<double> macd_res;

    // we use a for loop here
    for (int i = 0; i < mavg_fast.size(); i++) {
        if (percent) {
            macd_res.push_back(100 * (mavg_fast[i] / mavg_slow[i] - 1));
        } else {
            macd_res.push_back(mavg_fast[i] - mavg_slow[i]);
        }
    }

    std::vector<double> signal = ema(macd_res, k);

    List result = List::create(_["macd"] = macd_res, _["signal"] = signal);

    return result;
}
