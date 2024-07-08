#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

//' Fibonacci Retracement Levels
//' Calculate the Fibonacci retracement levels for a given price range
//' @param high The high price
//' @param low The low price
//' @return A list containing the retracement levels and prices
//' @export
// [[Rcpp::export]]
Rcpp::List fib(double high, double low) {
    std::vector<double> levels = {0.0, 0.236, 0.382, 0.5, 0.618, 0.786, 1.0};
    std::vector<double> retracement_levels(levels.size());
    
    double price_range = high - low;
    
    for (size_t i = 0; i < levels.size(); ++i) {
        retracement_levels[i] = high - (price_range * levels[i]);
    }
    
    return List::create(
        _["levels"] = levels,
        _["prices"] = retracement_levels
    );
}
