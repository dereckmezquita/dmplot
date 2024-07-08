#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

//' Simple Moving Average (SMA)
//'
//' @param price A numeric vector of prices
//' @param n The period for SMA calculation
//' @return A numeric vector containing the SMA values
//' @export
// [[Rcpp::export]]
std::vector<double> sma(std::vector<double> price, int n) {
    // pre-allocate the vector with NA values
    std::vector<double> result(price.size(), NA_REAL);

    // calculate the first value as the average of the first n values
    double first_val = 0;
    for (int i = 0; i < n; i++) {
        first_val += price[i] / (double) n;
    }

    // proof dividing in the for loop is correct
    // 1+2+3+4+5+6+7+8+9+10 = 55 / 10 = 5.5
    // (1/10)+(2/10)+(3/10)+(4/10)+(5/10)+(6/10)+(7/10)+(8/10)+(9/10)+(10/10) = 5.5
    // first_val /= (double) n;

    result[n - 1] = first_val;

    // iterate over every position of the result array
    // each are calculated from all values in window of size n
    for (int i = n; i <= price.size(); i++) {
        // iterate over the window of size n and calculate the sum / n
        // values are initially set to NA so we must do initial value at 0
        double sum = 0;
        for (int j = i - n; j < i; j++) {
            sum += price[j];
        }
        result[i - 1] = sum / (double) n;
    }
    
    // cast to NumericVector
    return result;
}