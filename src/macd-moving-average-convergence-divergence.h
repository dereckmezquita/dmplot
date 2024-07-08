#ifndef KUCOIN_MACD_MOVING_AVERAGE_CONVERGENCE_DIVERGENCE_H
#define KUCOIN_MACD_MOVING_AVERAGE_CONVERGENCE_DIVERGENCE_H

Rcpp::List macd(std::vector<double> price, int s, int l, int k, bool percent);

#endif