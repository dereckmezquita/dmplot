#ifndef DMPLOT_MACD_MOVING_AVERAGE_CONVERGENCE_DIVERGENCE_H
#define DMPLOT_MACD_MOVING_AVERAGE_CONVERGENCE_DIVERGENCE_H

Rcpp::List macd(std::vector<double> price, int s, int l, int k, bool percent);

#endif
