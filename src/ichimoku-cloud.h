#ifndef DMPLOT_FIB_ICHIMOKU_CLOUD_H
#define DMPLOT_FIB_ICHIMOKU_CLOUD_H

Rcpp::List ichimoku_cloud(
    NumericVector high,
    NumericVector low,
    NumericVector close,
    int tenkan_period = 9,
    int kijun_period = 26,
    int senkou_period = 52
);

Rcpp::List ichimoku_cloud2(
    NumericVector high,
    NumericVector low,
    NumericVector close,
    int tenkan_period = 9,
    int kijun_period = 26,
    int senkou_period = 52
);

#endif
