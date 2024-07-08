#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List monte_carlo(double seed_price, double daily_vol, int num_sims, int num_days) {
    int total_rows = num_sims * num_days;
    Rcpp::NumericVector close(total_rows);
    Rcpp::IntegerVector sim_idx(total_rows);
    Rcpp::NumericVector end_price(num_sims);
    Rcpp::IntegerVector end_idx(num_sims);
    
    int row_index = 0;
    for (int i = 0; i < num_sims; ++i) {
        double current_price = seed_price;
        for (int j = 0; j < num_days; ++j) {
            current_price *= (1 + R::rnorm(0, daily_vol));
            close[row_index] = current_price;
            sim_idx[row_index] = i + 1;
            ++row_index;
        }

        end_price[i] = current_price;
        end_idx[i] = i + 1;
    }

    Rcpp::DataFrame sim_df = Rcpp::DataFrame::create(
        Rcpp::_["close"] = close,
        Rcpp::_["simulation"] = sim_idx
    );

    Rcpp::DataFrame end_df = Rcpp::DataFrame::create(
        Rcpp::_["close"] = end_price,
        Rcpp::_["simulation"] = end_idx
    );

    return Rcpp::List::create(
        Rcpp::_["simulations"] = sim_df,
        Rcpp::_["end_prices"] = end_df
    );
}