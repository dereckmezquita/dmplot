#include <Rcpp.h>

//' Monte Carlo Simulation in C++
//'
//' This function performs a Monte Carlo simulation for price movements using C++.
//'
//' @param seed_price numeric. The starting price for the simulation.
//' @param daily_vol numeric. The daily volatility of the price movements.
//' @param num_sims integer. The number of simulation paths to generate.
//' @param num_days integer. The number of days to simulate for each path.
//'
//' @return A list containing two data frames:
//'   \item{simulations}{A data frame with columns 'close' (simulated prices) and 'simulation' (simulation index)}
//'   \item{end_prices}{A data frame with columns 'close' (final prices) and 'simulation' (simulation index)}
//'
//' @details
//' This function uses a geometric Brownian motion model to simulate price movements.
//' For each simulation path, it generates daily returns using a normal distribution
//' with mean 0 and standard deviation equal to the provided daily volatility.
//' The function is implemented in C++ for improved performance, especially for
//' large numbers of simulations or long time horizons.
//'
//' @examples
//' \dontrun{
//' results <- monte_carlo(seed_price = 100, daily_vol = 0.02, num_sims = 1000, num_days = 30)
//' head(results$simulations)
//' head(results$end_prices)
//' }
//'
//' @export
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
            // NOTE: this eliminates the price gap
            if (j == 0) {
                // For the first day, use the seed price directly
                close[row_index] = current_price;
            } else {
                // For subsequent days, apply the random walk
                current_price *= (1 + R::rnorm(0, daily_vol));
                close[row_index] = current_price;
            }
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
