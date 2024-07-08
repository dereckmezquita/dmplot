box::use(dmplot[ bb, rsi, macd, monte_carlo ])

# Generate some sample data
set.seed(123)
price_data <- cumsum(rnorm(100)) + 100

# Test Bollinger Bands
bb_result <- bb(price_data, n = 20)
tail(bb_result)

# Test RSI
rsi_result <- rsi(price_data, n = 14)
tail(rsi_result)

# Test MACD
macd_result <- macd(price_data, s = 12, l = 26, k = 9)
tail(macd_result)

# Test Monte Carlo simulation
mc_result <- monte_carlo(seed_price = 100, daily_vol = 0.02, num_sims = 100, num_days = 252)
str(mc_result)
