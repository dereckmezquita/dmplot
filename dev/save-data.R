box::use(usethis[ use_data ])
box::use(dt = data.table)
box::use(dev/`anonymise-data`[ anonymise_data ])

# 2. Feature counts genes
feature_counts0 <- read.csv("dev/data/feature-counts-genes.csv")
dt$setDT(feature_counts0)

feature_counts <- anonymise_data(feature_counts0)
use_data(feature_counts, overwrite = TRUE)

# 3. Volcano differential expression
diff_expr_res <- read.csv("dev/data/volcano-differential-expression.csv")
dt$setDT(diff_expr_res)
use_data(diff_expr_res, overwrite = TRUE)


box::use(kucoin[ get_market_data ])

ticker <- "BTC/USDT"

btc_1_year_hourly <- get_market_data(
    symbols = ticker,
    from = lubridate::now() - lubridate::days(365),
    to = lubridate::now(),
    frequency = "1 hour"
)

dt$setDT(btc_1_year_hourly)

use_data(btc_1_year_hourly, overwrite = TRUE)
