box::use(usethis[ use_data ])
box::use(dt = data.table)
box::use(dev/`anonymise-data`[ anonymise_data ])

# 2. Feature counts genes
feature_counts0 <- read.csv("dev/data/original.ignore/feature-counts-genes.csv")
dt$setDT(feature_counts0)

# save original colnames
sample_names <- colnames(feature_counts0)
sample_names <- sample_names[!sample_names %in% c("GeneID", "GeneSymbol", "GeneBiotype")]
sample_names <- dt$data.table(original = sample_names)

feature_counts <- anonymise_data(feature_counts0)

sample_names[, new_name := colnames(feature_counts)[4:ncol(feature_counts)] ]

dt$fwrite(sample_names, "dev/data/original.ignore/new-names-table.csv")
dt$fwrite(feature_counts, "dev/data/feature-counts-genes.csv")

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
