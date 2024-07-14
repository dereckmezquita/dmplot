#' Monte Carlo Simulation R6 Class
#'
#' @description
#' An R6 class for performing Monte Carlo simulations on financial time series data.
#' This class provides methods for data preparation, simulation execution, and result visualization.
#'
#' @details
#' The MonteCarlo class uses historical price data to calculate volatility and perform
#' Monte Carlo simulations for future price movements. It leverages the C++ implementation
#' of the Monte Carlo algorithm for efficiency.
#'
#' @field data A data.table containing the historical price data.
#' @field simulation_results A data.table containing the results of the Monte Carlo simulation.
#' @field end_prices A data.table containing the final prices from each simulation path.
#' @field log_historical Logical. Whether to use log returns for historical volatility calculation.
#' @field number_sims Integer. The number of simulation paths to generate.
#' @field project_days Integer. The number of days to project into the future.
#' @field start_date POSIXct. The start date for the simulation (last date of historical data).
#' @field verbose Logical. Whether to print progress messages.
#'
#' @export
MonteCarlo <- R6::R6Class(
    "MonteCarlo",
    private = list(
        validate_data = \() {
            if (!inherits(self$data, "data.table")) {
                stop("data must be a data.table")
            }
            required_cols <- c(
                "symbol", "datetime", "open", "high",
                "low", "close", "volume", "turnover"
            )

            if (!all(required_cols %in% colnames(self$data))) {
                stop("data must contain the following columns: ", paste(required_cols, collapse = ", "))
            }
        },
        prepare = \(log_historical = FALSE) {
            dt <- self$data

            if (log_historical) {
                dt[, historical_returns := log(close / data.table::shift(close, 1L, type = "lag")) ]
            } else {
                dt[, historical_returns := ((close - data.table::shift(close, 1L, type = "lag")) / close) ]
            }

            # this is the standard deviation of the historical returns
            # we're calculating it manually because R uses n-1 as the denominator
            private$daily_vol <- sqrt(
                sum((dt$historical_returns[-1] - mean(dt$historical_returns[-1])) ^ 2)
                    /
                (length(dt$historical_returns[-1]))
            )

            private$seed_price <- dt$close[nrow(dt)]
        },
        seed_price = NA_real_,
        daily_vol = NA_real_
    ),
    active = list(
        #' @field results A list containing simulation results and end prices.
        results = \() {
            return(list(
                simulation_results = self$simulation_results,
                end_prices = self$end_prices
            ))
        }
    ),
    public = list(
        data = NULL,
        simulation_results = NULL,
        end_prices = NULL,
        log_historical = FALSE,
        number_sims = 1000,
        project_days = 30,
        start_date = NULL,
        verbose = FALSE,

        #' @description
        #' Create a new MonteCarlo object.
        #' @param dt A data.table containing historical price data.
        #' @param log_historical Logical. Whether to use log returns for historical volatility calculation.
        #' @param number_sims Integer. The number of simulation paths to generate.
        #' @param project_days Integer. The number of days to project into the future.
        #' @param verbose Logical. Whether to print progress messages.
        initialize = \(dt, log_historical = FALSE, number_sims = 1000, project_days = 30, verbose = FALSE) {
            self$log_historical <- log_historical
            self$number_sims <- number_sims
            self$project_days <- project_days
            self$data <- dt
            self$start_date <- dt$datetime[nrow(dt)]
            self$verbose <- verbose
            private$validate_data()
            self$simulation_results <- vector(mode = "list", length = number_sims)
        },

        #' @description
        #' Run the Monte Carlo simulation.
        carlo = \() {
            private$prepare(log_historical = self$log_historical)

            # message("Running Monte Carlo cpp simulation...")
            results <- dmplot::monte_carlo(
                seed_price = private$seed_price,
                daily_vol = private$daily_vol,
                num_sims = self$number_sims,
                num_days = self$project_days
            )

            # Process and store results...
            data.table::setDT(results$simulations)
            data.table::setDT(results$end_prices)

            # Adding datetime column
            results$simulations[, datetime := rep(
                seq(
                    from = self$start_date,
                    to = self$start_date + lubridate::days(self$project_days - 1),
                    by = "day"
                ),
                times = self$number_sims
            )]

            results$end_prices[,
                datetime := rep(self$start_date + lubridate::days(self$project_days - 1), times = self$number_sims)
            ]

            self$simulation_results <- results$simulations[]
            self$end_prices <- results$end_prices[]
        },

        #' @description
        #' Plot the simulated price paths.
        #' @return A ggplot object showing the simulated price paths.
        plot_prices = \() {
            if (is.null(self$simulation_results) || is.null(self$end_prices)) {
                stop("Must run simulation first")
            }

            self$simulation_results |>
                ggplot2::ggplot(ggplot2::aes(datetime, close, group = simulation, colour = as.character(simulation))) +
                ggplot2::geom_line(alpha = 0.5, linewidth = 1) +
                theme_dereck_dark() +
                ggplot2::theme(legend.position = "none") +
                ggplot2::labs(
                    title = "Monte carlo: price of bitcoin",
                    # subtitle = stringr::str_interp('Daily volatility ${round(daily_vol, 6)} calculated from: ${start} - ${end} (n days: ${end - start})\nmean<last_prices> on ${end + lubridate::days(num_days)} days(${num_days}): ${round(mean(last_prices), 2)}'),
                    caption = stringr::str_interp('Iterations: ${self$number_sims}')
                )
        },

        #' @description
        #' Plot the distribution of final prices.
        #' @return A ggplot object showing the distribution of final prices.
        plot_distribution = \() {
            if (is.null(self$simulation_results) || is.null(self$end_prices)) {
                stop("Must run simulation first")
            }

            self$end_prices |>
                ggplot2::ggplot(ggplot2::aes(x = 1, y = log2(close))) +
                ggplot2::geom_violin() +
                ggplot2::geom_boxplot(width = 0.1, fill = "gold") +
                # ggplot2::scale_y_continuous(n.breaks = 50, trans = "log2") +
                # BUG: Error in break_suffix[bad_break][improved_break & !power10_break] <- names(lower_break[improved_break &  : NAs are not allowed in subscripted assignments
                # ggplot2::scale_y_continuous(n.breaks = 25, labels = function(x) {
                #     # scales::label_number_si(accuracy = 0.1)(2 ^ x)
                #     scales::label_number(scale_cut = scales::cut_short_scale())(2 ^ x)
                # }) +
                theme_dereck_dark() +
                ggplot2::labs(
                    title = "Distribution of final simulated prices",
                    # subtitle = stringr::str_interp('Daily volatility ${round(private$daily_vol, 6)} calculated from: ${start} - ${end} (n days: ${end - start})\nmean<last_prices> on ${end + lubridate::days(num_days)} days(${num_days}): ${round(mean(last_prices), 2)}'),
                    caption = stringr::str_interp('Iterations: ${self$number_sims}')
                )
        },

        #' @description
        #' Plot historical prices and simulated future prices.
        #' @return A ggplot object showing historical and simulated prices.
        plot_prices_and_predictions = \() {
            if (is.null(self$simulation_results) || is.null(self$end_prices)) {
                stop("Must run simulation first")
            }

            scale_period <- ""
            if (self$project_days <= 30) {
                scale_period <- "1 day"
            } else if (self$project_days <= 90) {
                scale_period <- "1 week"
            } else if (self$project_days <= 365) {
                scale_period <- "1 month"
            } else {
                scale_period <- "1 year"
            }

            self$data |>
                ggplot2::ggplot() +
                dmplot::stat_candlestick(ggplot2::aes(
                    x = datetime,
                    open = open,
                    close = close,
                    high = high,
                    low = low,
                    group = symbol
                )) +
                ggplot2::geom_line(
                    data = self$simulation_results,
                    ggplot2::aes(x = datetime, y = close, group = simulation),
                    colour = "blue",
                    alpha = 0.1,
                    linewidth = 0.5
                ) +
                ## ------------------------------------
                ggplot2::scale_x_datetime(
                    date_breaks = scale_period,
                    date_labels = "%d %b %y"
                ) + 
                theme_dereck_dark() +
                ggplot2::labs(
                    x = ggplot2::element_blank(),
                    y = ggplot2::element_blank()
                ) +
                ggplot2::theme(
                    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
                    panel.grid.minor.x = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_text(angle = 90),
                    legend.position = "none"
                )
        }
    )
)
