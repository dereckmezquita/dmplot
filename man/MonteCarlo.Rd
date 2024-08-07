% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MonteCarlo.R
\name{MonteCarlo}
\alias{MonteCarlo}
\title{Monte Carlo Simulation R6 Class}
\description{
An R6 class for performing Monte Carlo simulations on financial time series data.
This class provides methods for data preparation, simulation execution, and result visualization.
}
\details{
The MonteCarlo class uses historical price data to calculate volatility and perform
Monte Carlo simulations for future price movements. It leverages the C++ implementation
of the Monte Carlo algorithm for efficiency.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{data}}{A data.table containing the historical price data.}

\item{\code{simulation_results}}{A data.table containing the results of the Monte Carlo simulation.}

\item{\code{end_prices}}{A data.table containing the final prices from each simulation path.}

\item{\code{log_historical}}{Logical. Whether to use log returns for historical volatility calculation.}

\item{\code{number_sims}}{Integer. The number of simulation paths to generate.}

\item{\code{project_days}}{Integer. The number of days to project into the future.}

\item{\code{start_date}}{POSIXct. The start date for the simulation (last date of historical data).}

\item{\code{verbose}}{Logical. Whether to print progress messages.}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{results}}{A list containing simulation results and end prices.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MonteCarlo-new}{\code{MonteCarlo$new()}}
\item \href{#method-MonteCarlo-carlo}{\code{MonteCarlo$carlo()}}
\item \href{#method-MonteCarlo-plot_prices}{\code{MonteCarlo$plot_prices()}}
\item \href{#method-MonteCarlo-plot_distribution}{\code{MonteCarlo$plot_distribution()}}
\item \href{#method-MonteCarlo-plot_prices_and_predictions}{\code{MonteCarlo$plot_prices_and_predictions()}}
\item \href{#method-MonteCarlo-clone}{\code{MonteCarlo$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MonteCarlo-new"></a>}}
\if{latex}{\out{\hypertarget{method-MonteCarlo-new}{}}}
\subsection{Method \code{new()}}{
Create a new MonteCarlo object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MonteCarlo$new(
  dt,
  log_historical = FALSE,
  number_sims = 1000,
  project_days = 30,
  verbose = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dt}}{A data.table containing historical price data.}

\item{\code{log_historical}}{Logical. Whether to use log returns for historical volatility calculation.}

\item{\code{number_sims}}{Integer. The number of simulation paths to generate.}

\item{\code{project_days}}{Integer. The number of days to project into the future.}

\item{\code{verbose}}{Logical. Whether to print progress messages.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MonteCarlo-carlo"></a>}}
\if{latex}{\out{\hypertarget{method-MonteCarlo-carlo}{}}}
\subsection{Method \code{carlo()}}{
Run the Monte Carlo simulation.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MonteCarlo$carlo()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MonteCarlo-plot_prices"></a>}}
\if{latex}{\out{\hypertarget{method-MonteCarlo-plot_prices}{}}}
\subsection{Method \code{plot_prices()}}{
Plot the simulated price paths.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MonteCarlo$plot_prices()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A ggplot object showing the simulated price paths.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MonteCarlo-plot_distribution"></a>}}
\if{latex}{\out{\hypertarget{method-MonteCarlo-plot_distribution}{}}}
\subsection{Method \code{plot_distribution()}}{
Plot the distribution of final prices.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MonteCarlo$plot_distribution()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A ggplot object showing the distribution of final prices.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MonteCarlo-plot_prices_and_predictions"></a>}}
\if{latex}{\out{\hypertarget{method-MonteCarlo-plot_prices_and_predictions}{}}}
\subsection{Method \code{plot_prices_and_predictions()}}{
Plot historical prices and simulated future prices.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MonteCarlo$plot_prices_and_predictions()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A ggplot object showing historical and simulated prices.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MonteCarlo-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MonteCarlo-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MonteCarlo$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
