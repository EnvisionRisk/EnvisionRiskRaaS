% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/envrsk_api_bridge_2_R.R
\name{envrsk_market_price_raw}
\alias{envrsk_market_price_raw}
\title{Market Price Raw}
\usage{
envrsk_market_price_raw(symbols)
}
\arguments{
\item{symbols}{A list, the symbols of the financial instruments for which the
market prices are to be obtained.}
}
\value{
A list containing the input parameters, the time of operation,
        the output market price data, and details of mapped and unmapped symbols.
        If the API call is unsuccessful, the original API response is returned.
}
\description{
This function calls the 'market-price-raw' endpoint of the EnvisionRisk API
to obtain the market prices for a set of financial instruments. The prices are
reported in the specified base currency.
}
\examples{
\dontrun{
market_prices <- envrsk_market_price_raw(symbols = c("AAPL.US", "DANSKE.CO"))
}
}
