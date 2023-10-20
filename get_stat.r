

# object of class xts input data is organized with asset returns in columns and
# dates represented in row. xts convenient with PerformanceAnalytics package

get_stat <- function() {
# Get a list of token as xts objects 
library(xts)
library(PerformanceAnalytics)
library(binancer)

portfolio.simul <- read_csv("simul_small.csv", col_names = 'asset', show_col_types = FALSE)
portfolio.simul.usd <- portfolio.simul %>% 
    mutate(asset = paste(asset, 'USDT', sep = ''))

asset.price.list <- lapply(portfolio.simul.usd$asset, function(x) binance_klines(x,
                            interval = '1d'))
extract_close <- function(df) {
    df[, c('close', 'close_time'), drop = FALSE]
}
asset.price.close <- lapply(asset.price.list, extract_close)

to_date <- function(df) {
    df <- df |> mutate(close_time = as.Date(close_time))
}
asset.price.close <- lapply(asset.price.close, to_date)

to_xts <- function(df) {
    xts::xts(x = df$close, order.by = as.Date(df$close_time))
}
asset.xts.lst <- lapply(asset.price.close, to_xts)
names(asset.xts.lst) <- portfolio.simul.usd$asset

# list of daily returns in xts format
asset.return.lst <- lapply(asset.xts.lst, Return.calculate)
# create a xts object with all returns and asset names as colnames
asset.return.xts <- na.omit(do.call(merge, asset.return.lst))
assign('asset.return.xts', asset.return.xts, envir = .GlobalEnv)

# Create our final tibble asset.stats
# 1 - Return.calculate : price to return to return.annualized
asset.return <- round(sapply(asset.return.lst, Return.annualized) * 100, digits = 2)
asset.stats <- as_tibble_col(asset.return, column_name = 'Annualized_return')
asset.stats <- rownames_to_column(asset.stats, var = 'Assets')
asset.stats$Assets <- names(asset.xts.lst)

# 2 - annualized standard deviation
asset.stddev <- round(sapply(asset.return.lst, StdDev.annualized) * 100, digits = 2)
asset.stddev <- as_tibble_col(unlist(asset.stddev), column_name = 'Annualized_volatility')

# 3 - annualized sharpe ratio
asset.sharpe <- round(sapply(asset.return.lst, SharpeRatio.annualized, scale = 365), digits = 2)
asset.sharpe <- as_tibble_col(unlist(asset.sharpe), column_name = 'Annualized_sharpe')

# 4 - skewness Positive skewness indicates that more of the returns are positive, negative skewness indicates that
# more of the returns are negative
# asset.skewness <- sapply(asset.return.lst, PerformanceAnalytics::skewness)
# asset.skewness <- as_tibble_col(asset.skewness, column_name = 'Skewness')

# 5 - semivariance, semideviation ??
# SemiDeviation and SemiVariance are implemented as a
# wrapper of DownsideDeviation with MAR=mean(R)
asset.semidev <- round(sapply(asset.return.lst, SemiDeviation) * 100, digits = 2)
asset.semidev <- as_tibble_col(asset.semidev, column_name = 'Semi_deviation')

# 6 maxdrawdown
asset.maxdd <- round(sapply(asset.return.lst, maxDrawdown) * 100, digits = 2)
asset.maxdd <- as_tibble_col(asset.maxdd, column_name = 'max_drawdown')
# OR Average drawdown + average recovery period
asset.avgdd <- round(sapply(asset.return.lst, AverageDrawdown) * 100, digits = 2)
asset.avgdd <- as_tibble_col(asset.avgdd, column_name = 'Average_drawdown')
asset.avgrec <- round(sapply(asset.return.lst, AverageRecovery), digits = 2)
asset.avgrec <- as_tibble_col(asset.avgrec, column_name = 'Average_recovery')


# 7 VAR
# More efficient estimates of VaR are obtained if a (correct) assumption
# is made on the return distribution, such as the normal distribution. If your return series is skewed
# and/or has excess kurtosis, Cornish-Fisher estimates of VaR can be more appropriat
asset.var <- round(sapply(asset.return.lst, VaR, method = 'historical') * 100, digits = 2)
asset.var <- as_tibble_col(asset.var, column_name = 'Var_historical')

# 8 CVAR or Expected shortfall
asset.es <- round(sapply(asset.return.lst, ETL, method = 'historical') * 100, digits = 2)
asset.es <- as_tibble_col(asset.es, column_name = 'Expected_shortfall')

# bind all
asset.all.stats <- bind_cols(asset.stats, asset.stddev, asset.sharpe, asset.semidev, asset.maxdd, asset.var)
# asset.all.stats <- bind_cols(asset.stats, asset.stddev, asset.sharpe, asset.semidev, asset.avgdd, asset.avgrec, asset.var)
}

#  table of various risk ratios
# table.DownsideRisk(asset.return.lst[[7]]) 
# chart with daily return and modified var, HistoricalES, HistoricalVaR
# chart.BarVaR(asset.return.lst[[6]], methods = 'ModifiedVaR')
# chart.Histogram(asset.return.lst[[3]], methods = 'add.normal')  histogram of returns and see if normal distribution
# chart.RiskReturnScatter() A wrapper to create a scatter chart of annualized returns versus annualized risk (standard deviation)

