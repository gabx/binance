

# object of class xts input data is organized with asset returns in columns and
# dates represented in row. xts convenient with PerformanceAnalytics package


get_quotes <- function(p) {
    
    library(xts)
    library(PerformanceAnalytics)
    library(binancer)
    
    # TO DO: pass simul.csv as argument
    # return a xts object of returns on a specific period
    portfolio.simul <- read_csv("simul.csv", col_names = 'asset', show_col_types = FALSE)
    portfolio.simul.usd <- portfolio.simul %>% 
        mutate(asset = paste(asset, 'USDT', sep = ''))
    asset.price.list <- lapply(portfolio.simul.usd$asset, function(x) binance_klines(x,
        interval = p ))
    asset.price.close <- lapply(asset.price.list, function(df) 
        {df[, c('close', 'close_time'), drop = FALSE]})
    asset.price.close <- lapply(asset.price.close, function(df)
        {df <- df |> mutate(close_time = as.Date(close_time))})
    names(asset.price.close) <- portfolio.simul.usd$asset
    assign('asset.price.close', asset.price.close, envir = .GlobalEnv)
    to_xts <- function(df) {
    xts::xts(x = df$close, order.by = as.Date(df$close_time))
    }
    asset.price.xts <- lapply(asset.price.close, to_xts)
}

get_stat <- function(p, s) {

    library(xts)
    library(PerformanceAnalytics)
    library(corrplot)
    
    asset.price.xts <- get_quotes(p)
    assign('asset.price.xts',asset.price.xts, envir = .GlobalEnv)
    asset.return.lst <- lapply(asset.price.xts, Return.calculate)
    asset.return.xts <- do.call(merge, asset.return.lst)
    assign('asset.return.xts',asset.return.xts, envir = .GlobalEnv)
# Create our final tibble asset.stats
# 1 - Return.calculate : price to return to return.annualized
    asset.return <- round(sapply(asset.return.xts, Return.annualized) * 100, digits = 2)
    asset.stats <- as_tibble_col(asset.return, column_name = 'Annualized_return')
    asset.stats <- rownames_to_column(asset.stats, var = 'asset')
    asset.stats$asset <- names(asset.price.xts)

# 2 - annualized standard deviation
    asset.stddev <- round(sapply(asset.return.xts, StdDev.annualized) * 100, digits = 2)
    asset.stddev <- as_tibble_col(asset.stddev, column_name = 'Annualized_volatility')

# 3 - annualized sharpe ratio
    asset.sharpe <- round(sapply(asset.return.xts, SharpeRatio.annualized, scale = s), digits = 2)
# asset.sharpe <- as_tibble_col(unlist(asset.sharpe), column_name = 'Annualized_sharpe')
    asset.sharpe <- as_tibble_col(asset.sharpe, column_name = 'Annualized_sharpe')

# 4 Kurtosis
# > 3  heavier tails and a higher peak 
# < 3 lighter tails and a flatter peak
    asset.kurtosis <- round(sapply(asset.return.xts, PerformanceAnalytics::kurtosis), digits = 2)
    asset.kurtosis <- as_tibble_col(asset.kurtosis, column_name = 'Kurtosis')

# 4 - skewness Positive skewness indicates that more of the returns are positive, negative skewness indicates that
# more of the returns are negative
    asset.skewness <- round(sapply(asset.return.xts, PerformanceAnalytics::skewness), digits = 2)
    asset.skewness <- as_tibble_col(asset.skewness, column_name = 'Skewness')

# 5 - semivariance, semideviation ??
# SemiDeviation and SemiVariance are implemented as a
# wrapper of DownsideDeviation with MAR=mean(R)
# Downside deviation, similar to semi deviation, eliminates positive returns when calculating risk.
# Instead of using the mean return or zero, it uses the Minimum Acceptable Return as proposed by
# Sharpe (which may be the mean historical return or zero)
    asset.semidev <- round(sapply(asset.return.xts, SemiDeviation) * 100, digits = 2)
    asset.semidev <- as_tibble_col(asset.semidev, column_name = 'Semi_deviation')

# 6 maxdrawdown
    asset.maxdd <- round(sapply(asset.return.xts, maxDrawdown) * 100, digits = 2)
    asset.maxdd <- as_tibble_col(asset.maxdd, column_name = 'max_drawdown')
# OR Average drawdown + average recovery period
    asset.avgdd <- round(sapply(asset.return.xts, AverageDrawdown) * 100, digits = 2)
    asset.avgdd <- as_tibble_col(asset.avgdd, column_name = 'Average_drawdown')
    asset.avgrec <- round(sapply(asset.return.xts, AverageRecovery), digits = 2)
    asset.avgrec <- as_tibble_col(asset.avgrec, column_name = 'Average_recovery')


# 7 VAR
# More efficient estimates of VaR are obtained if a (correct) assumption
# is made on the return distribution, such as the normal distribution. If your return series is skewed
# and/or has excess kurtosis, Cornish-Fisher estimates of VaR can be more appropriat
    asset.var <- round(sapply(asset.return.xts, VaR, method = 'historical') * 100, digits = 2)
    asset.var <- as_tibble_col(asset.var, column_name = 'Var_historical')

# 8 CVAR or Expected shortfall
    asset.es <- round(sapply(asset.return.xts, ETL, method = 'historical') * 100, digits = 2)
    asset.es <- as_tibble_col(asset.es, column_name = 'Expected_shortfall')

# bind all
    asset.all.stats <- bind_cols(asset.stats, asset.stddev, asset.kurtosis, asset.skewness,
                             asset.sharpe, asset.maxdd, asset.var)
# asset.all.stats <- bind_cols(asset.stats, asset.stddev, asset.sharpe, asset.semidev, asset.avgdd, asset.avgrec, asset.var)

# remove USDT from 1st column
    asset.all.stats <- asset.all.stats %>% mutate(asset = gsub('USDT', '', asset))
    
    # at a average line below
    average.row <- asset.all.stats |>
        select_if(is.numeric) |>
        summarise(across(everything(), ~mean(., na.rm = TRUE)))
    asset <- c('AVERAGE')
    average.row <- add_column(asset, .before = 1, .data = average.row)
    asset.all.stats <- bind_rows(asset.all.stats, average.row)
}

get_correl <- function() {
    
    # we need to remove all NA to compare our various assets
    list.length <- sapply(asset.price.xts, length)
    length.shortest <- min(list.length)
    row.number <- nrow(asset.return.xts) - length.shortest +2
    asset.return.no_na.xts <- asset.return.xts[row.number:nrow(asset.return.xts), ]
    assign('asset.return.no_na.xts',asset.return.no_na.xts, envir = .GlobalEnv)
}


#  table of various risk ratios
# table.DownsideRisk(asset.return.lst[[7]]) 
# table.Drawdowns(asset.return.xts$MKRUSDT)
# chart with daily return and modified var, HistoricalES, HistoricalVaR
# chart.BarVaR(asset.return.lst[[6]], methods = 'ModifiedVaR')
# chart.Histogram(asset.return.lst[[3]], methods = 'add.normal')  histogram of returns and see if normal distribution
# chart.RiskReturnScatter(asset.return.xts, scale = 365) A wrapper to create a scatter chart of annualized returns versus annualized risk (standard deviation)
# corr <- cor(asset.return.xts)
# corrplot(corr, method = 'number')
# corrplot(corr, type = 'upper', method = 'number')
# table.Arbitrary(asset.return.xts$BTCUSDT,metrics=c("VaR", 'mean'), metricsNames=c("modVaR","mean"),p=.95) we can pass any wanted metrics



