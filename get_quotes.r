

get_quotes <- function(interval, csv) {
    
    library(PerformanceAnalytics)
    library(binancer)
    
    # TO DO: pass simul.csv as argument
    # return a xts object of returns on a specific period
    portfolio.simul <- read_csv(csv, col_names = 'asset', show_col_types = FALSE)
    portfolio.simul.usd <- portfolio.simul %>% 
        mutate(asset = paste(asset, 'USDT', sep = ''))
    asset.price.list <- lapply(portfolio.simul.usd$asset, function(x) binance_klines(x,
                                                                                     interval = interval ))
    asset.price.close <- lapply(asset.price.list, function(df) 
    {df[, c('close', 'close_time'), drop = FALSE]})
    asset.price.close <- lapply(asset.price.close, function(df)
    {df <- df |> mutate(close_time = as.Date(close_time))})
    names(asset.price.close) <- portfolio.simul.usd$asset
    assign('asset.price.close', asset.price.close, envir = .GlobalEnv)
    to_xts <- function(df) {
        xts::xts(x = df$close, as.Date(df$close_time))
    }
    asset.price.xts <- lapply(asset.price.close, to_xts)
}