# return a plot with return/volatility 

get_volror <- function(int){
    
    # x is a number of days
    portfolio.simul <- read_csv("simul1.csv", col_names = 'asset', show_col_types = FALSE)
    portfolio.simul.usd <- portfolio.simul %>% 
    mutate(asset = paste(asset, 'USDT', sep = ''))

    asset.price <- lapply(portfolio.simul.usd$asset, function(x) binance_klines(
        x, interval = '1d', limit = int))

    close.price <- lapply(asset.price, function(x) pull(x,close))

    # function to extract first and last element of a list 
    extract_first_last <- function(x) {
    if (length(x) >= 2) {
        c(x[1], x[length(x)])
    } else {
        x
    }
}

    # annualized return 
    # ((ending_value / beginning_value) ^ (1 / number_of_years) - 1) * 100

    # annualized returns
    close.first.last <- lapply(close.price, function(x) extract_first_last(x))
    return <- map(close.first.last, ~ ((.x[2] / .x[1]) ^ (1 / 1) -1) *100)
    return <- lapply(return, function(x) round(x, digits = 2))
    names(return) <- portfolio.simul$asset
    return <- enframe(return, name = 'asset', value = 'Return')
    return$Return <- unlist(return$Return)

# annualized volatility
# log_returns <- lapply(close.price, function(x) diff(log(x)))
# volat <- lapply(log_returns, function(x) round(sd(x) * 100, digits = 2))
# names(volat) <- portfolio.simul$asset
# volat <- enframe(volat, name = 'asset', value = 'Volatility')
# volat$Volatility <- unlist(volat$Volatility)

    daily_returns <- lapply(close.price, function(x) diff(x)/lag(x))
    mean_returns <- lapply(daily_returns, function(x) mean(x, na.rm = TRUE))
    my_vol <- lapply(daily_returns, function(x) round((sd(x, na.rm = TRUE) * sqrt(int) * 100), digits = 2))
    names(my_vol) <- portfolio.simul$asset
    volat <- enframe(my_vol, name = 'asset', value = 'Volatility')
    volat$Volatility <- unlist(volat$Volatility)

    # join
    vol_ror <- left_join(return, volat, by = 'asset')
}

