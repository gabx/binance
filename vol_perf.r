# compute volatility, performance and visualize it on a chart
# assets are in a list mlc_asset

# our list of token
balance <- get_balance()
mlc.asset <- balance$asset
mlc.asset <- as.list(paste(mlc.asset, 'USDT', sep = ''))
mlc.asset <- mlc.asset[mlc.asset != 'USDTUSDT']

## 1 compute rate of return for 2 weeks and 2 months
ror_3months <- function(x) {                 
x.close <- as_tibble(binance_klines(x, interval = '8h', start_time = '2023-04-11', end_time = '2023-07-11'))
x.close <- select(x.close, open_time, open)
x.close <- filter(x.close, grepl('18:00:00', open_time))
ror.3months <- (x.close$open[90]-x.close$open[1])/x.close$open[1]
ror.3months <- round(ror.3months * 100, digits = 2)

}

ror_3weeks <- function(x) {
x.close <- as_tibble(binance_klines(x, interval = '8h', start_time = '2023-06-11', end_time = '2023-07-11'))
x.close <- select(x.close, open_time, open)
x.close <- filter(x.close, grepl('18:00:00', open_time))
ror.3weeks <- (x.close$open[21]-x.close$open[1])/x.close$open[1]
ror.3weeks <- round(ror.3weeks * 100, digits = 2 )
}

# a tibble of ror for each asset
portfolio.ror.3months <- lapply(mlc.asset, ror_3months)
names(portfolio.ror.3months) <- mlc.asset
portfolio.ror.3months <- portfolio.ror.3months %>% map_dfr(~ .x %>% as_tibble(), .id = 'asset')
portfolio.ror.3months <- rename(portfolio.ror.3months, ror.3months = value)

portfolio.ror.3weeks <- lapply(mlc.asset, ror_3weeks)
names(portfolio.ror.3weeks) <- mlc.asset
portfolio.ror.3weeks <- portfolio.ror.3weeks %>% map_dfr(~ .x %>% as_tibble(), .id = 'asset')
portfolio.ror.3weeks <- rename(portfolio.ror.3weeks, ror.3weeks = value)

mlc.asset.ror <- inner_join(portfolio.ror.3months, portfolio.ror.3weeks, by = 'asset')
mlc.asset.ror <- arrange(mlc.asset.ror, desc(ror.3months))

## 2 compute volatility of assets

# compute daily return
# volatility is the standard deviation of prices over a period of time.
# select last 60 days
# to work with ts objects, use tk_ts() from timetk package
x.close.90 <- x.close[length(x.close):length(x.close) - 90,]
volatiliday.3months <- x.close.90 %>% summarise(mean = mean(close), volatility = sd(close))

# compute returns
ret <- mutate(x_close.90, daily_ret = (close - lag(close))/lag(close))
ret <- mutate(ret, daily_ret = round(daily_ret * 100, digits = 2))
ret <- na.omit(ret)

