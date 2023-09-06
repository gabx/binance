# get MLC portfolio balance
# git URL: https://github.com/gabx/binance.git

source('get_balance.r')
source('get_historic.r')
source('get_portfolio.r')

# create a new env to save our data
.PortfolioEnv <- new.env()

## START OF THE WHOLE SCRIPT ##

mlc <- get_balance()
open.price <- get_historic()
portfolio <- get_portfolio()


# ! HINT !
# set_names(c("foo", "bar")) |> purr::map_chr(paste0, ":suffix")

# export to pdf
library(gridExtra)
pdf('portoflio_20230831.pdf')
grid.table(portfolio)
dev.off()



# check_trades(start_time, end_time)
    startTime <- gsub('-', '', start_time)
    endTime <- gsub('-', '', end_time)
# find our two df in .PortfolioEnv
    df1 <- get(grep(startTime, ls(envir = .PortfolioEnv), value = TRUE), envir = .PortfolioEnv)
    df1 <- mutate(df1, 'asset' = paste0(df1$asset, 'USDT'))
    df2 <- get(grep(endTime, ls(envir = .PortfolioEnv), value = TRUE), envir = .PortfolioEnv)
    df2 <- mutate(df2, 'asset' = paste0(df2$asset, 'USDT'))

    
# check if assets have been sold or bought
# if total.x - total.y != 0, then we have traded. In this case, we need first to adjust quantity as
# they need to be equal. After we must take into account the trades.
# !! we can't exclude trades between the two dates with equal amounts
#     portfolio.2dates <- inner_join(df1, df2, by = 'asset')
#     compare.amount <- mutate(portfolio.2dates, amount = amount.x - amount.y)
# # keep rows with amount !=0 and remove BNB
#     trades <- filter(compare.amount, amount != 0)
#     trades <- filter(trades, asset != 'BNB')
#     trades <- mutate(trades, 'asset' = paste0(trades$asset, 'USDT'))
# # if length(trades$asset) = 0, we didn't trade.
# # ! HINT: one cancelled open order will change quantity but with no trades.
#     if (length(trades$asset) > 0) {
#         past.trades <- lapply(trades$asset, function(x) binance_all_orders(x, start_time = start_time, end_time = end_time))
#         names(past.trades) <- trades$asset
#         past.trades$BTCUSDT %>% filter(time >= start_time & time <= end_time) 
#     }

    
# get_trades_pl()
    # here we must check our assets in df1 & df2 are the same 
    if (identical(df1$asset, df2$asset) != TRUE) {
        stop('Nos assets sont différentes entre les deux dates')
    }
    past.trades <- lapply(df1$asset, function(x) binance_all_orders(x))
    names(past.trades) <- df1$asset
    # keep our needed period
    condition_filter <- function(tb) {
        return(filter(tb, time >= start_time))
    }
    past.trades.select <- lapply(past.trades, condition_filter)
    
# return last row of each tibbles of the list
last.rows <- map(past.trades, ~tail(.x, n = 1))
# keep specific columns
last.rows.select <- map(last.rows, ~select(.x, symbol, executed_qty, cummulative_quote_qty, status, time))

pl <- mutate(pl, 'PLRet' = ((total.y - total.x)/total.x)*100)


# get_trades

trade <- binance_all_orders('AAVEUSDT')
# to get correct average price, we must divide cumulative_quote_qty by executed_qty
# we must check status == FILLED and the time ex: 2023-07-27 22:14:18
# we can use a 24h only time frame: binance_all_orders('CELRUSDT', start_time = '2023-07-27', end_time = '2023-07-28')

# Compute returns

