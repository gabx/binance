# get_journal
# compute the portfolio PL by token 

get_journal <- function(end.day = as.Date(format(Sys.Date(), "%Y-%m-%d")), number_of_days) {

    library(PMwR)
    
    start.day <- end.day - number_of_days

# get a list of days to apply binance_all_orders()
# no more than 24 hours between start and end time
    trading.days <- seq(start.day, by = 'day', length.out = number_of_days)

    my_trades <- function(x) {
    Map(function(y,z) binance_all_orders(x, start_time = y, end_time = z), head(trading.days, -1), tail(trading.days, -1))
}
    my.trades.full <- lapply(mlc$asset.usdt, function(x) my_trades(x))
    non_empty_df <- function(l) {
    lapply(l, function(df) df[sapply(df, function(df) nrow(df) !=0)])
    # return(l)
}
# remove any empty list or data frame
    my.trades <- non_empty_df(my.trades.full)
    my.trades <- Filter(function(x) length(x) > 0, my.trades)

# filter status, keep only needed columns, replace buy and sell by 1 and -1
    transform_function <- function(df) {
        df <- filter(df, status == 'FILLED')
        df <- select(df, symbol, price, executed_qty, cummulative_quote_qty, side, time)
        df <- df |> mutate(price = cummulative_quote_qty / executed_qty)
        df <- df |> mutate(executed_qty = if_else(side == 'BUY', executed_qty, executed_qty * -1))
    }                    
    #df <- df |> mutate(executed_qty = ifelse(side == 'BUY', executed_qty, ifelse(side == 'SELL', executed_qty * -1, executed_qty)))
    # cummulative_quote_qty = if_else(side == 'BUY', cummulative_quote_qty, cummulative_quote_qty * -1))
    # df <- df |> mutate(side = as.numeric(ifelse(side == 'BUY', 1, ifelse(side == 'SELL', -1, side))))
    # df <- df |> mutate(side = if_else(side == 'BUY', 1, -1))
    # df <- df |> mutate(cummulative_quote_qty = cummulative_quote_qty * side)
    # df <- df |> select(symbol, executed_qty, cummulative_quote_qty)

# result is a list of data frames with all FILLED trades for the past week
my.trades.transformed <- lapply(my.trades, function(lst) {
         lapply(lst, transform_function)
})

# convert our list of df to one tbl
my.trades.week <- as_tibble(my.trades.transformed |> bind_rows())

# change some columns
my.trades.week <- my.trades.week |> select(symbol, price, executed_qty, side, time)
my.trades.week <- my.trades.week |> mutate(time = as.Date(time))
my.trades.week <- my.trades.week |> rename(instrument = symbol, amount = executed_qty)
# from tibble to journal class
my.journal <- as.journal(my.trades.week)
my.journal <- sort(my.journal, by = 'timestamp', decreasing = TRUE)

}

# my.journal.org <- toOrg(my.journal)
# aggregate(. ~ asset, data = my.trades.week, FUN = sum)
