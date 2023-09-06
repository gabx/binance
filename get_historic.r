# get MLC portfolio balance
# git URL: https://github.com/gabx/binance.git


# for further use, we run: open.price <- get_historic()
get_historic <- function() {

    #asset.price.tb <- lapply(mlc$asset.usd, function(x, y, z) binance_klines(x,
    #interval = '8h', start_time = '2023-07-17', end_time = '2023-07-21'))
    asset.price.list <- lapply(mlc$asset.usdt, function(x) binance_klines(x,
             interval = '8h'))
    # keep open value for each day 18:00
    keep_daily_open <- function(df) {
    df[grepl('18:00:00', df$open_time)]
    }

    asset.price.open <- lapply(asset.price.list, keep_daily_open)
    names(asset.price.open) <- mlc$asset
    # final list of df with open_time, open and symbol columns
    asset.price.open <- map(asset.price.open, ~select(.x, 1, 2, 12))
    # change df into tibbles
    open.price <- map(asset.price.open, as_tibble)
    return(open.price)
}
