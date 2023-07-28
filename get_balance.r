# get MLC portfolio balance
# git URL: https://github.com/gabx/binance.git

library(binancer)
binance_credentials(bin_key,bin_secret)

# for clarity, we use mlc <- get_balance()
get_balance <- function () {
    
    library('binancer')
    library('rjson')
    
    env.var <- fromJSON(file = 'variables.env.json')
    binance_credentials(env.var[[1]],env.var[[2]])    

# get binance balance without zero
    bin.balance <- as_tibble(binance_balances())
    bin.balance <- filter(bin.balance, bin.balance$free != 0)
    bin.balance <- select(bin.balance, -c(free, locked))
    bin.balance <- arrange(bin.balance, asset)
    bin.balance <- filter(bin.balance, !(asset %in% c('ETWH', 'LUNA', 'USDC', 'XRP', 'ETHW')))

    safe.balance <- read_table("safe.csv", col_names = c('asset', 'total'))

# get total balance
    balance <- bind_rows(bin.balance, safe.balance) %>%
        group_by(asset) %>%
        summarise(total = sum(total))

    mlc.asset <- as.list(balance$asset)
# remove USDT form our assets & add USDT to each asset
    mlc.asset <- mlc.asset[-which(mlc.asset == 'USDT')]
    mlc.asset.usdt <- as.list(paste(mlc.asset, 'USDT', sep = ''))

    return(list(balance = balance, asset.usd = mlc.asset.usdt, asset = mlc.asset))
}

# for clarity, we use open.price <- get_historic()
get_historic <- function() {

    library('binancer')
    library('rjson')
    env.var <- fromJSON(file = 'variables.env.json')
    binance_credentials(env.var[[1]],env.var[[2]])  
   

    mlc <- get_balance()

    #asset.price.tb <- lapply(mlc$asset.usd, function(x, y, z) binance_klines(x,
    #interval = '8h', start_time = '2023-07-17', end_time = '2023-07-21'))
    asset.price.tb <- lapply(mlc$asset.usd, function(x) binance_klines(x,
             interval = '8h'))
    # keep open value for each day 18:00
    keep_daily_open <- function(df) {
    df[grepl('18:00:00', df$open_time)]
    }

    asset.price.open <- lapply(asset.price.tb, keep_daily_open)
    names(asset.price.open) <- mlc$asset
    # final list of df with open_time, open and symbol columns
    asset.price.open <- map(asset.price.open, ~select(.x, 1, 2, 12))
    # change df into tibbles
    asset.price <- map(asset.price.open, as_tibble)
}


# we must remove row with USDT
# balance.no.usd <- filter(balance, !asset == 'USDT')

# get a asset/value tibble with open price. 
open.price.list <- lapply(open.price, function(x) pull(x,open_time, open))

open.price.list <- open.price.list |> map(c(length(open.price.list[[1]]),1))
# transform list into tibble
open.price <- open.price.list %>% map_dfr(~ .x %>% as_tibble(), .id = 'asset')

# create final tibble
balance.final <- inner_join(balance.no.usd, open.price, by = 'asset')
balance.final <- rename(balance.final, 'amount' = total)
balance.final <- mutate(balance.final, 'total' = amount * value)

## add a weight column
sum.asset <- sum(balance %>% pull(total))
balance1 <- mutate(balance, weight = total / sum.asset)
balance1 <- mutate(balance1, weight = round(weight * 100, digits = 2))

return(balance)


# set_names(c("foo", "bar")) |> purr::map_chr(paste0, ":suffix")
