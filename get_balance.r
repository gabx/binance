# get MLC portfolio balance

# for further use, we run: mlc <- get_balance()
get_balance <- function () {
    
    library('binancer')
    library('rjson')
    
    env.var <- fromJSON(file = 'variables.env.json')
    binance_credentials(env.var[[1]],env.var[[2]])    

# get binance balance without zero
    bin.balance <- as_tibble(binance_balances())
    bin.balance <- filter(bin.balance, bin.balance$total != 0)
    bin.balance <- select(bin.balance, -c(free, locked))
    bin.balance <- arrange(bin.balance, asset)
    bin.balance <- filter(bin.balance, !(asset %in% c('ETWH', 'LUNA', 'USDC', 'XRP', 'ETHW')))

    safe.balance <- read_table("safe.csv", col_names = c('asset', 'total'))

# get total balance
    balance <- bind_rows(bin.balance, safe.balance) %>%
        group_by(asset) %>%
        summarise(total = sum(total))
    mlc.asset <- as.list(balance$asset)
    
# remove USDT form our assets & add USDT to each asset name
    mlc.asset <- mlc.asset[-which(mlc.asset == 'USDT')]
    mlc.asset.usdt <- as.list(paste(mlc.asset, 'USDT', sep = ''))
    # mlc.asset.usdt <- mlc.asset.usdt[-which(mlc.asset.usdt == 'USDTUSDT')]

    mlc <- list(balance = balance, asset.usdt = mlc.asset.usdt, asset = mlc.asset)
}


