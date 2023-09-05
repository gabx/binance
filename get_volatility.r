
# volatility
# 1 get daily returns or
# get log returns with asset prices diff(log(asset_prices))

# 2 get standard deviation sd()

return_percentage <- function(x,y){
    (y - x) / x * 100
}