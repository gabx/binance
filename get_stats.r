# get portfolio stats on 7, 30 and 60 days

# we start with our list of tb open.price.list

# function to retrun the quote depending of the day, noted d
last_quote <- function(l,d){
    return(l[(length(l) -d):length(l)])
}

# function to transform a vector to a tibble
transform_to_tb <- function(vec){
    tibble(
    Asset = names(vec),
    Quotes = vec
    )
}

# function to extract first and last element of a list 
extract_first_last <- function(x) {
    if (length(x) >= 2) {
        c(x[1], x[length(x)])
    } else {
        x
    }
}

last.quote <- lapply(open.price.list, function(l) last_quote(l, 0))
last.7.quote <- lapply(open.price.list, function(l) last_quote(l, 6))
last.30.quote <- lapply(open.price.list, function(l) last_quote(l, 29))
last.90.quote <- lapply(open.price.list, function(l) last_quote(l, 89))

last7q <- test %>%
    mutate(Quotes = map(Quotes, extract_first_last))

 return_percentage <- function(x,y){
    (y - x) / x * 100
}
# volatility
# 1 get daily returns or
# get log returns with asset prices diff(log(asset_prices))

# 2 get standard deviation sd()
