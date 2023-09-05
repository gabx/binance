# get portfolio stats on 7, 30 and 90 days

# we start with our list of tb open.price.list

# function to return the quote depending of the day, noted d
last_quote <- function(l,d){
    # we print NA if day number is > at historical quotes
    if (d > length(l)) {
        return(rep(NA,d))
    } else {
    return(l[(length(l) -d + 1):length(l)])
    }
}

# function to extract first and last element of a list 
extract_first_last <- function(x) {
    if (length(x) >= 2) {
        c(x[1], x[length(x)])
    } else {
        x
    }
}


# we will first work with lists
# !! We must include a test returning NA if historic quotes are not long enough
lasd <- lapply(open.price.list, function(l) last_quote(l, 0))





# 7 days
last7d <- lapply(open.price.list, function(l) last_quote(l, 7))
last7d.first.last <- lapply(last7d, function(x) extract_first_last(x))
last7d.return <- map(last7d.first.last, ~ ((.x[2] - .x[1]) / .x[1]) *100)
last7d.return <- lapply(last7d.return, function(x) round(x, digits = 2))
# transform the list in a tibble
last7d.return <- enframe(last7d.return, name = 'Assets', value = 'RoR')
# change the numeric column class
last7d.return$RoR <- unlist(last7d.return$RoR)


# 30 days
last30d <- lapply(open.price.list, function(l) last_quote(l, 30))
last30d.first.last <- lapply(last30d, function(x) extract_first_last(x))
last30d.return <- map(last30d.first.last, ~ ((.x[2] - .x[1]) / .x[1]) *100)
last30d.return <- lapply(last30d.return, function(x) round(x, digits = 2))
last30d.return <- enframe(last30d.return, name = 'Assets', value = 'RoR')
last30d.return$RoR <- unlist(last30d.return$RoR)

# 90 days
last90d <- lapply(open.price.list, function(l) last_quote(l, 90))
last90d.first.last <- lapply(last90d, function(x) extract_first_last(x))
last90d.return <- map(last90d.first.last, ~ ((.x[2] - .x[1]) / .x[1]) *100)
last90d.return <- lapply(last90d.return, function(x) round(x, digits = 2))
last90d.return <- enframe(last90d.return, name = 'Assets', value = 'RoR')
last90d.return$RoR <- unlist(last90d.return$RoR)

# join the three tibbles
rate_of_return1 <- left_join(last7d.return, last30d.return, by = 'Assets')
rate_of_return <- left_join(rate_of_return1, last90d.return, by = 'Assets')

# change col names
rate_of_return <- rate_of_return %>% rename(Return.7day = RoR.x, Return.30days = RoR.y, Return.90days = RoR)

return_percentage <- function(x,y){
    (y - x) / x * 100
}
 
# volatility
# 1 get daily returns or
# get log returns with asset prices diff(log(asset_prices))

# 2 get standard deviation sd()