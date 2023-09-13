# get portfolio stats on 7, 30 and 90 days

get_return <- function() {
# we start with our list of tb open.price.list
open.price.list <- lapply(open.price, function(x) pull(x,open))

# function to return the quotes depending of the day, noted d
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
lasd <- lapply(open.price.list, function(l) last_quote(l, 0))


# 7 days
last7d <- lapply(open.price.list, function(l) last_quote(l, 7))
last7d.first.last <- lapply(last7d, function(x) extract_first_last(x))
last7d.return <- map(last7d.first.last, ~ ((.x[2] - .x[1]) / .x[1]) *100)
last7d.return <- lapply(last7d.return, function(x) round(x, digits = 2))
# transform the list in a tibble
last7d.return <- enframe(last7d.return, name = 'asset', value = 'RoR')
# change the numeric column class
last7d.return$RoR <- unlist(last7d.return$RoR)

# 30 days
last30d <- lapply(open.price.list, function(l) last_quote(l, 30))
last30d.first.last <- lapply(last30d, function(x) extract_first_last(x))
last30d.return <- map(last30d.first.last, ~ ((.x[2] - .x[1]) / .x[1]) *100)
last30d.return <- lapply(last30d.return, function(x) round(x, digits = 2))
last30d.return <- enframe(last30d.return, name = 'asset', value = 'RoR')
last30d.return$RoR <- unlist(last30d.return$RoR)
# volatility
log_returns30d <- lapply(last30d, function(x) diff(log(x)))
volat30d <- lapply(log_returns30d, function(x) round(sd(x) * 100, digits = 2))
volat30d <- enframe(volat30d, name = 'asset', value = 'Vol')
volat30d$Vol <- unlist(volat30d$Vol)

# 90 days
last90d <- lapply(open.price.list, function(l) last_quote(l, 90))
last90d.first.last <- lapply(last90d, function(x) extract_first_last(x))
last90d.return <- map(last90d.first.last, ~ ((.x[2] - .x[1]) / .x[1]) *100)
last90d.return <- lapply(last90d.return, function(x) round(x, digits = 2))
last90d.return <- enframe(last90d.return, name = 'asset', value = 'RoR')
last90d.return$RoR <- unlist(last90d.return$RoR)
# volatility
log_returns90d <- lapply(last90d, function(x) diff(log(x)))
volat90d <- lapply(log_returns90d, function(x) round(sd(x) * 100, digits = 2))
volat90d <- enframe(volat90d, name = 'asset', value = 'Vol')
volat90d$Vol <- unlist(volat90d$Vol)


# join the three tibbles
rate_of_return1 <- left_join(last7d.return, last30d.return, by = 'asset')
rate_of_return <- left_join(rate_of_return1, last90d.return, by = 'asset')
# join with our portfolio
rate_of_return <- left_join(portfolio, rate_of_return, by = 'asset')
rate_of_return <- rate_of_return %>% rename(Return.7day = RoR.x, Return.30days = RoR.y, Return.90days = RoR)
rate_of_return <- rate_of_return %>% mutate(total = round(total, digits = 2))
# join with the volatility
my_portfolio1 <- left_join(rate_of_return, volat30d, by = 'asset')
mlc_portfolio <- left_join(my_portfolio1, volat90d, by = 'asset')
mlc_portfolio <- mlc_portfolio %>% rename(Vol.30days = Vol.x, Vol.90days = Vol.y)


# return(my_portfolio)

# ! Missing is adding a line with our USDT
}
 

