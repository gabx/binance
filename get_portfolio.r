# get MLC portfolio balance
# git URL: https://github.com/gabx/binance.git


# for further use, we run: portfolio <- get_portfolio()
get_portfolio <- function() {

    # start_time  <- format(Sys.Date()-2, "%Y-%m-%d")
    end_time <- format(Sys.Date()-1, "%Y-%m-%d")
    
    # get a asset/value tibble with open price. 
    # we first get a list of numeric value
    open.price.list <- lapply(open.price, function(x) pull(x,open))
    
    # here we keep last daily value
    # Function to get the last element of a list
    get_last_element <- function(x) {
        if (length(x) > 0) {
            return(x[length(x)])
        } else {
            return(NA)
        }
    }
    open.price.list.last <- map_dbl(open.price.list, get_last_element)
    # transform list into tibble
    open.price.list.last <- open.price.list.last %>% map_dfr(~ .x %>% as_tibble(), .id = 'asset')

    # create final tibble
    # we keep USDT row and replace NA by 1 when joining for value of USDT
    balance.final <- left_join(mlc$balance, open.price.list.last, by = 'asset') %>%
        replace_na(list(value = 1))
    balance.final <- rename(balance.final, 'amount' = total)
    balance.final <- mutate(balance.final, 'total' = amount * value)

    ## add a weight column
    sum.asset <- sum(balance.final %>% pull(total))
    balance <- mutate(balance.final, weight = total / sum.asset)
    balance <- mutate(balance, weight = round(weight * 100, digits = 2))
    
    # order by weight
    balance <- arrange(balance, desc(weight))
    # round numbers
    balance <-  mutate(balance, total = round(total, 2))
    # rename portfolio with yesterday date
    yesterday_date <- gsub('-', '', end_time)
    new_name <- paste0('portfolio_', yesterday_date)
    # save it in a new env
    # list new env data: ls(envir = .PortfolioEnv) or ls.str(.PortfolioEnv)
    # get(ls(envir = .PortfolioEnv, pattern = '27'))
    assign(new_name, balance, envir = .PortfolioEnv)
    assign(new_name, balance, envir = .GlobalEnv)
}


