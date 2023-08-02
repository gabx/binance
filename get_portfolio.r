# get MLC portfolio balance
# git URL: https://github.com/gabx/binance.git


# for further use, we run: portfolio <- get_portfolio()
get_portfolio <- function() {

    library('binancer')
    library('rjson')
    
    env.var <- fromJSON(file = 'variables.env.json')
    binance_credentials(env.var[[1]],env.var[[2]])  
    
    # get a asset/value tibble with open price. 
    # we first get a list of numeric value
    open.price.list <- lapply(open.price, function(x) pull(x,open))
    # here we keep last daily value
    open.price.list.last <- open.price.list |> map(c(length(open.price.list[[1]]),1))
    # transform list into tibble
    open.price.list.last <- open.price.list.last %>% map_dfr(~ .x %>% as_tibble(), .id = 'asset')


    # create final tibble
    balance.final <- inner_join(mlc$balance, open.price.list.last, by = 'asset')
    balance.final <- rename(balance.final, 'amount' = total)
    balance.final <- mutate(balance.final, 'total' = amount * value)

    ## add a weight column
    sum.asset <- sum(balance.final %>% pull(total))
    balance <- mutate(balance.final, weight = total / sum.asset)
    balance <- mutate(balance, weight = round(weight * 100, digits = 2))
    
    # order by weight
    balance <- arrange(balance, desc(weight))
    # round numbers
    balance <-  mutate(balance, amount = round(amount, 2), value = round(value, 2), total = round(total, 2))
    # rename portfolio with yesterday date
    yesterday_date <- gsub('-', '', end_time)
    new_name <- paste0('portfolio_', yesterday_date)
    # save it in a new env
    # list new env data: ls(envir = .PortfolioEnv) or ls.str(.PortfolioEnv)
    # rm(list = 'portfolio_20230727', envir = .PortfolioEnv)
    # get(ls(envir = .PortfolioEnv, pattern = '27'))
    # .PortfolioEnv <- new.env()
    assign(new_name, balance, envir = .PortfolioEnv)
    assign(new_name, balance, envir = .GlobalEnv)
    return(balance)
}


