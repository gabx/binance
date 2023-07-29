# get MLC portfolio balance
# git URL: https://github.com/gabx/binance.git

source('get_balance.r')
source('get_historic.r')
source('get_portfolio.r')

mlc <- get_balance()
open.price <- get_historic()
portfolio <- get_portfolio()

# rename portfolio with today date
yesterday_date <- format(Sys.Date()-1, "%Y%m%d")
new_name <- paste0('portfolio_', yesterady_date)
# save it in a new env
# list new env data: ls(envir = .PortfolioEnv) or ls.str(.PortfolioEnv)
.PortfolioEnv <- new.env()
assign(new_name, portfolio, envir = .PortfolioEnv)

# set_names(c("foo", "bar")) |> purr::map_chr(paste0, ":suffix")
# export to pdf
library(gridExtra)
pdf('portoflio_20230728.pdf')
grid.table(portfolio_20230728)
dev.off()
