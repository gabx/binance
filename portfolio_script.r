# get MLC portfolio balance
# git URL: https://github.com/gabx/binance.git

source('get_balance.r')
source('get_historic.r')
source('get_portfolio.r')

mlc <- get_balance()
open.price <- get_historic()
portfolio <- get_portfolio()

# rename portfolio with today date
today_date <- format(Sys.Date(), "%Y%m%d")
new_name <- paste0('portfolio_', today_date)
# save it in a new env
.PortfolioEnv <- new.env()
assign(new_name, portfolio, envir = .PortfolioEnv)

# set_names(c("foo", "bar")) |> purr::map_chr(paste0, ":suffix")
# export to pdf
library(gridExtra)
pdf('portoflio_20230728.pdf')
grid.table(portfolio_20230728)
dev.off()
