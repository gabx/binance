# binance

work with Binance to get account balance, historical data and statistics

## get_balance.r

1.  get_balance()

This function returns the daily balance of Binance account in the form of tibble. It has to be run first this way, as we will need further the data: mlc \<- get_balance()

2.  get_historic()

This function returns a list of tibbles, each one being the daily historical data, at 6:00 PM, for a token of our portfolio. Run it in second this way: open.price \<- get_historic()

3.  get_portfolio()

This function returns our final daily portfolio. Run it last this way: portfolio \<- get_portfolio()
