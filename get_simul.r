# return a plot with return/volatility 

library(ggthemes)
library('binancer')

portfolio.simul <- read_csv("simul.csv", col_names = 'asset')
portfolio.simul.usd <- portfolio.simul %>% 
    mutate(asset = paste(asset, 'USDT', sep = ''))

asset.price <- lapply(portfolio.simul.usd$asset, function(x) binance_klines(
    x, interval = '1d', limit = 365
))
close.price <- lapply(asset.price, function(x) pull(x,close))

# function to extract first and last element of a list 
extract_first_last <- function(x) {
    if (length(x) >= 2) {
        c(x[1], x[length(x)])
    } else {
        x
    }
}

# returns
close.first.last <- lapply(close.price, function(x) extract_first_last(x))
return <- map(close.first.last, ~ ((.x[2] - .x[1]) / .x[1]) *100)
return <- lapply(return, function(x) round(x, digits = 2))
names(return) <- portfolio.simul$asset
return <- enframe(return, name = 'asset', value = 'Return')
return$Return <- unlist(return$Return)

# volatility
log_returns <- lapply(close.price, function(x) diff(log(x)))
volat <- lapply(log_returns, function(x) round(sd(x) * 100, digits = 2))
names(volat) <- portfolio.simul$asset
volat <- enframe(volat, name = 'asset', value = 'Volatility')
volat$Volatility <- unlist(volat$Volatility)

# join
vol_ror <- left_join(return, volat, by = 'asset')

# plot
# library(RColorBrewer)
# display.brewer.all(colorblindFriendly = TRUE)
colourCount = length(vol_ror$asset)
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
my.color <- c('#EE8EBF','#0F4432','#2E946F','#78C66E','#3D5D78','#D58C50','#25DDBF',
              '#CB88B1','#5B5070','#AA5A74','#8790C1','#54ADD1','#A578A7','#87A04F',
              '#078989','#52A98B','#6AF2A5','#B89B44','#DF8052','#BB77A5','#22DFEA',
              '#E4F261','#F07A8E','#AEA33C','#874339','#93D383','#A3C455','#617197',
              '#092129','#C1CD51','#C0E463')



ggplot(vol_ror, aes(x = Volatility, y = Return)) +
    geom_point(aes(color = asset)) +
    labs(title = 'Portfolio simulation',
         x = 'Volatility', y = '365 day rate of return') +
    geom_smooth(method = 'lm', se = FALSE) +
    scale_color_manual(values = my.color[1:length(vol_ror$asset)])


#### TEST ###
ggplot(data = vol_ror, mapping = aes(x = Volatility, y = Return)) + geom_point(mapping = aes(color = asset)) +
    geom_smooth(method = 'lm') +
    labs(title = 'Volatility vs Return',
         x = 'Volatility', y = '365 day rate of return',
         color = 'asset') +
    scale_color_manual(values = my.color)





ggplot(data = vol_ror, mapping = aes(x = Volatility, y = Return)) + 
    geom_point(aes(color = asset)) +
    geom_smooth(method = 'lm') 


    scale_color_gradientn(colours = rainbow(26))


    labs(title = 'Portfolio simulation',
         x = 'Daily volatility', y = '365 day rate of return',
         color = 'asset') 




    scale_fill_brewer(palette = "Dark2")

    scale_color_gradient(low = 'greenyellow', high = 'forestgreen')

    scale_color_colorblind() 

#   scale_fill_brewer(palette = "Set3")
