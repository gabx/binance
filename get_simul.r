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
my.color <- rainbow(26)

ggplot(vol_ror, aes(x = Volatility, y = Return)) +
    geom_point(aes(color = asset)) +
    labs(title = 'Portfolio simulation',
         x = 'Daily volatility', y = '365 day rate of return') +
    geom_smooth(method = 'lm') 



#### TEST ###
ggplot(data = vol_ror, mapping = aes(x = Volatility, y = Return)) + geom_point(mapping = aes(color = asset)) +
    geom_smooth(method = 'lm') +
    labs(title = 'Volatility vs Return',
         x = 'Volatility', y = '365 day rate of return',
         color = 'asset') +
    scale_color_manual(values = my.color)



#2D212D,#312734,#352D3B,#373343,#3A3A4A,#3B4051,#3D4759,#3D4E5F,#3D5666,#3D5D6C,#3D6572,#3C6C77,#3C747C,#3C7B80,#3D8383,#3F8B86,#419288,#469A89,#4BA18A,#52A98B,#5AB08A,
#64B78A,#6EBF88,#7AC587,#86CC85,#93D383,#A1D981,#B0DF7F,#BFE57D,#CFEB7C,#E0F07B

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
