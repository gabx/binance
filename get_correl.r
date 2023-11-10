# get_correl function
# return corelation matrix

get_correl <- function() {
    
    # we need to remove all NA to compare our various assets
    list.length <- sapply(asset.price.xts, length)
    length.shortest <- min(list.length)
    row.number <- nrow(asset.return.xts) - length.shortest +2
    asset.return.no_na.xts <- asset.return.xts[row.number:nrow(asset.return.xts), ]
    assign('asset.return.no_na.xts',asset.return.no_na.xts, envir = .GlobalEnv)
}