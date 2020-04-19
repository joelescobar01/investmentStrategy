library(TTR)
source("utils.R") 

#we can use SMA() from TTR package 
indicatorSMA <- function(price, n){
    sma <- c()
    sma[1:(n-1)] <- NA
    for( i in n:length(price)){
        sma[i] <- mean( price[(i-n+1):i])
    }
    sma <- reclass(sma, price)
    return(sma)                                                             
}
          
indicatorMACD <- function( stock, fast=12, 
                          slow=26, sig = 9 ){
    mcad <- MACD( stock, nFast=fast, nSlow=slow, nSig=sig )
    #columns macd => price(volume) oscillator
        # signal => oscillator signal (movingAvg of the oscillator ) 

    return(mcad)
}

dailyCheckMACD <- function( stock, mcad, from=as.Date(Sys.Date()-5) ){
    crossOver <- c() 

}

dailyCandleBodySize <- function( stockClose, stockOpen ){
    bodySize <- c()
    min = length(stockClose)
    for(t in 1:min){
        bodySize[t] <- abs(stockOpen[t] - stockClose[t])
    }  
   return(bodySize) 
}

dailyCandleSize <- function( stockLow, stockHigh ){
    candleSize <- c() 
    min = length(stockLow) 
    for(t in 1:min){
        candleSize[t] <- stockHigh[t] - stockLow[t] 
    }
    return(candleSize) 
}


