library(TTR)
source("utils.R") 
source('settings.R')
Sys.setenv(TZ = "UTC")

stockMACD <- function( stock, fast=12, 
                          slow=26, sig = 9 ){
    macd <- MACD(Cl(stock), nFast=fast, nSlow=slow,
             nSig=sig, percent=FALSE)
    return(na.omit(macd)) 
}

stockDFMACD <- function( stockDF,fast=12, 
                          slow=26, sig = 9 ){
    stockXts <- dfToTimeSeries( stockDF ) 
    macd <- stockMACD( stockXts )
    return(macd) 
} 

indicatorSMA <- function(price, n){
    sma <- c()
    sma[1:(n-1)] <- NA
    for( i in n:length(price)){
        sma[i] <- mean( price[(i-n+1):i])
    }
    sma <- reclass(sma, price)
    return(sma)                                                             
}

indicateMACDBuy <- function( macd ){ 
    macdLine <- coredata( macd$macd ) 
    sigLine <- coredata( macd$signal ) 
    sellDate < c() 
    buyDate <- c()
    buyI = 1
    sellI = 1
    if( macdLine[1] > sigLine[1] )
        macBelow = FALSE 
    el
    for(i in length(macd) ){
        if( macdLine[i] > sigLine[i] ){
            macBelow = FALSE
            buyDate[ buyI ] = index(macd[i])  
        }
        else if( macdLine[i] < sigLine[i] ){
            sellIndex = sellIndex+1
            sellDate[ sellIndex ] <- startDate+i 
            macBelow = TRUE 
        }
    }

}

indicatorMACD <- function( macdDF ){
    #columns macd => price(volume) oscillator
    # sdignal => oscillator signal (movingAvg of the oscillator ) 
    buyDate <- c() 
    sellDate <- c()
    buyIndex = 0
    sellIndex = 0  
    macBelow = first( macdDF$macd ) < first( macdDF$signal )
    
    for(i in 1:nrow(macdDF)){
        if( macdDF$macd[i] > macdDF$signal[i] && macBelow ){
            buyIndex=buyIndex+1
            buyDate[ buyIndex ] <- macdDF$Date[i] - timeLagConstant 
            macBelow = FALSE 
        }
        else if( macdDF$macd[i] < macdDF$signal[i]  && !macBelow ){
            sellIndex = sellIndex+1
            sellDate[ sellIndex ] <- macdDF$Date[i] - timeLagConstant
            macBelow = TRUE 
        }
    }
    class(buyDate) <- "Date"
    class(sellDate) <- "Date" 
    crossDate <- list( "buy" = buyDate, "sell" = sellDate )   
    return(crossDate)
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


