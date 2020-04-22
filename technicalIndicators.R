library(TTR)
source("utils.R") 
source('settings.R')
source("rsiFunction.R")
Sys.setenv(TZ = "UTC")

stock3DayPeriod <- function( stock ){
    period.apply(CAT,endpoints(stock,on='days', k=4 ),
                 FUN=function(x) { max(Cl(x)) } ) 
}
stockWeeklyHighs <- function(stock){
    return( Hi( to.weekly(stock) ) )
}

stockMonthlyHighs <- function(stock){
    return( Hi( to.monthly(stock) ) )
}

stockWeeklyLows <- function(stock){
    return( Lo( to.weekly(stock) ) )
}

stockMonthlyLows <- function(stock){
    return( Lo( to.monthly(stock) ) )
}

lagDiff <- function( vect1, vect2, lag=1 ){
    diffV <- c() 
    for(i in 2:nrow( vect1 ) ){
        diffV[i] = sign(vect1[i]) - sign(vect2[i-lag]) 
    }
    return(diffV) 
}

buyLargePriceDiff <- function( stock, threshold=0.005 ){
    price <- Cl(stock) # close price
    r <- price/Lag(price) - 1 # % price change
    delta <- threshold #threshold
    signal <-c(0) # first date has no signal
    #Loop over all trading days (except the first)
    for (i in 2: length(price)){
        if (r[i] > delta){
            signal[i]<- 1
        } else
            signal[i]<- 0
    }
    print(signal)
    print(price)
    signal<-reclass(signal,price)
    return(signal)
}
    
stockMACD <- function( stock, fast=12, 
                          slow=26, sig = 9 ){
    macd <- MACD(Cl(stock), nFast=fast, nSlow=slow,
             nSig=sig, percent=FALSE)
    return(na.omit(macd)) 
}
stockSmaRSI <- function( stock, pastDays=14 ){
    rsi <- RSI(Cl(stock), SMA, n=pastDays)
    return( na.omit(rsi)) 
}
stockEmaRSI <- function( stock, pastDays=14 ){
    rsi <- RSI(Cl(stock), EMA, n=pastDays)
    return( na.omit(rsi)) 
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

indicatorMACD <- function( macdDF ){
    #columns macd => price(volume) oscillator
    # sdignal => oscillator signal (movingAvg of the oscillator ) 
    buyDate <- macdDF[ macdDF$macd > macdDF$signal, 'Date']
    sellDate <- macdDF[ macdDF$macd < macdDF$signal, 'Date']
  
    class(buyDate) <- "Date"
    class(sellDate) <- "Date" 
    crossDate <- list( "buy" = buyDate, "sell" = sellDate )   
    return(crossDate)
}

indicatorRSI <- function( rsiDF ){
    buyDate <- c()
    sellDate <- c() 
    buyIndex = 0
    sellIndex = 0
    betweenLimits = TRUE 
    if( first(rsiDF$signal) == 1 )
        betweenLimits = FALSE 
    else if ( first(rsiDF$signal)== -1 )
        betweenLimits = FALSE 
    for( i in 2:nrow(rsiDF) ){
        if( rsiDF$signal[i] == 1 && betweenLimits){
            buyIndex = buyIndex + 1 
            buyDate[buyIndex] <- rsiDF$Date[i] - timeLagConstant 
            betweenLimits = FALSE 
        } else if( rsiDF$signal[i] == -1 && betweenLimits){
            sellIndex = sellIndex + 1 
            sellDate[ sellIndex ] <- rsiDF$Date[i] - timeLagConstant
            betweenLimits = FALSE 
        } else if( rsiDF$signal[i] == 0 ){
            betweenLimits = TRUE 
        }
    }

    class(sellDate) <- "Date"
    class(buyDate) <- "Date" 
    crossDate <- list( "buy" = buyDate, "sell" = sellDate ) 
    return(crossDate ) 
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


