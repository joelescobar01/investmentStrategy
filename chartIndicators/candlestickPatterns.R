library(TTR)

source("lib/utils.R")
source("var/settings.R")

candlestickBodyFilled <- function( openPrice, closePrice ){
    return(  max( closePrice, openPrice ) == openPrice )
}
candlestickTotalLength <- function( stockHigh, stockLow ){
    return( stockHigh - stockLow ) 
} 
candlestickBodyLength <- function( stockOpen, stockClose ){
    return( abs( stockOpen - stockClose) ) 
} 
candlestickUpperShadowLength <- function( stockClose, stockOpen, stockHigh ){
    return( stockHigh - max(stockClose, stockOpen ))
}
candlestickLowerShadowLength <- function( stockClose, stockOpen, stockLow ){
    return( min(stockClose, stockOpen) - stockLow )
}
movementUp <- function(closePrice, openPrice){
    if( length(closePrice) != length(openPrice) )
        return(NA)
    U <- c()
    if (CL[t] > OP[t]){
        U[t] = 1
    } else {
        U[t] = 0
    }
    return(U)
}

movementDown <- function(closePrice, openPrice ){
    if( length(closePrice) != length(openPrice) )
        return(NA)
    D <- c()
    for (t in seq_along(length(openPrice))){
        if (CL[t] < OP[t]){
            D[t] = 1
        } else {
            D[t] = 0
        }
    }
    return(D)
}

gapUp <- function( closePrice, openPrice ){
    #Gap up: candle body Day 2 is higher than that of Day 1. 
    if( length(closePrice) != length(openPrice) )
        return(NA)
    GU <- c(0)
    t=2 
    for (t in seq_along(length(closePrice))){
        
        if (min(OP[t],CL[t])
            > max(OP[t-1],CL[t-1])){
            GU[t] = 1
        } else {
            GU[t] = 0
        }
    }
    return(GU)
}

gapDown <- function( closePrice, openPrice ){
    #Gap down: candle body Day 2 is lower than that of Day 1. 
    GD <- c(0)
    for (t in 2:N){
        if (max(OP[t],CL[t])
            > min(OP[t-1],CL[t-1])){
            GD[t] = 1
        } else {
            GD[t] = 0
        }
    }
    return(GD)
}

doji <- function( quotes ){
    delta <-0.1
    doji <- c()
    for (i in seq_along(length(quotes))){
        if (delta *candlestickTotalLength[i]>candlestickBodyLength[i]){
            doji[i] <- 1
        } else{
            doji[i] <- 0
        } 
    }
    return(doji)
}

dragonflyDoji <- function( quote ){
    delta <-0.1
    d.doji <- c()
    for (i in 1:N){
        if (delta*candlestickTotalLength[i]>candlestickUpperShadowLength[i] &&
            doji[i]==1){
            d.doji[i] <- 1
        } else{
            d.doji[i] <- 0
        } 
    }
    return(d.doji)
}

hammer <- function(quote){
    delta1 <- 0.1
    delta2 <- 0.7
    hammer <-c()
    for (i in 1:N){
        if (delta1*candlestickTotalLength[i]>candlestickUpperShadowLength[i] && delta2*candlestickTotalLength[i]<candlestickUpperShadowLength[i]){
            hammer[i] <- 1
        } else{
            hammer[i] <- 0
        } 
    }
    return(hammer)
}

invertedHammer <- function(quote){
    delta1 <- 0.1
    delta2 <- 0.7
    invertedHammer <-c()
    for (i in 1:N){
        if (delta1*candlestickTotalLength[i]<candlestickUpperShadowLength[i] && delta2*candlestickTotalLength[i]>candlestickUpperShadowLength[i]){
            invertedHammer[i] <- 1
        } else{
            invertedHammer[i] <- 0
        } 
    }
    return(invertedHammer)
}

bullishEngulfing <- function( quote ){
    # MSFT['2011-11-02/2011-11-08']
    engulf <-c(0)
    for (i in 2:N){
        if (D[i-1]>0 && 
            U[i]>0 && 
            OP[i]<=CL[i-1] &&
            CL[i]>=OP[i-1]){
            engulf[i] <- 1
        } else{
            engulf[i] <- 0
        } 
    }
    return(engulf)
}

bearishEngulfing <- function(quote){
    #MSFT['2011-12-15/2011-12-20']
    
}

bullishHarami <- function(quote){
    #MSFT['2011-12-27/2011-12-30']
    harami <-c(0)
    for (i in 2:N){
        if (D[i-1]>0 && 
            U[i]>0 && 
            OP[i]>=CL[i-1] && 
            CL[i]<=OP[i-1]){
            harami[i] <- 1
        } else{
            harami[i] <- 0
        } 
    }
    return(harami)
}

bearishHarami <- function(quote){
    #MSFT['2011-11-02/2011-11-07']
}

shadowsRatio <- function( upperShadow, lowerShadow ){
    # ratio = upperShadow/lowerShadow 
    return( upperShadow/lowerShadow ) 
}
shadowsWideRange <- function( stockHigh, stockLow ){

}
bodyCandleRatio <- function( bodyLength, candleLength ){
    # ratio = upperShadow/lowerShadow 
    return( bodyLength/candleLength ) 
}
bodyBig <- function( stockQuote ){
    bodySize = candlestickBodyLength( stockQuote$Open, stockQuote$Close )
    candleSize = candlestickTotalLength( stockQuote$High, stockQuote$Low )  
    return( bodySize/candleSize > bigBodySizeMin )  
}
bodySmall <- function( stockQuote ){
    bodySize = candlestickBodyLength( stockQuote$Open, stockQuote$Close )
    candleSize = candlestickTotalLength( stockQuote$High, stockQuote$Low )  
    print( paste( "Candle Body: ", bodySize/candleSize ))
    return( bodySize/candleSize < bigBodySizeMin )  
}
bodyDoji <- function( stockQuote, tolerance=tolerance ){
    degreeDiff = abs( 1 - stockQuote$Close/stockQuote$Open )
    
    return( degreeDiff < dojiSizeTolerance)
} 
bodyTopHeavy <- function( stockQuote ){
    upperShadow <- candlestickUpperShadowLength( data$Close, data$Open, stock$High )
    lowerShadow <- candlestickUpperShadowLength( data$Close, data$Open, stock$Low )
    ratio = upperShadow/lowerShadow 
    return( ratio < bodyPositionLimit )
}
bodyBottomHeavy <- function( stockQuote){
    upperShadow <- candlestickUpperShadowLength( data$Close, data$Open, stock$High )
    lowerShadow <- candlestickUpperShadowLength( data$Close, data$Open, stock$Low )
    ratio = lowerShadow/upperShadow  
    return( ratio < bodyPositionLimit )
}
patternBigBlack <- function( stockQuote){
    if( candlestickBodyFilled( stockQuote$Open, stockQuote$Close ) && 
       bodyBig( stockQuote ) )
        return(TRUE)
    return(FALSE) 
}
