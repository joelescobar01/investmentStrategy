library(TTR)
source("settings.R")
source("utils.R")

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
