

#' Lower shadow length is the differernce between the low price and the minimium of open and close price.
#' @param x OHLC prices.
#' @return length of lower shadow;
LowerShadowLength <- function(stockTbbl) {
  LS <- 
    CandleBodyBottom(stockTbbl) %>% 
    mutate( lower.shadow.length = candle.body.bottom - low )  
  return(LS)
}
#' Determine upper shadow length using a OHLC price series
#' @param x OHLC prices.
#' @return length of upper shadow
UpperShadowLength <- function(stockTbbl) {
  US <- 
    CandleBodyTop( stockTbbl) %>% 
    mutate( upper.shadow.length = high - candle.body.top ) 
  return(US)
}


