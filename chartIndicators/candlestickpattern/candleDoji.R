#' Determine Doji Pattern using a OHLC price series
#' @param x OHLC prices.
#' @param delta sensitivity parameter
#' @return TRUE if Doji pattern detected
#' @export
Doji <- function(stockTbbl, delta = 0.1) {
  candle <- 
    stockTbbl %>% 
    CandleStickLength() %>% 
    CandleBodyLength() %>% 
    mutate( doji = delta*candle.stick.length >= candle.body.length ) 
  return(candle)
}
#' Determine Dragon Doji Pattern using a OHLC price series
#' @param x OHLC prices.
#' @param delta Sensivity
#' @return TRUE if Dragon Doji pattern detected
DragonflyDoji <- function(stockTbbl, delta=0.1) {
  candle <- 
    stockTbbl %>% 
    CandleStickLength() %>% 
    UpperShadowLength() %>% 
    Doji() %>%
    mutate( dragonfly.doji = delta*candle.stick.length >= upper.shadow.length & doji ) 
  return(candle)
}


#' Determine Gravestoen Doji Pattern using a OHLC price series
#' @param x OHLC prices.
#' @param delta Sensivity
#' @return TRUE if Doji pattern detected
GravestoneDoji <- function(stockTbbl,delta=0.1) {
  candle <- 
    stockTbbl %>% 
    CandleStickLength() %>% 
    LowerShadowLength() %>% 
    Doji() %>%
    mutate( gravestone.doji = delta*candle.stick.length >= lower.shadow.length & doji ) 
  return(candle)
}
