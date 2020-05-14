#' Determine hammer pattern using a OHLC price series
#' @param x OHLC prices.
#' @param US.delta sensitivity parameter for upper shadow
#' @param LS.delta sensitivity parameter for lower shadow
#' @return TRUE if hammer pattern detected
Hammer <- function(stockTbbl, US.delta = 0.1, LS.delta=0.7) {
  candle <- 
    stockTbbl %>% 
    CandleStickLength() %>% 
    UpperShadowLength() %>% 
    LowerShadowLength() %>% 
    mutate( hammer = US.delta * candle.stick.length >= upper.shadow.length &
                    LS.delta * candle.stick.length <= lower.shadow.length ) 
  return(candle)
}
#' Determine inverted harmer pattern using a OHLC price series
#' @param x OHLC prices.
#' @param US.delta sensitivity parameter for upper shadow
#' @param LS.delta sensitivity parameter for lower shadow
#' @return TRUE if inverted hammer pattern detected
InvertedHammer <- function(stockTbbl, US.delta = 0.1, LS.delta=0.7) {
  candle <- 
    stockTbbl %>% 
    CandleStickLength() %>% 
    UpperShadowLength() %>% 
    LowerShadowLength() %>% 
    mutate( inverted.hammer = US.delta * candle.stick.length <= upper.shadow.length &
                    LS.delta * candle.stick.length >= lower.shadow.length ) 
  return(candle)
}
