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
                    LS.delta * candle.stick.length <= lower.shadow.length ) %>% 
    filter( hammer == TRUE )

  return(candle)
}
#' Determine inverted harmer pattern using a OHLC price series
#' @param x OHLC prices.
#' @param US.delta sensitivity parameter for upper shadow
#' @param LS.delta sensitivity parameter for lower shadow
#' @return TRUE if inverted hammer pattern detected
#US.delta * candle.stick.length <= upper.shadow.length &
#                    max( upper.shadow.length, lower.shadow.length ) >= 2*candle.body.length &
#                    LS.delta * candle.stick.length >= lower.shadow.length ) %>% 

InvertedHammer <- function(stockTbbl, US.delta = 0.75, LS.delta=0.25, CL.delta=0.25) {
  candle <- 
    stockTbbl %>% 
    CandleStickLength() %>% 
    UpperShadowLength() %>% 
    LowerShadowLength() %>%
    CandleBodyLength() %>% 
    mutate( inverted.hammer = lower.shadow.length <= LS.delta*candle.stick.length &
                              upper.shadow.length >= US.delta*candle.stick.length &
                              candle.body.length <= CL.delta*candle.stick.length ) %>%  
    filter( inverted.hammer == TRUE ) 
  return(candle)
}


