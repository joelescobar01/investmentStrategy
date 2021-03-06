#' Determine bullish candle using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if bullish candle detected
BullishCandle <- function(stockTbbl) {
candle <- 
    stockTbbl %>% 
    mutate( bullish.candle = open < close )  %>% 
    filter( bullish.candle == TRUE ) 
  return( candle ) 
}
#' Determine bullish engulfing pattern using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if hammer pattern detected
BullishEngulf <- function( stockTbbl ){
  candle <-
    stock %>% 
    CandleBodyTop( ) %>% 
    CandleBodyBottom() %>% 
    BearishCandle() %>% 
    BullishCandle() %>% 
    mutate( bullish.engulfing = bullish.candle & 
                                lag(bearish.candle ) & 
                                candle.body.top >= lag( candle.body.top ) & 
                                candle.body.bottom <= lag( candle.body.bottom )) %>% 
    filter( bullish.engulfing == TRUE ) %>% 
    select( date, open, close, high, low, bullish.engulfing ) 
  return( candle ) 

}

#' Determine bullish harami pattern using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if bullish harami  pattern detected
BullishHarami <- function( stockTbbl ) {
  candle <- 
    stockTbbl %>% 
    CandleBodyTop() %>% 
    CandleBodyBottom() %>% 
    BullishCandle() %>% 
    BearishCandle() %>% 
    mutate( bullish.harami =  bullish.candle & 
                              lag( bearish.candle ) & 
                              candle.body.top <= lag( candle.body.top ) &
                              candle.body.bottom >= lag( candle.body.bottom ) ) %>% 
    select( date, open, close, high, low, bullish.harami ) %>% 
    filter( bullish.harami == TRUE ) 
    return( candle ) 
}
