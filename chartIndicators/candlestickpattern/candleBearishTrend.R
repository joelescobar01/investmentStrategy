#' Determine bearish candle using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if bullish candel detected
BearishCandle <- function(stockTbbl) {
  candle <- 
    stockTbbl %>% 
    mutate( bearish.candle = open > close ) 

  return( candle ) 
}
#' Determine bearish engulfing pattern using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if bearish engulfing pattern detected
BearishEngulf <- function(stockTbbl) {
  candle <-
    stock %>% 
    CandleBodyTop( ) %>% 
    CandleBodyBottom() %>% 
    BearishCandle() %>% 
    BullishCandle() %>% 
    mutate( bearish.engulfing = bearish.candle & 
                                lag(bullish.candle ) & 
                                candle.body.top >= lag( candle.body.top ) & 
                                candle.body.bottom <= lag( candle.body.bottom )) 

  return( candle ) 
}

#' Determine bearish harami pattern using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if bearish haramipattern detected

BearishHarami <- function( stockTbbl ) {
  candle <- 
    stockTbbl %>% 
    CandleBodyTop() %>% 
    CandleBodyBottom() %>% 
    BullishCandle() %>% 
    BearishCandle() %>% 
    mutate( bearish.harami =  bearish.candle & 
                              lag( bullish.candle ) & 
                              candle.body.top <= lag( candle.body.top ) &
                              candle.body.bottom >= lag( candle.body.bottom ) )
    return( candle ) 
}
