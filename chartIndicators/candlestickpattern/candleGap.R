#' Determine gap down using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if bullish candel detected
GapDown <- function(stockTbbl) {
  candle <- 
    stockTbbl %>% 
    mutate( gap.down = pmax( open, close ) - pmin( lag(close), lag(open) ))
  
  candle <- 
    candle %>% 
    mutate( gap.down = gap.down < 0 ) %>% 
    filter( gap.down == TRUE ) 
  return(candle)
}

#' Determine gap up using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if bullish candel detected
GapUp <- function( stockTbbl ){
  candle <- 
    stockTbbl %>% 
    mutate( gap.up = pmin( open, close ) - pmax( lag(close), lag(open) ))
  
  candle <- 
    candle %>% 
    mutate( gap.up = gap.up > 0 ) %>% 
    filter( gap.up == TRUE ) 
  return(candle)
}
