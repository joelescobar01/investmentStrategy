#' @param x OHLC prices.
#' @return length of candle body
CandleBodyLength <- function(stockTbbl) {
  candle <- 
    stockTbbl %>% 
    mutate( candle.body.length = abs( open - close ) ) %>% 
    select( date, candle.body.length ) 
  return(candle ) 
}

#' @param x OHLC tibble.
#' @return length of candle body
CandleStickLength <- function(stockTbbl) {
  candle <-
    stockTbbl %>% 
    mutate( candle.stick.length = high - low ) %>% 
    select( date, candle.stick.length )
  return( candle ) 
}


#' @param x OHLC tibble.
#' @return top of candle body
CandleBodyTop <- function(stockTbbl) {
  candle <-
    stockTbbl %>% 
    mutate( candle.body.top = pmax( open, close ) ) %>% 
    select( date, candle.body.top ) 
  return(candle)
}

#' @param x OHLC prices.
#' @return bottom of candle body
CandleBodyBottom <- function(stockTbbl) {
  candle <- 
    stockTbbl %>% 
    mutate( candle.body.bottom = pmin( open, close ) ) %>% 
    select( date, candle.body.bottom ) 
  return(candle)
}

#' @param x OHLC prices.
#' @return center of candle body
CandleBodyCenter <- function(stockTbbl) {
  candle <- 
    stockTbbl %>% 
    mutate( candle.body.center = (open+close)/2 ) %>% 
    select( date, candle.body.center ) 

  return( candle ) 
}


