#' @param x OHLC prices.
#' @return length of candle body
CandleBodyLength <- function(stockTbbl) {
  candle <- 
    stockTbbl %>% 
    mutate( candle.body.length = abs( open - close ) ) 
  return(candle ) 
}

#' @param x OHLC tibble.
#' @return length of candle body
CandleStickLength <- function(stockTbbl) {
  candle <-
    stockTbbl %>% 
    mutate( candle.stick.length = high - low ) 
  return( candle ) 
}


#' @param x OHLC tibble.
#' @return top of candle body
CandleBodyTop <- function(stockTbbl) {
  candle <-
    stockTbbl %>% 
    mutate( candle.body.top = pmax( open, close ) ) 
  return(candle)
}

#' @param x OHLC prices.
#' @return bottom of candle body
CandleBodyBottom <- function(stockTbbl) {
  candle <- 
    stockTbbl %>% 
    mutate( candle.body.bottom = pmin( open, close ) ) 
  return(candle)
}

#' @param x OHLC prices.
#' @return center of candle body
CandleBodyCenter <- function(stockTbbl) {
  candle <- 
    stockTbbl %>% 
    mutate( candle.body.center = (open+close)/2 ) 

  return( candle ) 
}


