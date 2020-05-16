#' Determine long candle using a OHLC price series
#' @param x OHLC prices.
#' @param n period
#' @param delta sensitivity parameter
#' @return TRUE if long candle detected
long.candle <- function(x, n=20, delta=1) {
  BL <- CandleBodyLength(x)
  BL.Median <- TTR::runMedian(BL,n)

  result <- xts::reclass(BL >= BL.Median * delta, x)
  colnames(result) <- "long candle"
  return(result)
}

#' Determine long candle using a OHLC price series
#' @param x OHLC prices.
#' @param n period
#' @param delta sensitivity parameter
#' @return TRUE if long candle detected
LongCandle <- function(stockTbbl, n=10, delta=3) {
  candle <- 
    CandleBodyLength(stockTbbl) %>% 
    tq_mutate(  select= candle.body.length, 
                  mutate_fun = runMedian, 
                  n=n,
                  col_rename="n.median") %>%
    mutate( long.candle = candle.body.length >= n.median*delta ) 
  return(candle)
}
#' Determine short candle using a OHLC price series
#' @param x OHLC prices.
#' @param n period
#' @param delta sensitivity parameter
#' @return TRUE if short candle detected
short.candle <- function(x, n=20, delta=1) {
  BL <- CandleBodyLength(x)
  BL.Median <- TTR::runMedian(BL,n=n)

  result <- xts::reclass(BL <= BL.Median * delta, x)
  colnames(result) <- "long candle"
  return(result)
}

#' Determine short candle using a OHLC price series
#' @param x OHLC prices.
#' @param n period
#' @param delta sensitivity parameter
#' @return TRUE if short candle detected
ShortCandle <- function(stockTbbl, n=10, delta=3) {
  candle <- 
    CandleBodyLength(stockTbbl) %>% 
    tq_mutate(  select= candle.body.length, 
                  mutate_fun = runMedian, 
                  n=n,
                  col_rename="n.median") %>%
    mutate( short.candle = candle.body.length <= n.median*delta ) 
  return(candle)
}
#' Determine short shadow using a OHLC price series
#' @param x OHLC prices.
#' @param n period
#' @param delta sensitivity parameter
#' @return TRUE if short candle detected
short.upper.shadow <- function(x, n=20, delta=1) {
  BL <- CandleBodyLength(x)
  SL <- UpperShadowLength(x)
  BL.Median <- TTR::runMedian(BL,n=n)

  result <- xts::reclass(SL <= BL.Median * delta, x)
  colnames(result) <- "long upper shadow "
  return(result)
}

#' Determine long shadow using a OHLC price series
#' @param x OHLC prices.
#' @param n period
#' @param delta sensitivity parameter
#' @return TRUE if short candle detected
long.upper.shadow <- function(x, n=20, delta=1) {
  BL <- CandleBodyLength(x)
  SL <- UpperShadowLength(x)
  BL.Median <- TTR::runMedian(BL,n=n)

  result <- xts::reclass(SL <= BL.Median * delta, x)
  colnames(result) <- "long upper shadow "
  return(result)
}
