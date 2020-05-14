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
long.candle2 <- function(stockTbbl, n=20, delta=1) {
  candle.med <- 
    CandleBodyLength(stockTbbl) %>% 
    tq_transmute( select= candle.body.length, 
                  mutate_fun = runMedian, 
                  n=n ) 
  
  

  BL.Median <- TTR::runMedian(BL,n)
  result <- xts::reclass(BL >= BL.Median * delta, x)
  colnames(result) <- "long candle"
  return(result)
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
