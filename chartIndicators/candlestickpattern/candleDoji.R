#' Determine Doji Pattern using a OHLC price series
#' @param x OHLC prices.
#' @param delta sensitivity parameter
#' @return TRUE if Doji pattern detected
#' @export
doji <- function(x, delta = 0.1) {
  WC <- CandleStickLength(x)
  BL <- CandleBodyLength(x)
  result <- xts::reclass(delta * WC >= BL, x)
  colnames(result) <- "doji"
  return(result)
}

#' Determine Dragon Doji Pattern using a OHLC price series
#' @param x OHLC prices.
#' @param delta Sensivity
#' @return TRUE if Dragon Doji pattern detected
dragonfly.doji <- function(x, delta=0.1) {
  WC <- CandleStickLength(x)
  US <- UpperShadowLength(x)

  result <- xts::reclass(delta * WC >= US & doji(x,delta), x)
  colnames(result) <- "dragonfly doji"
  return(result)
}

#' Determine Gravestoen Doji Pattern using a OHLC price series
#' @param x OHLC prices.
#' @param delta Sensivity
#' @return TRUE if Doji pattern detected
gravestone.doji <- function(x,delta=0.1) {
  WC <- CandleStickLength(x)
  LS <- LowerShadowLength(x)

  result <- xts::reclass(delta * WC >= LS & doji(x,delta), x)
  colnames(result) <- "gravestone.doji"
  return(result)
}
