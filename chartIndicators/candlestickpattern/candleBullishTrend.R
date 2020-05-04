#' Determine bullish candle using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if bullish candle detected
bullish.candle <- function(x) {
  OP <- quantmod::Op(x)
  Cl <- quantmod::Cl(x)

  result <- xts::reclass(OP < Cl, x)
  colnames(result) <- "bullish candle"
  return(result)
}

#' Determine bullish engulfing pattern using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if hammer pattern detected
bullish.engulf <- function(x) {
  BT <- CandleBodyTop(x)
  BB <- CandleBodyBottom(x)

  Lag.BT <- quantmod::Lag(BT)
  Lag.BB <- quantmod::Lag(BB)

  U <- bullish.candle(x)
  D <- bearish.candle(x)

  Lag.D <- quantmod::Lag(D)

  result <- xts::reclass(U  &
                    Lag.D &
                    BT >= Lag.BT &
                    BB <= Lag.BB, x)
  colnames(result) <- "bullish engulfing"
  return(result)
}

#' Determine bullish harami pattern using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if bullish harami  pattern detected
bullish.harami <- function(x) {
  BT <- CandleBodyTop(x)
  BB <- CandleBodyBottom(x)

  Lag.BT <- quantmod::Lag(BT)
  Lag.BB <- quantmod::Lag(BB)

  U <- bullish.candle(x)
  D <- bearish.candle(x)

  Lag.D <- quantmod::Lag(D)

  result <- xts::reclass(U  &
                      Lag.D &
                      BT <= Lag.BT &
                      BB >= Lag.BB, x)
  colnames(result) <- "bullish harami"
  return(result)
}
