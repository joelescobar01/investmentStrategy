#' Determine bearish candle using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if bullish candel detected
bearish.candle <- function(x) {
  OP <- quantmod::Op(x)
  CL <- quantmod::Cl(x)

  result <- xts::reclass(OP > CL, x)
  colnames(result) <- "bearish candle"
  return(result)
}

#' Determine bearish engulfing pattern using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if bearish engulfing pattern detected
bearish.engulf <- function(x) {
  BT <- CandleBodyTop(x)
  BB <- CandleBodyBottom(x)

  Lag.BT <- quantmod::Lag(BT)
  Lag.BB <- quantmod::Lag(BB)

  U <- bullish.candle(x)
  D <- bearish.candle(x)

  Lag.U <- quantmod::Lag(U)

  result <- xts::reclass(D  &
                    Lag.U &
                    BT >= Lag.BT &
                    BB <= Lag.BB, x)
  colnames(result) <- "bearish engulfing"
  return(result)
}


#' Determine bearish harami pattern using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if bearish haramipattern detected
bearish.harami <- function(x) {
  BT <- CandleBodyTop(x)
  BB <- CandleBodyBottom(x)

  Lag.BT <- quantmod::Lag(BT)
  Lag.BB <- quantmod::Lag(BB)

  U <- bullish.candle(x)
  D <- bearish.candle(x)

  Lag.U <- quantmod::Lag(U)

  result <- xts::reclass(D  &
                      Lag.U &
                      BT <= Lag.BT &
                      BB >= Lag.BB, x)
  colnames(result) <- "bearish harami"
  return(result)
}
