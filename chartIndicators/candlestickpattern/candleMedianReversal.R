#' Determine dark cloud cover pattern using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if dark cloud cover pattern detected
dark.cloud.cover<- function(x) {
  Cl <- quantmod::Cl(x)
  M <- CandleBodyCenter(x)

  Lag.M <- quantmod::Lag(M)

  U <- bullish.candle(x)
  D <- bearish.candle(x)

  Lag.U <- quantmod::Lag(U)

  result <- xts::reclass(D  &
                    Lag.U &
                    Cl <= Lag.M,
                    x)
  colnames(result) <- "dark cloud cover"
  return(result)
}

#' Determine piercing line pattern using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if hammer pattern detected
piercing.line <- function(x) {
  Cl <- quantmod::Cl(x)
  M <- CandleBodyCenter(x)

  Lag.M <- quantmod::Lag(M)

  U <- bullish.candle(x)
  D <- bearish.candle(x)

  Lag.D <- quantmod::Lag(D)

  result <- xts::reclass(U  &
                    Lag.D &
                    Cl >= Lag.M,
                    x)
  colnames(result) <- "piercing line"
  return(result)
}
