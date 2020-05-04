#' Determine kicking down pattern using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if kicking down pattern detected
kick.down <- function(x) {
  U <- bullish.candle(x)
  D <- bearish.candle(x)
  GD <- gap.down(x)

  Lag.U <- quantmod::Lag(U)

  result <- xts::reclass(D  &
                    Lag.U &
                    GD,
                    x)
  colnames(result) <- "kick down"
  return(result)
}

#' Determine kicking up pattern using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if kicking up pattern detected
kick.up <- function(x) {
  U <- bullish.candle(x)
  D <- bearish.candle(x)
  GU <- gap.up(x)

  Lag.D <- quantmod::Lag(D)

  result <- xts::reclass(U  &
                    Lag.D &
                    GU ,
                    x)
  colnames(result) <- "kick up"
  return(result)
}
