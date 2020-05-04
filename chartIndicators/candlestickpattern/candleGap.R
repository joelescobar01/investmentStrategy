#' Determine gap down using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if bullish candel detected
gap.down <- function(x) {
  OP <- quantmod::Op(x)
  Cl <- quantmod::Cl(x)
  Lag.Cl <- quantmod::Lag(Cl)
  Lag.OP <- quantmod::Lag(OP)

  GAP <- pmax(OP,Cl) - pmin(Lag.Cl,Lag.OP)

  result <- xts::reclass(GAP<0, x)
  colnames(result) <- "Gap down"
  return(result)
}

#' Determine gap up using a OHLC price series
#' @param x OHLC prices.
#' @return TRUE if bullish candel detected
gap.up <- function(x) {
  OP <- quantmod::Op(x)
  Cl <- quantmod::Cl(x)
  Lag.Cl <- quantmod::Lag(Cl)
  Lag.OP <- quantmod::Lag(OP)

  GAP <- pmin(OP,Cl) - pmax(Lag.Cl,Lag.OP)

  result <- xts::reclass(GAP>0, x)
  colnames(result) <- "Gap up"
  return(result)
}
