
#' @param x OHLC prices.
#' @return bottom of candle body
CandleBodyBottom <- function(x) {
  BT <- pmin(quantmod::Op(x), quantmod::Cl(x))
  result <- xts::reclass(BT, x)
  colnames(result) <- "CandleBodyBottom"
  return(result)
}

#' @param x OHLC prices.
#' @return center of candle body
CandleBodyCenter <- function(x) {
  BC <- (quantmod::Op(x) + quantmod::Cl(x))/2
  result <- xts::reclass(BC, x)
  colnames(result) <- "CandleBodyCenter"
  return(result)
}

#' @param x OHLC prices.
#' @return length of candle body
CandleBodyLength <- function(x) {
  BL <- abs(quantmod::Op(x) - quantmod::Cl(x))
  result <- xts::reclass(BL, x)
  colnames(result) <- "CandleBodyLength"
  return(result)
}


#' @param x OHLC prices.
#' @return top of candle body
CandleBodyTop <- function(x) {
  BT <- pmax(quantmod::Op(x), quantmod::Cl(x))
  result <- xts::reclass(BT, x)
  colnames(result) <- "CandleBodyTop"
  return(result)
}

#' @param x OHLC prices.
#' @return TRUE if Dragon Doji pattern detected
CandleStickLength <- function(x) {
  WC <- quantmod::Hi(x) - quantmod::Lo(x)
  result <- xts::reclass(WC, x)
  colnames(result) <- "CandleStickLength"
  return(result)
}
