

#' Lower shadow length is the differernce between the low price and the minimium of open and close price.
#' @param x OHLC prices.
#' @return length of lower shadow;
LowerShadowLength <- function(x) {
  LS <- CandleBodyBottom(x) - quantmod::Lo(x)
  result <- xts::reclass(LS, x)
  colnames(result) <- "LowerShadowLength"
  return(result)
}

#' Determine upper shadow length using a OHLC price series
#' @param x OHLC prices.
#' @return length of upper shadow
UpperShadowLength <- function(x) {
  US <- quantmod::Hi(x) - CandleBodyTop(x)
  result <- xts::reclass(US, x)
  colnames(result) <- "UpperShadowLength"
  return(result)
}


