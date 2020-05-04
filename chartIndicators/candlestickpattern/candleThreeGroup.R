#' Determine falling three pattern using a OHLC price series
#'
#' @param x OHLC prices.
#' @param n number of period of trend
#' @param delta sensitivity for long candle
#' @return TRUE if kicking up pattern detected
#' @export


falling.three <- function(x,n=20,delta=1) {
  U <- bullish.candle(x)
  D <- bearish.candle(x)

  Lag4.D <- quantmod::Lag(D,4)

  Lag.U <- quantmod::Lag(U,1)
  Lag2.U <- quantmod::Lag(U,2)
  Lag3.U <- quantmod::Lag(U,3)

  LC <- long.candle(x,n,delta)
  Lag.LC <- quantmod::Lag(LC)
  Lag4.LC <- quantmod::Lag(LC,4)

  LO <- quantmod::Lo(x)
  MIN.LO <- TTR::runMin(LO,4)
  Lag.MIN.LO <- quantmod::Lag(MIN.LO)

  HI <- quantmod::Hi(x)
  MAX.HI <- TTR::runMax(HI,n=4)
  Lag4.HI <- quantmod::Lag(HI,4)

  result <- xts::reclass(D & LC &
                      Lag.U & Lag2.U & Lag3.U &
                      Lag4.D & Lag4.LC &
                      LO < Lag.MIN.LO &
                      Lag4.HI > MAX.HI,
                    x)
  colnames(result) <- "failling three"
  return(result)
}

#' Determine rising three pattern using a OHLC price series
#' @param x OHLC prices.
#' @param n number of period of trend
#' @param delta sensitivity for long candle
#' @return TRUE if kicking up pattern detected
rising.three <- function(x,n=20,delta=1) {
  U <- bullish.candle(x)
  D <- bearish.candle(x)

  Lag4.U <- quantmod::Lag(U,4)

  Lag.D <- quantmod::Lag(D,1)
  Lag2.D <- quantmod::Lag(D,2)
  Lag3.D <- quantmod::Lag(D,3)

  LC <- long.candle(x,n,delta)
  Lag.LC <- quantmod::Lag(LC)
  Lag4.LC <- quantmod::Lag(LC,4)

  LO <- quantmod::Lo(x)
  MIN.LO <- TTR::runMin(LO,4)

  Lag4.LO <- quantmod::Lag(LO,4)

  HI <- quantmod::Hi(x)
  MAX.HI <- TTR::runMax(HI,n=4)
  Lag.MAX.HI <- quantmod::Lag(MAX.HI)

  result <- xts::reclass(U & LC &
                      Lag.D & Lag2.D & Lag3.D &
                      Lag4.U & Lag4.LC &
                      HI > Lag.MAX.HI &
                      Lag4.LO < MIN.LO,
                    x)
  colnames(result) <- "rising three"
  return(result)
}

#' Determine three black crows pattern using a OHLC price series
#' @param x OHLC prices.
#' @param n number of period of trend
#' @param delta sensitivity for long candle
#' @return TRUE if kicking up pattern detected
three.black.crows <- function(x,n=20,delta=1) {
  D <- bearish.candle(x)
  Lag.D <- quantmod::Lag(D)
  Lag2.D <- quantmod::Lag(D,2)

  LC <- long.candle(x,n,delta)
  Lag.LC <- quantmod::Lag(LC)
  Lag2.LC <- quantmod::Lag(LC,2)

  Cl <- quantmod::Cl(x)
  Lag.Cl <- quantmod::Lag(Cl)
  Lag2.Cl <-quantmod::Lag(Cl,2)

  OP <- quantmod::Op(x)
  Lag.OP <- quantmod::Lag(OP)
  Lag2.OP <-quantmod::Lag(OP,2)

  result <- xts::reclass(D & Lag.D & Lag2.D &
                      LC & Lag.LC & Lag2.LC &
                      Cl < Lag.Cl & Lag.Cl < Lag2.Cl &
                      OP < Lag.OP & Lag.OP < Lag2.OP,
                    x)
  colnames(result) <- "three black crows"
  return(result)
}


#' Determine three white soldiers pattern using a OHLC price series
#' @param x OHLC prices.
#' @param n number of period of trend
#' @param delta sensitivity for long candle
#' @return TRUE if kicking up pattern detected
three.white.soldiers <- function(x,n=20,delta=1) {
  U <- bullish.candle(x)
  Lag.U <- quantmod::Lag(U)
  Lag2.U <- quantmod::Lag(U,2)

  LC <- long.candle(x,n,delta)
  Lag.LC <- quantmod::Lag(LC)
  Lag2.LC <- quantmod::Lag(LC,2)

  Cl <- quantmod::Cl(x)
  Lag.Cl <- quantmod::Lag(Cl)
  Lag2.Cl <-quantmod::Lag(Cl,2)

  OP <- quantmod::Op(x)
  Lag.OP <- quantmod::Lag(OP)
  Lag2.OP <-quantmod::Lag(OP,2)

  result <- xts::reclass(U & Lag.U & Lag2.U &
                    LC & Lag.LC & Lag2.LC &
                    Cl > Lag.Cl & Lag.Cl > Lag2.Cl &
                    OP > Lag.OP & Lag.OP > Lag2.OP,
                    x)
  colnames(result) <- "three white soliders"
  return(result)
}
