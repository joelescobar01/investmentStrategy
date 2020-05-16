#' Determine everning star pattern using a OHLC price series
#' @param x OHLC prices.
#' @param n number of period of trend
#' @param L.delta sensitivity for long candle
#' @param S.delta sensitivity for short candle
#' @return TRUE if kicking up pattern detected
evening.star <- function(x,n=20,L.delta=1,S.delta=1) {
  U <- bullish.candle(x)
  D <- bearish.candle(x)

  Lag2.U <- quantmod::Lag(U,2)

  LC <- long.candle(x,n,L.delta)
  Lag2.LC <- quantmod::Lag(LC,2)

  SC <- short.candle(x,n,S.delta)
  Lag.SC <- quantmod::Lag(SC)

  GU <- gap.up(x)
  GD <- gap.down(x)
  Lag.GU <- quantmod::Lag(GU)


  result <- xts::reclass(D & LC &
                      Lag.SC &
                      Lag2.U & Lag2.LC &
                      GD &
                      Lag.GU,
                    x)
  colnames(result) <- "evening star"
  return(result)
}
EveningStar <- function(stockTbbl,n=20,L.delta=1,S.delta=1) {
  #Day 1 Lag(2)
  candle <- 
    stockTbbl %>%
    BullishCandle %>% 
    mutate( bullish.candle = lag( bullish.candle, 2 ) ) 

  candle <-
    candle %>% 
    LongCandle() %>% 
    mutate( long.candle = lag( long.candle, 2 ) )

  #Day 2 Lag(1) 
  candle <- 
    candle %>% 
    GapUp() %>% 
    mutate( gap.up = lag( gap.up ) ) 

  candle <-
    candle %>% 
    BearishCandle %>% 
    mutate( bearish.candle = lag( bearish.candle ) ) 
  
  #Day 3 Present 
  candle <- 
    candle %>% 
    GapDown() %>% 
    BearishCandle() 
  
  candle <-
    candle %>% 
    mutate( evening.star = bullish.candle & long.candle & gap.up & bearish.candle & gap.down ) 
  
  return( candle ) 
}


#' Determine morning star pattern using a OHLC price series
#' @param x OHLC prices.
#' @param n number of period of trend
#' @param L.delta sensitivity for long candle
#' @param S.delta sensitivity for short candle
#' @return TRUE if kicking up pattern detected
morning.star <- function(x,n=20,L.delta=1,S.delta=1) {
  U <- bullish.candle(x)
  D <- bearish.candle(x)

  Lag2.D <- quantmod::Lag(D,2)

  LC <- long.candle(x,n,L.delta)
  Lag2.LC <- quantmod::Lag(LC,2)

  SC <- short.candle(x,n,S.delta)
  Lag.SC <- quantmod::Lag(SC)

  GU <- gap.up(x)
  GD <- gap.down(x)
  Lag.GD <- quantmod::Lag(GD)


  result <- xts::reclass(U & LC &
                      Lag.SC &
                      Lag2.D & Lag2.LC &
                      GU &
                      Lag.GD,
                    x)
  colnames(result) <- "morning star"
  return(result)
}
