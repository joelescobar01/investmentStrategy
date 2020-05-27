#' Determine down trend based on moving average using a OHLC price series
#' @param x OHLC prices.
#' @param delta sensitivity
#' @param S number of short period for short-run moving average
#' @param L number of short period for short-run SMA
#' @return length of upper shadow
DownTrend <- function(stockTbbl, delta=0.01, S=5, L=20) {
  candle <- 
    stockTbbl %>% 
    tq_mutate( select= close, 
               mutate_fun = EMA, 
               n=S,
               col_rename="EMA.Short") 
    candle <- 
      candle %>% 
      tq_mutate( select= close, 
                  mutate_fun = EMA, 
                n=L,
                col_rename="EMA.Long")
  candle <-
    candle %>% 
    mutate( down.trend = (EMA.Short/EMA.Long)-1 < -delta ) 

  return(candle)
}

#' Determine up trend based on moving average using a OHLC price series
#' @param x OHLC prices.
#' @param delta sensitivity
#' @param S number of short period for short-run moving average
#' @param L number of short period for short-run SMA
#' @return length of upper shadow
UpTrend <- function(stockTbbl, delta=0.01, S=5, L=20) {
  candle <- 
    stockTbbl %>% 
    tq_mutate( select= close, 
               mutate_fun = EMA, 
               n=S,
               col_rename="EMA.Short") 
    candle <- 
      candle %>% 
      tq_mutate( select= close, 
                  mutate_fun = EMA, 
                n=L,
                col_rename="EMA.Long")
  candle <-
    candle %>% 
    mutate( up.trend = (EMA.Short/EMA.Long)-1 > delta ) 

  return(candle)
}

UpTrend.Momentum <- function(stockTbbl, delta=0.01, S=5, L=20) {
  candle <- 
    stockTbbl %>% 
    mutate( momentum.short = TTR::momentum( volume, n=S ) ) %>% 
    mutate( momentum.long = TTR::momentum( volume, n=L ) ) %>% 
    mutate( up.trend.momentum = (momentum.short/momentum.long)-1 > delta ) %>%
    select( -momentum.short, -momentum.long ) 

  return(candle)
}

