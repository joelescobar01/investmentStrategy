library(TTR)
library(tidyverse)
library(tidyquant)
library(ggplot2)


# finds stocks that signal a nearby buy 
#signal.MACD.BUY <- function( stockTbbl, nDays=7 ){
    
getMACD <- function( stockDF ){
  macd <- 
    MACD(Cl(stockDF), nFast=12, nSlow=26,
        nSig=9, maType=EMA)
  return( macd)
}

macd.Interface <- function( stockTbbl){
  stock.MACD.Tb <- 
    stockTbbl %>% 
    get.MACD()
  
  #calculate signal cross over 
  #stock.MACD.Tb <- 
  #  stock.MACD.Tb %>% 
  #  macdLinesCrossover() 
  return( stock.MACD.Tb )
}

get.MACD <- function(stockTbbl, fast=3, slow=10, signal=16 ){
    
  stockMACDTbbl <- 
    stockTbbl %>% 
    group_by(symbol) %>% 
    tq_mutate(select     = adjusted, 
                mutate_fun = MACD, 
                nFast      = 3, 
                nSlow      = 10, 
                nSig       = 16, 
                maType     = EMA) %>%
     mutate(divergence = macd - signal) %>%
     select(-(open:volume)) %>% 
    drop_na() 
  return(stockMACDTbbl) 
}

#returns Vector 
macdLinesCrossover <- function( macdTbbl ){
  cross <- 
    macdTbbl %>% 
    mutate( macd.above = macd>signal ) %>% 
    pull(macd.above) %>% 
    diff()
  cross <- c( 0, cross ) #since diff doesn't perform on first value 
  macdTbbl <- 
    macdTbbl %>% 
    mutate( crossover = cross )
  
  return(macdTbbl )      
}

macdSlope <- function( macdTbbl, slope=1){
  slope <- sign(slope)
  
  positiveSlopeMACD <-
    macdTbbl[ which( divergence( positiveDiffMACD$macd ) >= 0 ), ]
  return(macdTbbl) 
}

macdSignalDistance <- function( macdTbbl ){
  positiveDiffMACD <- 
    stockMACDTbbl %>%
    filter( row_number() >= (n()-29)) %>% 
    filter( divergence >= 0 )
  return(positiveDiffMACD) 
}

signal.Buy.MACD <- function( stockTbbl, nDays=9 ){
  
  stockMACDTbbl <- 
    stockTbbl %>%
    get.MACD() 

  MACDTbbl <- 
    stockMACDTbbl %>% 
    tail(n=nDays) 

    
  #signal 1.5: If divergence is pretty large and positive ignore uptrending 
  divergenceVector <- 
    MACDTbbl %>% 
    select(divergence) %>% 
    pull() 
  
  divergenceMean <- 
    divergenceVector %>% 
    mean() 
  divergenceSD <- 
    divergenceVector %>% 
    sd() 

  #signal 1: positive DIFF 
  positiveDiffMACD <- 
    MACDTbbl %>%
    filter( divergence  <= divergenceMean+divergenceSD )

  #signal2: positive MACD slope 
  positiveSlopeMACD <-
    positiveDiffMACD %>% 
    mutate( slope.macd = macd - lag(macd) ) %>% 
    filter( slope.macd > 0) %>% 
    pull()

  #signal3: macd crossed over signal
  #crossMACDLine <- 
  #  positiveSlopeMACD %>% 
  #  macdLinesCrossover() 
  
  if( length(positiveSlopeMACD) == 0 )
    return(NA)
  else{
    g1 <- 
      chart.MACD( stockMACDTbbl )
    return(g1)
  } 
}

chart.MACD <- function( macdTbbl, plotTitle="MACD Version 1.2" ){
  startDate <- 
    macdTbbl %>% 
    select(date) %>% 
    first(n=3) %>%
    last() %>% 
    pull() 
  
  startSignal <-
    macdTbbl %>% 
    select(signal) %>% 
    first() %>% 
    pull() 
  
  startMACD <-
    macdTbbl %>% 
    select(macd) %>% 
    first() %>% 
    pull() 
  
    g1 <- ggplot( macdTbbl, aes(x=date)) + 
        geom_line( aes(y=macd, colour="MACD"), size=1 ) +
        geom_text(x=startDate, y=startMACD+1, aes(colour="MACD"), label="MACD", size=4)+
        geom_line( aes(y=signal, colour="Signal"), size=1) +
        geom_text(x=startDate, y=startSignal-0.5, aes(colour="Signal"), label="Signal", size=4)+
        geom_col( aes(y=divergence, fill=sign(divergence) ) ) + 
        scale_colour_manual( 
          guide="none",
          values=c("blue", "red")
        )+
        labs(title=plotTitle, 
              y="", 
              x="Date") + 
        scale_fill_gradient(guide=NULL, name=NULL, low = alpha("red",.5), high = alpha("green",.5))+
        theme()

    # Note that, the argument legend.position can be also a numeric vector c(x,y). 
    #In this case it is possible to position the legend inside the plotting area. x and y 
    #are the coordinates of the legend box. Their values should be between 0 and 1. c(0,0) 
    #corresponds to the “bottom left” and c(1,1) corresponds to the “top right” position.
    return(g1)
}
