library(TTR)
library(tidyverse)
library(tidyquant)
library(ggplot2)


# finds stocks that signal a nearby buy 
#signal.MACD.BUY <- function( stockTbbl, nDays=7 ){
    
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

GetMACD <- function(stockTbbl, fast=3, slow=10, signal=16 ){
  if( stockTbbl %>% tally < signal*2 )
    return(NA)

  stockMACDTbbl <- 
    stockTbbl %>% 
    group_by(symbol) %>% 
    tq_mutate(select     = adjusted, 
                mutate_fun = MACD, 
                nFast      = fast, 
                nSlow      = slow,
                nSig       = signal, 
                maType     = EMA) %>%
     mutate(divergence = macd - signal)
  return(stockMACDTbbl) 
}

RateOfChangeDivergence <- function( macdTbbl ){
  macdTbbl <- 
    macdTbbl %>% 
    mutate( signal.diverging = abs(divergence) > abs(lag(divergence)) &
                               abs(lag( divergence )) > abs(lag( divergence,2)) ) 
  return( macdTbbl ) 
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

signal.Buy.MACD <- function( stockTbbl, nDays=5, symbol="MACD" ){
  stockMACDTbbl <- 
    stockTbbl %>%
    GetMACD()  
  
  if( is.na( stockMACDTbbl ) )
    return(NA)

  testTbbl <- 
    stockMACDTbbl %>% 
    RateOfChangeDivergence() %>% 
    filter( date >= ymd(Sys.Date() - days(nDays) ) ) %>%
    filter( any( signal.diverging ) )

  if( testTbbl %>% nrow() == 0 ) 
    return(NA)
  else{
    g1 <- 
      chart.MACD( stockMACDTbbl, plotTitle=symbol )
    return(g1)
  } 
}

chart.MACD <- function( macdTbbl,plotTitle="MACD Version 1.2",zoomDays=21 ){
  g1 <- ggplot( macdTbbl, aes(x=date)) + 
        geom_line( aes(y=macd, colour="MACD"), size=1 ) +
        geom_text(x=nth(macdTbbl$date,n=-1), 
                  y=nth(macdTbbl$macd,n=-1), 
                  aes(colour="MACD"),
                  vjust=0, nudge_y=0.5,
                  label="MACD", size=5)+
        geom_line( aes(y=signal, colour="Signal"), size=1) +
        geom_text(x=nth(macdTbbl$date,n=-15), 
                  y=nth(macdTbbl$signal,n=-15), 
                  aes(colour="Signal"), 
                  vjust=0, nudge_y=0.5,
                  label="Signal", size=5)+
        geom_col( aes(y=divergence, fill=sign(divergence) ) ) + 
        scale_colour_manual( 
          guide="none",
          values=c("blue", "red")
        )+
        labs(title=plotTitle, 
              y="", 
              x="Date") + 
        scale_fill_gradient(guide=NULL, name=NULL, low = alpha("red",.5), high = alpha("green",.5))+
        coord_cartesian(xlim=c( 
                            nth(macdTbbl$date,n=1)+days(zoomDays), 
                            nth(macdTbbl$date,n=-1)) )+ 
        theme()

    # Note that, the argument legend.position can be also a numeric vector c(x,y). 
    #In this case it is possible to position the legend inside the plotting area. x and y 
    #are the coordinates of the legend box. Their values should be between 0 and 1. c(0,0) 
    #corresponds to the “bottom left” and c(1,1) corresponds to the “top right” position.
    return(g1)
}
