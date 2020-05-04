library(TTR)
library(tidyverse)
library(tidyquant)
library(ggplot2)
#source("analysis/riskAnalysis.R", chdir=T )
#source("lib/utils.R")
#source("var/settings.R")

getMACD <- function( stockDF ){
  macd <- 
    MACD(Cl(stockDF), nFast=12, nSlow=26,
        nSig=9, maType=EMA)
  return( macd)
}


macd.Interface <- function( stockTbbl, startDate=NA, 
                           endDate=NA ){
  stock.MACD.Tb <- 
    stockTbbl %>% 
    get.MACD() %>% 
    macdLinesCrossover() 
  
  #calculate signal cross over 
  #stock.MACD.Tb <- 
  #  stock.MACD.Tb %>% 
  #  macdLinesCrossover() 

  if( !is.na(startDate) ){
    stock.MACD.Tb <-
      stock.MACD.Tb %>% 
      filter( date >= startDate )
  }
  
  if( !is.na(endDate) ){
    stock.MACD.Tb <-
      stock.MACD.Tb %>% 
      filter( date <= endDate )
  }
  
  return( stock.MACD.Tb )
}

get.MACD <- function(stockTbbl, fast=3, slow=10, signal=16 ){
  stockMACDTbbl <- 
    stockTbbl %>%
      group_by(symbol) %>% 
      tq_mutate(select     = close, 
                mutate_fun = MACD, 
                nFast      = 3, 
                nSlow      = 10, 
                nSig       = 16, 
                maType     = EMA) %>%
     mutate(diff = macd - signal) %>%
     select(-(open:volume)) %>%
     drop_na() 

  return(stockMACDTbbl) 
}

#returns Vector 
macdLinesCrossover <- function( macdTbbl ){
  print(macdTbbl)
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

chart.MACD <- function( macdTbbl, plotTitle="MACD Version 1.1" ){
    buySignal <- 
      macdTbbl %>% 
      filter(crossover>0) %>% 
      select(date) %>% 
      pull()  
   
  sellSignal <- 
      macdTbbl %>% 
      filter(crossover<0) %>% 
      select(date) %>%
      pull()  

    g1 <- ggplot( macdTbbl, aes(x=date)) + 
        geom_line( aes(y=macd, colour="MACD"), size=1 ) + 
        geom_line( aes(y=signal, colour="Signal"), size=1) +
        geom_col( aes(y=diff, fill=sign(diff) ) ) + 
        geom_hline( aes(yintercept = 0), linetype="dashed") +
        geom_vline( xintercept=buySignal, color="orange", linetype="dashed" ) +
        geom_vline( xintercept=sellSignal, color="blue" , linetype="dashed" ) +
        labs(title=plotTitle, 
              subtitle="Minor ticks = 3 days, Buy Signal = Orange, Sell Signal = Blue", 
              y="", 
              x="Date",
              caption="nFast=12, nSlow=26, nSig=9, maType=EMA") + 
        scale_colour_manual(values=c("red","green"), name=NULL) +
        scale_fill_gradient(name=NULL, low = alpha("red",.3), high = alpha("green",.3))+
        theme(legend.position = c(0.1, 0.2))

    # Note that, the argument legend.position can be also a numeric vector c(x,y). 
    #In this case it is possible to position the legend inside the plotting area. x and y 
    #are the coordinates of the legend box. Their values should be between 0 and 1. c(0,0) 
    #corresponds to the “bottom left” and c(1,1) corresponds to the “top right” position.
    return(g1)
}
