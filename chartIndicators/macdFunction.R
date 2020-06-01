library(TTR)
library(tidyverse)
library(tidyquant)
library(ggplot2)
source('visual.lib.R')

GetMACD <- function(stockTbbl, fast=12, slow=26, signal=9 ){
  stockMACDTbbl <- tryCatch({  
    stockTbbl %>% 
    tq_mutate(select     = close, 
                mutate_fun = MACD, 
                nFast      = fast, 
                nSlow      = slow,
                nSig       = signal, 
                maType     = EMA) %>%
    mutate(divergence = macd - signal)}, 
  error=function(e) {
      stockTbbl %>% mutate( macd=NA, signal=NA, divergence=NA ) 
  })
  return(stockMACDTbbl) 
}

chart.MACD <- function( macdTbbl,plotTitle="MACD Version 1.2",zoomDays=21 ){
  macdTbbl <- 
    macdTbbl %>% 
    drop_na() 
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
        zoom.last_n( macdTbbl, n=zoomDays ) + 
        scale.date.axis() + 
      max.plot.space()   
    # Note that, the argument legend.position can be also a numeric vector c(x,y). 
    #In this case it is possible to position the legend inside the plotting area. x and y 
    #are the coordinates of the legend box. Their values should be between 0 and 1. c(0,0) 
    #corresponds to the “bottom left” and c(1,1) corresponds to the “top right” position.
    return(g1)
}

