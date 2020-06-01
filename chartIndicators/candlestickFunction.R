library(TTR)
library(dplyr)
library( ggplot2 )
library(tidyquant)
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleSticks.R") 
#movement patterns 

chart.CandleStick <- function( stockTbbl, plotTitle="Candlestick Version 0.1" ) {  
    g1 <- 
    ggplot( stockTbbl, aes( x=date ) ) + 
    geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
    labs(title = "Candlestick Chart", 
         y = "Closing Price", 
         x = "") + 
    scale_x_date( date_labels="%m %Y",
                  breaks = '1 months',
                  minor_breaks = '2 weeks') +
    theme() 
  return(g1) 
}

Candlestick.Chart.Add.Pattern <- function( candlestickChart, patternTbbl, color="black", label="" ) {
  g1 <-
    candlestickChart + 
    geom_rect(data=patternTbbl, 
              aes(  ymin=low, ymax=high, 
                    xmin=date-days(1), xmax=date+days(1)),
              alpha=0.3, color=color, fill=color ) + 
    geom_label( data=patternTbbl, position=position_nudge(x=0,y=-1), 
                alpha=0.2, aes( x=date, y=low), 
                label=label ) 
  return( g1 ) 
}

Candlestick.Chart.Add.Ribbon <- function( candlestickChart, patternTbbl, 
                                         pastDays=1, futureDays=1, color="red", 
                                         label="") {
  g1 <-
    candlestickChart + 
    geom_rect(data=patternTbbl, 
              aes(  ymin=low, 
                    ymax=high, 
                    xmin=date-days(pastDays), 
                    xmax=date+days(futureDays) ),
              alpha=0.3, color=color, fill=color ) + 
    geom_label( data=patternTbbl, 
                position=position_nudge(x=0,y=-1), 
                alpha=0.2, 
                aes( x=date, y=low), 
                label=label ) 
  return( g1 ) 

}
