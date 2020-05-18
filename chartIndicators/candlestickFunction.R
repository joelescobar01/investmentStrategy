library(TTR)
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleBearishTrend.R")    
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleBody.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleBullishTrend.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleHammer.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleSizes.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleWick.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleBody.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleDoji.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleKick.R") 
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleStar.R") 
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleThreeGroup.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleGap.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleMedianReversal.R")
source("/home/joel/Documents/stocks/chartIndicators/candlestickpattern/candleTrend.R")
source("/home/joel/Documents/stocks/lib/utils.R") 
library(dplyr)
library( ggplot2 )
library(tidyquant)
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
