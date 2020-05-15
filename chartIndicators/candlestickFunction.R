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
library(tidyquant)
#movement patterns 


chart.CandleStick <- function( stockTbbl, plotTitle="Candlestick Version 0.1" ) {  

    g1 <- 
    ggplot( stockTbbl, aes( x=date ) ) + 
    geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
    labs(title = "Candlestick Chart", 
         y = "Closing Price", 
         x = "") + 
    scale_x_date( date_labels="%m/%Y",
                  breaks = '1 months',
                  minor_breaks = '2 weeks') +
    theme() 
  
  return(g1) 
}

Candlestick.Chart.Add.Patter <- function( candlestickChart, patternTbbl, color="black" ) {
  g1 <-
    candlestickChart + 
    geom_rect(data=patternTbbl, aes( ymin=low, ymax=high, xmin=date-days(1), xmax=date+days(1)),alpha=0.5, color=color, fill=color )

  return( g1 ) 
}

