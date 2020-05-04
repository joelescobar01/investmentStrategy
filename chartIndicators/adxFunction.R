library(tidyquant)
library(tidyverse)
library(TTR)
library(ggplot2)
#source("lib/utils.R")
#source("var/settings.R")
#Sys.setnev(TZ="UTC")

stockADX <- function( stock, dxCal=14  ){
  #where it is greater 1 if it is above the upper band, and less than 0 when it is below the lower band
  adxData <- cbind( Hi(stock), Cl(stock), Lo(stock) )
  adx <- ADX(adxData, n = dxCal )
  return( na.omit(adx)) 
}

get.ADX <- function(stockTbbl, period=3, type=EMA  ){
  stockADXTbbl <- 
    stockTbbl %>%
      tq_mutate( mutate_fun = ADX, 
                n = period, 
                maType = SMA)
  return(stockADXTbbl) 
}

chart.ADX <- function(adxTbbl, plotTitle = "ADX Graph 1.1"){
  g1 <- ggplot( adxTbbl, aes(x=date)) + 
    geom_line( aes(y=ADX, colour="adx"), size=1 ) + 
    geom_line( aes(y=DIp, colour="Positive Direction Index"), size=1) +
    geom_line( aes(y=DIn, colour="Negative Direction Index"), size=1) +
    geom_col( aes(y=DX, fill=sign(DX) ) ) + 
    #geom_hline( aes(yintercept = 0), linetype="dashed") +
    #geom_vline( xintercept=buySignal, color="orange", linetype="dashed" ) +
    #geom_vline( xintercept=sellSignal, color="blue" , linetype="dashed" ) +
    labs(title=plotTitle, 
          subtitle="Minor ticks = 3 days", 
          y="", 
          x="Date",
          caption="n=3, maType=EMA") + 
    scale_colour_manual(values=c("blue", "red","green"), name=NULL) +
    scale_fill_gradient(name=NULL, low = alpha("red",.3), high = alpha("green",.3))+
    theme(legend.position = c(0.1, 0.2))

  return(g1)
}

uptrendStrengthXTS <- function( adxDF ){
  # +DMI is above the -DMI, prices are moving up, and ADX measures the strength of the uptrend.
  uptrend <- adxDF[ adxDF$DIp > adxDF$DIn, c("Date", "ADX")]
  uptrendXTS <- dfToTimeSeries2( uptrend[,'ADX'], uptrend[,'Date'])
  return( uptrendXTS)
}

downtrendStrength <- function( adxDF ){
  # When the -DMI is above the +DMI, prices are moving down, and ADX measures the strength of the downtrend.
  
}

adxSignals <- function( adxDF ){
  uptrendDates <- coredata( adxDF[ which( adxDF$DIp > adxDF$DIn ), 'Date' ] ) 
  downtrendDates <- coredata( adxDF[ which( adxDF$DIp < adxDF$DIn ), 'Date' ] )
  
  class(uptrendDates) <- "Date"
  class(downtrendDates) <- "Date" 
  crossDate <- list( "uptrend" = uptrendDates, "downtrend" = downtrendDates )   
  return(crossDate)  
}

signalXValues <- function( adxList, startDate ){
  if( length(adxList) == 0 )
    return(c() )
  xValues <- c()
  xValueIndex = 1 
  for(i in 1:length(adxList) ){
    xValues[xValueIndex] <- businessDayCounter(startDate, adxList[i] )   
    xValueIndex = 1 + xValueIndex
  }
  return( xValues )
}
