library(tidyverse)
library(dplyr )
library( tidyquant )
library( gt )
source("chartIndicators/macdFunction.R") 
library( ggpubr ) 
# PipeLines for stock 
# Adds columns of TRUE/FALSE wether signal is met


VolumeRangeMinLimit <- function( stockTbbl, minLimit=1000000 ){
  minVolume <-
    stockTbbl %>% 
    mutate( volume.trade = volume >= minLimit ) 
  return( minVolume ) 
}

AboveAvgTradeVolume.Signal <- function( stockTbbl ){
  tradeVolumeStockTbl <- 
    stockTbbl %>%
      mutate( short.ema = TTR::EMA( volume, n=3 ) ) %>% 
      mutate( long.ema = TTR::EMA( volume, n=21 ) ) %>% 
      mutate( smoothing.divergence = abs( short.ema - long.ema ) ) %>%  
      mutate( avg.trade.volume = TTR::runMean( smoothing.divergence )  ) %>% 
      mutate( high.trade.volume = volume*0.7 >= avg.trade.volume ) 
  return( tradeVolumeStockTbl ) 
}

MomentumSlope.Signal <- function( stockTbbl ){
  momentumStock <-
    stockTbbl 
  momentumStock <- tryCatch(
    stockTbbl %>% 
      tq_mutate(  select=close, 
                mutate_fun=momentum, 
                n=2,
                col_rename="momentum" ) %>%
    mutate( momentum.slope = momentum < lag(momentum) ) %>% 
    mutate( roc.momentum = momentum - lag( momentum ) ), 
  error=function(e){
    mutate( momentum.slope = FALSE ) %>% 
    mutate( roc.momentum = 0 )
    print("Error: Cannot attain Momentum" ) 
  })
  return( momentumStock ) 
}

OversoldStock.Signal <- function( stockTbbl ){
  oversoldStock <- stockTbbl 
  oversoldStock <- tryCatch( 
    stockTbbl %>% 
    tq_mutate(select = close, 
             mutate_fun = RSI, 
             n          = 9, 
             col_rename = "rsi" ) %>% 
    mutate( oversold = rsi < 30 ), 
    error=function(e){
        oversoldStock %>% 
        mutate( oversold = FALSE )
        print("Cannot Attain RSI") 
    })
  return( oversoldStock ) 
}


MACDTrend.Signal <- function( stockTbbl ) {
  trendOscillator <-
    stockTbbl 
  trendOscillator <- tryCatch(
    stockTbbl %>% 
      GetMACD(fast=8, slow=21, signal=13 ) %>% 
      mutate( macd.under.signal = macd <= signal ), 
    error=function(e){
      stockTbbl %>% 
      mutate( macd.under.signal = FALSE ) 
      print("Error: Cannot attain MACD" ) 
    })
  return( trendOscillator ) 
}

Momentum.Buy.Indicator <- function( stockTbbl ){
  indicator <-
    stockTbbl %>%
    VolumeRangeMinLimit %>% 
    AboveAvgTradeVolume.Signal %>% 
    OversoldStock.Signal %>% 
    MACDTrend.Signal %>%
    MomentumSlope.Signal %>% 
    drop_na() %>%
    mutate( buy.indicator = momentum.slope & 
                            volume.trade & 
                            macd.under.signal & 
                            oversold & 
                            high.trade.volume ) %>% 
    mutate( one.day.hold.profit = lead( close ) - close ) %>% 
    mutate( two.day.hold.profit = lead( close,2 ) - close ) %>% 
    mutate( three.day.hold.profit = lead( close, 3 ) - close ) 

  return( indicator ) 
}


WindowDataRetrieval <- function( stockTbbl ){
  datesWeeks <-
    stockTbbl %>% 
    group_by( weekBuy = week(date ) ) %>% 
    select( buy.indicator, weekBuy ) %>% 
    filter( buy.indicator == TRUE ) %>% 
    select( weekBuy ) %>% pull() 

  weekStocks <- 
    stockTbbl %>% 
    group_by( weekBuy = week(date ) ) %>% 
    filter( weekBuy %in% datesWeeks ) %>% 
    arrange( weekBuy ) %>% 
    ungroup() %>% 
    select( -weekBuy ) 

  return( weekStocks ) 
}

generateBuyReportBatch <- function(  symbols=c(), ...){
    data1 <- NULL                               # NULL data1
    data1 <- tryCatch(tq_get( symbols,  
                               get = "stock.prices", 
                               complete_cases=TRUE,
                               ...) %>% 
              group_by( symbol ),
              error=function(e){
                print("Error Occurred retrieving stock data")
                return(NA) 
              })      # empty function for error handling
    
    return( data1 ) 
}

OpenCloseCycles <- function( openPrice=c(), closePrice=c() ) {
  if( length( openPrice ) != length( closePrice ) )
    return(NA) 

  currentVal <- 
    c( cumsum( max(closePrice[1],openPrice[1] ) - min( closePrice[1],openPrice[1] ) )) 
  for( ii in 2:length( openPrice ) ){
    if( currentVal[ii-1] > max(closePrice[ii],openPrice[ii] ) ){ 
      currentVal[ii] <- 
        cumsum( max(closePrice[ii],openPrice[ii]) - min( closePrice[ii],openPrice[ii] ))
    } else {
      currentVal[ii] <-
        cumsum( currentVal[ii-1] + 
               (max(closePrice[ii],openPrice[ii]) - min( closePrice[ii],openPrice[ii] )))
    }
  }
  return( currentVal ) 
}

HighLowCycles <- function( highPrice=c(), lowPrice=c() ) {
  if( length( lowPrice ) != length( highPrice ) )
    return(NA) 

  currentVal <- 
    c( cumsum( highPrice[1] - lowPrice[1] )) 
  for( ii in 2:length( lowPrice ) ){
    if( currentVal[ii-1] > highPrice[ii] ){ 
      currentVal[ii] <- 
        cumsum( highPrice[ii]-lowPrice[ii] )
    } else {
      currentVal[ii] <-
        cumsum( currentVal[ii-1] + 
               highPrice[ii] - lowPrice[ii])
    }
  }
  return( currentVal ) 
}

HighPriceCycle <- function( highPrice=c(), maxPrice=c() ){
  if( length( maxPrice ) != length( highPrice ) )
    return(NA) 

  currentVal <- 
    c( cumsum( highPrice[1] - maxPrice[1] )) 
  for( ii in 2:length( maxPrice ) ){
    if( currentVal[ii-1] > highPrice[ii] ){ 
      currentVal[ii] <- 
        cumsum( highPrice[ii]-maxPrice[ii] )
    } else {
      currentVal[ii] <-
        cumsum( currentVal[ii-1] + 
               highPrice[ii] - maxPrice[ii])
    }
  }
  return( currentVal )
}

LowPriceCycle <- function( lowPrice=c(), minPrice=c() ){
  if( length( lowPrice ) != length( minPrice ) )
    return(NA) 

  currentVal <- 
    c( cumsum( minPrice[1] - lowPrice[1] )) 
  for( ii in 2:length( lowPrice ) ){
    if( currentVal[ii-1] > minPrice[ii] ){ 
      currentVal[ii] <- 
        cumsum( minPrice[ii]-lowPrice[ii] )
    } else {
      currentVal[ii] <-
        cumsum( currentVal[ii-1] + 
               minPrice[ii] - lowPrice[ii])
    }
  }
  return( currentVal ) 
}

HighLowBodyCycle <- function( highPrice=c(), lowBodyPrice=c() ){
  if( length( highPrice ) != length( lowBodyPrice ) )
    return(NA) 

  currentVal <- 
    c( cumsum( highPrice[1] - lowBodyPrice[1] )) 
  for( ii in 2:length( lowBodyPrice ) ){
    if( currentVal[ii-1] > highPrice[ii] ){ 
      currentVal[ii] <- 
        cumsum( highPrice[ii]-lowBodyPrice[ii] )
    } else {
      currentVal[ii] <-
        cumsum( currentVal[ii-1] + 
               highPrice[ii] - lowBodyPrice[ii])
    }
  }
  return( currentVal ) 
}

LowTopBodyCycle <- function( topBodyPrice=c(), lowPrice=c() ){
  if( length( topBodyPrice ) != length( lowPrice ) )
    return(NA) 

  currentVal <- 
    c( cumsum( topBodyPrice[1] - lowPrice[1] )) 
  for( ii in 2:length( lowPrice ) ){
    if( currentVal[ii-1] > topBodyPrice[ii] ){ 
      currentVal[ii] <- 
        cumsum( topBodyPrice[ii]-lowPrice[ii] )
    } else {
      currentVal[ii] <-
        cumsum( currentVal[ii-1] + 
               topBodyPrice[ii] - lowPrice[ii])
    }
  }
  return( currentVal ) 
}



EndOfTrend <- function( stockTbbl ) {
  
  openPrice <- 
    stockTbbl %>% 
    select( open ) %>% 
    pull() 

  closePrice <- 
    stockTbbl %>% 
    select( close ) %>% 
    pull()
  
  o.c.minPrice <-
    pmin( openPrice, closePrice ) 
  o.c.maxPrice <-
    pmax( openPrice, closePrice ) 

  highPrice <-
    stockTbbl %>% 
    select( high ) %>% 
    pull() 

  lowPrice <- 
    stockTbbl %>% 
    select( low ) %>% 
    pull() 

  cycleTrend <- 
    OpenCloseCycles( openPrice, closePrice ) 
  highLowTrend <-  
    HighLowCycles( highPrice, lowPrice )
  lowTrend <-
    LowPriceCycle( lowPrice, o.c.minPrice )
  highTrend <- 
    HighPriceCycle( highPrice, o.c.maxPrice )
  topQuarterTrend <- 
    HighLowBodyCycle( highPrice, o.c.minPrice )
  bottomQuarterTrend <- 
    LowTopBodyCycle( o.c.maxPrice, lowPrice ) 

  stockTbbl <-   
    stockTbbl %>% 
    mutate( open.close.cycle= cycleTrend ) %>% 
    mutate( open.close.run = TrendRunCounter( open.close.cycle ) ) %>%
    mutate( open.close.slope = open.close.cycle/open.close.run ) %>% 
    mutate( high.low.cycle = highLowTrend ) %>% 
    mutate( high.low.run = TrendRunCounter( highLowTrend ) ) %>%
    mutate( high.low.slope = high.low.cycle / high.low.run ) %>% 
    mutate( high.trend = highTrend ) %>% 
    mutate( high.trend.run = TrendRunCounter( high.trend ) ) %>% 
    mutate( high.trend.slope = high.trend/high.trend.run ) %>% 
    mutate( low.trend = lowTrend ) %>% 
    mutate( low.trend.run = TrendRunCounter( low.trend ) ) %>% 
    mutate( low.trend.slope = low.trend/low.trend.run ) %>% 
    mutate( top.quarter = topQuarterTrend ) %>% 
    mutate( top.quarter.run = TrendRunCounter( top.quarter ) ) %>% 
    mutate( top.quarter.slope = top.quarter / top.quarter.run ) %>% 
    mutate( bottom.quarter = bottomQuarterTrend )  %>% 
    mutate( bottom.quarter.run = TrendRunCounter( bottom.quarter ) )  %>% 
    mutate( bottom.quarter.slope = bottom.quarter / bottom.quarter.run ) 
  return( stockTbbl ) 

}

TrendAnalysis <- function( stockTbbl ){

  trend <- 
    stockTbbl %>% 
    EndOfTrend()
 
  p1 <- 
    trend %>% 
    ggplot( aes(x=date) ) + geom_line( aes(y=close) ) + 
    geom_line( aes(y=open.close.cycle, colour="OpenCloseVol") )
  p2 <- 
    trend %>% 
    ggplot( aes(x=date) ) + geom_line( aes(y=close) ) +
    geom_line( aes( y=high.low.cycle, colour="HighLowVol" ) )
  p3 <- 
    trend %>% 
    ggplot( aes(x=date) ) + geom_line( aes(y=close)) + 
    geom_line( aes( y=high.trend, colour="High" ) )

  p4 <- 
    trend %>% 
    ggplot( aes(x=date) ) + 
    geom_line( aes(y=close)) + 
    geom_line( aes( y=low.trend, colour="Low" ) )

  ticker <-
    stockTbbl %>% 
    select( symbol ) %>% 
    unique %>% 
    paste( collapse="" ) 

  fileName1 <- 
    paste( "/home/joel/Documents/stocks/research/plots/volatiltyPlots", 
            ticker, "_OpenClose_HighLow.png", sep="" ) 
  fileName2 <- 
    paste( "/home/joel/Documents/stocks/research/plots/volatiltyPlots", 
          ticker, "_High_Low.png", sep="" )
  
  gp1 <- 
    ggarrange( p1, p2, nrow=2, ncol=1 ) 
  gp2 <-
    ggarrange( p3, p4, nrow=2, ncol=1 ) 

  ggsave( fileName1, plot=gp1 ) 
  ggsave( fileName2, plot=gp2 ) 

}

TrendAnalysis2 <- function( stockTbbl ){

  trend <- 
    stockTbbl %>% 
    EndOfTrend() %>% 
		mutate( ema.close = TTR::EMA( close, n=5 ) )  

  ticker <-
    stockTbbl %>% 
    select( symbol ) %>% 
    unique %>% 
    paste( collapse="" )

  p1 <- 
    trend %>% 
    ggplot( aes(x=date) ) + 
		geom_line( aes(y=close), size=3 ) +
    geom_line( aes(y=open.close.cycle, colour="Open-Close"), alpha=0.5, size=2) +
    geom_line( aes( y=low.trend, colour="Min-Low" ), alpha=0.5, size=2 ) +
    geom_line( aes( y=top.quarter, colour="Hi-BBody"), alpha=0.5, size=2 ) + 
    scale_x_date( date_breaks="1 months", date_label="%m/%Y", minor_breaks="2 weeks" ) + 
    labs( title = ticker, y="Stock Close Price" ) + 
    scale_colour_manual(	values = c("blue", "red", "yellow", "gray"),
												 	labels = c("OpCl Volatility", "MinLo Volatility", 
                                      "Top.Quart", "Smooth Close" ) )

  p2 <- 
    trend %>% 
    ggplot( aes(x=date) ) + 
		geom_line( aes(y=close), size=3 ) +
		geom_line( aes( y=high.low.cycle, colour="High-Low" ), alpha=0.5, size=2 ) + 
    geom_line( aes( y=high.trend, colour="Max-High" ), size=2) +
    geom_line( aes( y=bottom.quarter, colour="TBody-Lo"), alpha=0.5, size=2 ) + 
    scale_x_date( date_breaks="1 months", date_label="%m/%Y", minor_breaks="2 weeks" ) + 
    labs( title = ticker, y="Stock Close Price" ) + 
		scale_colour_manual(	values = c( "orange", "green", "brown", "gray"),
												 	labels = c( "HiLo Volatility", "MxHi Volatility", 
                                      "Bot.Quart" ,"Smooth Close" ) )

  gp1 <-
    ggarrange( p1, p2, ncol=1, nrow=2 ) 
  #fileName1 <- 
  #  paste( "/home/joel/Documents/stocks/research/plots/volatiltyPlots/", 
  #          ticker, "_OpenClose_MinLow.png", sep="" ) 
  #fileName2 <- 
  #  paste( "/home/joel/Documents/stocks/research/plots/volatiltyPlots/", 
  #        ticker, "_HighLow_MaxHigh.png", sep="" )
  
  #ggsave( fileName1, plot=p1 ) 
  #ggsave( fileName2, plot=p2 ) 
  return( gp1 ) 
}

TrendRunCounter <- function( volatilitySlopeVector ){  
  if( length( volatilitySlopeVector ) == 0 )
    return( 0 ) 

  dayCounter <- c(0) 
  for( ii in 2:length(volatilitySlopeVector ) ){
    if( volatilitySlopeVector[ii] < volatilitySlopeVector[ii-1] )
      dayCounter[ii] <- 0
    else {
      dayCounter[ii] <- 
        dayCounter[ii-1] + 1 
    }
  }
  return( dayCounter ) 
}



printTibblePlot <- function( tibblePlot ){
  fileName <- 
    paste( tibblePlot$symbol, "_MACD", ".png", sep="", collapse="" ) 
  fileName <- paste( directory, fileName, sep="/") 
  p1 <- 
    tibblePlot$macd.indicator[[1]] 
  if( !is.na(p1) ) 
    ggsave( fileName, plot=chart.MACD(p1)  ) 

  fileName <- 
    paste( tibblePlot$symbol, "_RSI", ".png", sep="", collapse="" ) 
  fileName <- paste( directory, fileName, sep="/") 
  p1 <- 
    tibblePlot$rsi.indicator[[1]] 
  if( !is.na(p1) ) 
    ggsave( fileName, plot=chart.RSI(p1)  ) 
}
