library(tidyverse)
library(ggplot2)
library(tidyquant)
library(ggpubr) 
source("visual.lib.R")

chart.SectorDailyReturns <- function( ){

  sectorPerformance <- 
    c("IXM", "IXE", "IXT", "IXB", "IXY", "SPX", "IXU", "IXV", "IXR" ) %>% 
    yahoo.Stock.Prices(from="2020-04-01") %>% 
    group_by( symbol ) %>%  
    ggplot( aes( x=date) ) + 
    geom_col( aes(y=(close-open), fill=sign(close-open) ) ) + 
    scale_y_continuous(labels = scales::dollar_format()) + 
    facet_wrap(~symbol) + 
    labs( y="Daily Returns" ) + 
    scale_fill_gradient(low="red", high="green" ) + 
    guides(fill="none")
  return( sectorPerformance ) 
}

detectSupportResistance <- function(timeSeries, tolerance=0.01, 
                                    nChunks=10, nPoints=3, plotChart=TRUE){
    #detect maximums and minimums
    N = length(timeSeries)
    stp = floor(N / nChunks)
    minz = array(0.0, dim=nChunks)
    whichMinz = array(0, dim=nChunks)
    maxz = array(0.0, dim=nChunks)
    whichMaxz = array(0, dim=nChunks)
    for(j in 1:(nChunks-1)) 
    {
        lft = (j-1)*stp + 1  #left and right elements of each chunk
        rght = j*stp
        whichMinz[j] = which.min(timeSeries[lft:rght]) + lft
        minz[j] = min(timeSeries[lft:rght])
        whichMaxz[j] = which.max(timeSeries[lft:rght]) + lft
        maxz[j] = max(timeSeries[lft:rght])
    }   
    #last chunk
    lft = j*stp + 1  #left and right elements of each chunk
    rght = N
    whichMinz[nChunks] = which.min(timeSeries[lft:rght]) + lft
    minz[nChunks] = min(timeSeries[lft:rght])
    whichMaxz[nChunks] = which.max(timeSeries[lft:rght]) + lft
    maxz[nChunks] = max(timeSeries[lft:rght])
     
    result = list()
    result[["minima"]] = NULL
    result[["minimaAt"]] = NULL
    result[["maxima"]] = NULL
    result[["maximaAt"]] = NULL
    span = tolerance*(max(maxz) - min(minz))
     
    rang = order(minz)[1:nPoints]
    if((minz[rang[nPoints]] - minz[rang[1]]) <= span)
    {
        result[["minima"]] = minz[rang[1:nPoints]]
        result[["minimaAt"]] = whichMinz[rang[1:nPoints]]
    } 
     
    rang = order(maxz, decreasing = TRUE)[1:nPoints]
    if((maxz[rang[1]] - maxz[rang[nPoints]]) <= span)
    {
        result[["maxima"]] = maxz[rang[1:nPoints]]
        result[["maximaAt"]] = whichMaxz[rang[1:nPoints]]
    } 
     
    if(plotChart)
    {
        ts.plot(timeSeries)
        points(whichMinz, minz, col="blue")
        points(whichMaxz, maxz, col="red")
        if(!is.null(result[["minima"]])  &&  !is.null(result[["minimaAt"]]))
            abline(lm(result[["minima"]] ~  result[["minimaAt"]]))
        if(!is.null(result[["maxima"]])  &&  !is.null(result[["maximaAt"]]))
            abline(lm(result[["maxima"]] ~  result[["maximaAt"]]))
    } 
     
    return(result)    
}

chart.VolatilityCloseToClose <- function( stockOHLC, endDate=Sys.Date(), startDate=Sys.Date()-90, 
                                         plotTitle="Volatility Close Chart 0.1"){
  ohlc <- stockOHLC[,1:4]
  Close <- volatility(ohlc, calc="close")
  closeVolDF <- zooToDataFrame(Close)
  colnames(closeVolDF) <- c("Volatility")
  closeVolDF$Close <- coredata(Cl(stockOHLC))
  closeVolDF$Date <- as.Date( index(stockOHLC) )
  
  p1 <- ggplot( closeVolDF, aes(Date )) +
    geom_line(aes(y=Volatility, colour = "Volatility")) + 
    geom_line(aes(y=log(Close), colour = "Log Closing Price")) +
    scale_x_date(lim = ( c(startDate, endDate ) ),
                 breaks = as.Date( seq(startDate, endDate, by="2 weeks")),
                 minor_breaks = as.Date( seq(startDate, endDate, by="3 days") )) +
    scale_colour_manual(values=c("red","green","blue")) +
    theme(legend.position = c(0.8, 0.9))
  return(p1)
}

chart.ReturnDistribution <- function( stockTbbl, plotTitle="Log Daily Returns" ){
  
  stockDailyReturns <- 
    stockDailyLogReturns(stockTbbl)
  volatilityMeasure <- 
    stockDistributionMetric( stockDailyReturns )
  p1 <- ggplot(stockDailyReturns, aes(x = Log.Returns)) + 
    geom_histogram(aes(y = ..density..), bins=100) + 
    geom_density() +
    geom_rug(alpha = 0.5) +
    geom_vline(xintercept = unname(volatilityMeasure), colour="red", linetype=2 )+
    labs(title=plotTitle, y="Count", x="Daily Returns Log", caption=paste("2.5 Percentile (MaxLoss Per Share):", volatilityMeasure["2.5%"]))
  return(p1)
}

chart.ReturnRateTbbl <- function(dailyYieldTbbl, plotTitle="Daily Return Rate"){
  dailyRate <- cumprod(1+dailyYield)
  p1 <- ggplot(dailyRate, aes(x = date, y=Rate)) +
    geom_line() +
    labs(title = plotTitle, x = "Date", y ="Return") + 
    theme_tq() +
    labs(caption="Log Base")
  return(p1)
}

chart.ExcessiveReturn <- function( riskFreeRate, stockRate , plotTitle="Excess Return" ){
  p2 <- chartReturnRate(riskFreeRate, plotTitle="RiskFree Rate")
  p3 <- chartReturnRate(stockRate)
  riskFreeRate <- cumprod(1+riskFreeRate)
  stockRate <- cumprod(1+stockRate)
  p1 <- ggplot() + 
    geom_line(data = riskFreeRate, aes(x = index(riskFreeRate), y = Rate, colour = "blue"), size=1) +
    geom_line(data = stockRate, aes(x = index(stockRate), y = Rate, colour = "red"), size=1) +
    xlab('Dates') +
    ylab('Return' ) + 
    labs(title="Excessive Return", caption="Stock 3 month return rate compared to a Risk Free Rate") + 
    scale_color_discrete(name = "Return Rate", labels = c("T-Bill-3M", "stock"))
  
  pall <- ggarrange(p1,                                                 # First row with scatter plot
                    ggarrange(p2, p3, ncol = 2 ), # Second row with box and dot plots
                    nrow = 2                               # Labels of the scatter plot
  ) 
  return(pall)
}

chartVolatilityClose0 <- function( stock, endDate=Sys.Date(), startDate=Sys.Date()-90, plotTitle="Volatility Close Chart 0.1" ){
    ohlc <- stock[,1:4]
    vClose0 <- volatility(ohlc, calc="close", mean0=TRUE)
    vClose0DF <- zooToDataFrame(vClose0)
    colnames(vClose0DF) <- c("Date", "Volatility")
    p2 <- ggplot( vClose0DF, aes(x=Date, y= Volatility )) +
        geom_line() + 
        labs( x="Garman and Klass")
}

charVolatilityParkinson <- function(stock, endDate=Sys.Date(), startDate=Sys.Date()-90, plotTitle="Volatility Close Chart 0.1"){
    stock <- stock %>%
        adjustOHLC(use.Adjusted = TRUE)
    ohlc <- stock[,1:4]
    vParkinson <- volatility(ohlc, calc="parkinson")
    vParkinsonDF <- zooToDataFrame(vParkinson)
    colnames(vParkinsonDF) <- c("Date", "Volatility")
    p4 <- ggplot( vParkinsonDF, aes(x=Date, y= Volatility )) +
        geom_line() + 
        labs( x="Parkinson High-Low")
}

charVolatilityGK <- function(stock, endDate=Sys.Date(), startDate=Sys.Date()-90, plotTitle="Volatility Close Chart 0.1"){
    ohlc <- stock[,1:4]
    vGK <- volatility(ohlc, calc="garman")
 vGKDF <- zooToDataFrame(vGK)
 colnames(vGKDF) <- c("Date", "Volatility")
 p3 <- ggplot( vClose0DF, aes(x=Date, y= Volatility )) +
     geom_line() + 
     labs( x="OHLC Volatility")
}

charVolatilityGK <- function(stock, endDate=Sys.Date(), startDate=Sys.Date()-90, plotTitle="Volatility Close Chart 0.1"){
     ohlc <- stock[,1:4] 
     vRS <- volatility(ohlc, calc="rogers")
      vRSDF <- zooToDataFrame(vRS)
      colnames(vRSDF) <- c("Date", "Volatility")
      p5 <- ggplot( vRSDF, aes(x=Date, y= Volatility )) +
         geom_line() +
         labs( x="OHLC Rogers and Satchell")
}

TrendAnalysis2 <- function( stockTbbl ){

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

supportResistanceLines <- function( stockTbbl, nChunks='1 month', nPoints=3 ){
    #detect maximums and minimums

    chunkStock <- 
      stockTbbl %>% 
      dateIntervalSummary(nDays=nChunks) 
    
    localMin <- 
      chunkStock %>%
      group_by( week ) %>% 
      slice( which.min( pmin(open,close) ) ) %>% 
      select( date, close, open) %>% 
      mutate( daily.min = pmin( close, open ) ) %>%
      ungroup

    localMax <- 
      chunkStock %>%
      group_by( week ) %>% 
      slice( which.max( daily.max ) ) %>% 
      select( date, close, open) %>% 
      mutate( daily.max = pmax( close, open ) ) %>% 
      ungroup

    p <-
      stockTbbl %>% 
      drop_na() %>% 
      ggplot( aes(x=date) ) + 
      geom_line( aes(y=close) ) +
      geom_line( data=localMin, aes( x=date, y=daily.min), color='blue') +
      geom_line( data=localMax, aes( x=date, y=daily.max), color='red' ) + 
      max.plot.space() +
      scale.date.axis() + 
      scale.price.axis() +  
      theme()
    
    return(p)    
}

chart.Dividend.Payout <- function( stock ){
  p1 <- 
    stock %>% 
    group_by(symbol) %>% 
    ggplot(aes(x = date, y = value, color = symbol)) + 
    geom_point() + 
    geom_text(aes(label = symbol), vjust=0, nudge_y=0.002) +
    scale_y_continuous(labels = scales::dollar, 
                       breaks = scales::pretty_breaks(n = 10))  +
    scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
    labs(x = "", y = "Dividend per Share", title = "Dividends") +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) 

  return(p1)
}



#tq_transmute(adjusted,mutate_fun = periodReturn ,period='daily', type='log')                                       
# tq_mutate(select = close, mutate_fun = periodReturn,period='daily',type='log')  daily returns 
# #sp500Material[1,] %>% pull(symboL) %>% getStockTibbl( ...) %>% chart.Candlesticks.Tbbl() 
