library(tidyverse)
library(ggplot2)
library(tidyquant)

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

chart.Candlesticks.Tbbl <- function( stockTbbl, plotTitle="Candlestick Tibble Version 0.1", 
                                endDate=Sys.Date(), startDate=endDate-90 ){
    g1 <- 
      ggplot(stockTbbl, aes(x = date, y = close)) +
      geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
      geom_ma(color='red', ma_fun = SMA, n = 8 , linetype = 5, size = 1.25) +  
      geom_ma(color='blue', ma_fun = SMA, n =13 , linetype = 5, size = 1.25) +  
      labs(title = plotTitle, y = "Closing Price", x = "", subtitle="13 Day Moving Average", 
           caption= "red = 8 day MA, blue = 13 day MA" ) + 
        scale_x_date(lim = ( as.Date(c(startDate, endDate )) ),
                     breaks = as.Date( seq(startDate, endDate, by="2 weeks")),
                     minor_breaks = as.Date( seq(startDate, endDate, by="3 days") )) +
      theme_tq() 
     return(g1)
}

chartMACD <- function( macdTbbl, plotTitle="MACD Version 0.1", 
                     endDate=Sys.Date(), startDate=endDate-90 ){
    buySignal <- 
      macdTbbl %>% 
      filter(crossover>0) %>% 
      select(date) %>% 
      filter(date >= startDate, date <= endDate ) %>%
      pull()  
   
  sellSignal <- 
      macdTbbl %>% 
      filter(crossover<0) %>% 
      select(date) %>%
      filter(date >= startDate, date <= endDate ) %>% 
      pull()  

    g1 <- ggplot( macdTbbl, aes(x=date)) + 
        geom_col( aes(y=diff ) ) + 
        geom_line( aes(y=macd, colour="MACD") ) + 
        geom_line( aes(y=signal, colour="Signal"),size=3 ) +
        geom_hline( aes(yintercept = 0), linetype="dashed") +
        geom_vline( xintercept=buySignal, color="orange", linetype="dashed" ) +
        geom_vline( xintercept=sellSignal, color="blue" , linetype="dashed" ) +
        labs(title=plotTitle, subtitle="Minor ticks = 3 days, Buy Signal = Orange, Sell Signal = Blue", y="", x="Date",
                caption="nFast=12, nSlow=26, nSig=9, maType=EMA") + 
        scale_x_date(lim = ( as.Date(c(startDate, endDate )) ),
                     breaks = as.Date( seq(startDate, endDate, by="2 weeks")),
                     minor_breaks = as.Date( seq(startDate, endDate, by="3 days") )) +
        scale_colour_manual(values=c("red","green")i) +
        scale_fill_manual(values = alpha(c("red", "green"), .3)) + 
        theme(legend.position = c(0.1, 0.2))
    # Note that, the argument legend.position can be also a numeric vector c(x,y). 
    #In this case it is possible to position the legend inside the plotting area. x and y 
    #are the coordinates of the legend box. Their values should be between 0 and 1. c(0,0) 
    #corresponds to the “bottom left” and c(1,1) corresponds to the “top right” position.
    
    return(g1)
}



macd.candlestick.Plots <- function( macdTbbl, stockTbbl, plotTitle="MACD Version 0.1", 
                     endDate=Sys.Date(), startDate=endDate-90 ){

    buySignal <- 
      macdTbbl %>% 
      filter(crossover>0) %>% 
      select(date) %>% 
      filter(date >= startDate, date <= endDate ) %>%
      pull()  
   
  sellSignal <- 
      macdTbbl %>% 
      filter(crossover<0) %>% 
      select(date) %>%
      filter(date >= startDate, date <= endDate ) %>% 
      pull()  
 
  g1 <- chart.Candlesticks.Tbbl(stockTbbl) 
  g2 <- chartMACD( macdTbbl ) 
  
  ggarrange(g1, g2, ncol=1, nrow=2, 
            label.x = buySignal ) 

}
#tq_transmute(adjusted,mutate_fun = periodReturn ,period='daily', type='log')                                       
# tq_mutate(select = close, mutate_fun = periodReturn,period='daily',type='log')  daily returns 
# #sp500Material[1,] %>% pull(symboL) %>% getStockTibbl( ...) %>% chart.Candlesticks.Tbbl() 
