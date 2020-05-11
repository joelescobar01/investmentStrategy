require(quantmod)
require(PerformanceAnalytics)
require(forecast)
require(purrr)
require(tibble)
require(lubridate)
require(tidyquant)
require(dplyr)
require(rvest)
require(tidyverse)
source("analysis/chart.analysis.R")
# width="600px" border="0" cellspacing="0" cellpadding="0" bgcolor="#336699"
purchaseQty <- c( 10, 15, 20, 25, 30, 35 )
probs <- c(.005, 0.0165, .025, .05, .25, .5, .75, .975, .995)
MAXSALELOSS <- 0.02

getStockTbbl <- function( stockSymbolChar, ... ){
  stockTbbl <- tq_get( toupper(stockSymbolChar),
                        get="stock.prices",
                        ... )
  return(stockTbbl) 
}

stockDailyLogReturnsTbbl <- function ( stockTbbl ){
  dailyLogReturnsTbbl <- 
    stockTbbl %>% 
    tq_transmute(adjusted, 
                 mutate_fun = periodReturn, 
                 period = 'daily', 
                 type='log' )
  return(dailyLogReturnsTbbl)
}

stockDistributionMetricTbbl <- function( stockDailyLogReturnTbbl ){
  #probs <- c(.005, .025, .05, .25, .5, .75, .975, .995)
  dist_log_returns <- 
    stockDailyLogReturnTbbl %>% 
    select( daily.returns ) %>% 
    pull() %>%
    quantile(probs = probs, na.rm = TRUE)
  return( dist_log_returns )
}
stockAvgDailyReturnRateTbbl <- function( stockDailyLogReturnTbbl ){
  avgDailyReturn <- stockDailyLogReturnTbbl %>% 
    pull(daily.returns) %>% 
    mean(na.rm=TRUE) %>% exp() 
  #On average, the mean daily return is avgDailyReturn-1 more than the 
  ## previous day’s price. Doesn’t sound like much, but it compounds daily at an exponential rate
  # #On average, the mean daily return
  # #but it compounds daily at an exponential rate.
  return( avgDailyReturn ) #returns in percentage 
}

simpleRatesTbbl <- function( stockTbbl ){
  nDays <-
    stockTbbl %>%
    select(adjusted) %>%
    count() 

  simpleRates <-
    stockTbbl %>%
    pull(adjusted) %>%
    diff() 
  
  return( simpleRates/nDays ) 
}

compoundReturnsTbbl <- function( stockTbbl, nDaysBackStart=90, nDaysBackEnd=0 ) {
  # rt the continuously compounded return at moment t
  # Rt simple return 
  # rt = ln(1+Rt) 
  # Pt is the price at the moment 
  # Pt-1 is the price a day prior 
  # rt = ln(Pt/Pt-1) 
  # ln(Pt/Pt-1) = ln(Pt) - ln(Pt-1)
  lastDate <- 
    stockTbbl %>%
    slice(n()) %>% 
    pull(date) %>%
    as.Date() - nDaysBackEnd 

  firstDate <- 
    as.Date( lastDate - nDaysBackStart ) 

  stockTbbl <- 
    stockTbbl %>% 
    filter( date >= firstDate, date <= lastDate )

  compoundReturn <- 
    stockTbbl %>% 
    pull(adjusted) %>% 
    log() %>% 
    diff() 

  return(compoundReturn) 

}

stopLossAanalysis <- function( stockTbbl, portfolioAmnt){
  closePrice <- 
    stockTbbl %>% 
    select( close ) %>% 
    tail(n=1) %>% 
    pull() 
  print("Closing Price" )
  print( closePrice )
  
  #get Daily Returns 
  daily.Returns.Tbbl <- 
    stockTbbl %>% 
    stockDailyLogReturnsTbbl() %>% 
    tail(n=30) 
  
  dailyCompoundReturn <- 
    daily.Returns.Tbbl %>%
    stockAvgDailyReturnRateTbbl() #average volatitilty
  
  print("Stock daily Average compound return: ")
  print( dailyCompoundReturn-1 )
  
  
  # get Return Metric
  stockReturnMetric <- 
    stockTbbl %>% 
    stockDailyLogReturnsTbbl %>%     
    stockDistributionMetricTbbl()
  
  print("Stock Daily Return Metrics:")
  print( stockReturnMetric)
  
  #getVolatility from previous 9 days 
  shortTermVolatility <- 
    stockTbbl %>% 
    stockSumVolatility(  n=9 ) %>%
    drop_na() %>%
    tail(n=30) #graph 
  
  maxPortfolioLoss <-
    portfolioAmnt * MAXSALELOSS
  
  totalPerQty <- 
    closePrice * purchaseQty
  dailyReturnMtx <- 
    matrix( 0, nrow=length(totalPerQty), ncol = length(stockReturnMetric) ) 
  colnames(dailyReturnMtx) <- 
    names( stockReturnMetric )
  rownames(dailyReturnMtx) <- purchaseQty
  
  for(ii in seq_along(totalPerQty)){
    dailyReturnMtx[ii,] <-  unname( totalPerQty[ii]*(stockReturnMetric) )
  }
  print("Rate of Return per Day per Qty Purchased: ")
  print( dailyReturnMtx )
  
}

## get the last row of tibble %>% slice(n()) 
