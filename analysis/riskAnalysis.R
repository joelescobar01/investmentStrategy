require(quantmod)
require(PerformanceAnalytics)
require(tibble)
require(lubridate)
require(tidyquant)
require(dplyr)
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
    tq_mutate(adjusted, 
                 mutate_fun = periodReturn, 
                 period = 'daily', 
                 type='log' ) 
  return(dailyLogReturnsTbbl)
}

stockDistributionMetric <- function( stockDailyLogReturnTbbl ){
  #probs <- c(.005, .025, .05, .25, .5, .75, .975, .995)
  dist_log_returns <- 
    stockDailyLogReturnTbbl %>% 
    select( daily.returns ) %>% 
    pull() %>% exp() %>% 
    quantile(probs = probs, na.rm = TRUE)
  return( dist_log_returns )
}
stockDistributionMetricTbbl <- function( stockDailyLogReturnTbbl ){
  #probs <- c(.005, .025, .05, .25, .5, .75, .975, .995)
  dist_returns <- 
    stockDailyLogReturnTbbl %>% 
    select( daily.returns ) %>% 
    pull() %>% exp() %>% 
    quantile(probs = probs, na.rm = TRUE) %>% 
    as_tibble_row() %>% 
    mutate( qty=c(1) ) 
  return( dist_returns )
}

stockAvgDailyReturnRate <- function( stockDailyLogReturnTbbl ){
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

PastCumulativeReturns <- function( stockTbbl, nDays=13, principalAmt=1){
  returnRate <- 
    stockTbbl %>% 
    stockDailyLogReturnsTbbl %>%
    last(n=nDays ) %>% 
    select( date, daily.returns ) 
  
  rate <- 
    returnRate %>% 
    select( daily.returns ) %>% 
    pull 

  returnTbbl <- 
    returnRate %>% 
    add_column( cummalative.return = cumprod( principalAmt + rate ) ) 

  return( returnTbbl ) 
}

ChartPastCummulativeReturns <- function( returnTbbl ){
  
  #roE <- returnTbbl %>% first %>% select( cummalative.return ) %>% pull() 
  roE <- paste( "Cummulative Return ($USD)" ) 
  g1 <- 
    returnTbbl %>% 
    ggplot() + 
    geom_line( aes( x=date, y=cummalative.return) ) + 
    labs( x='Date', y=roE )+ 
    scale_x_date( breaks='5 days',
                  minor_breaks='1 days',
                  date_labels='%b-%d') +
    scale_y_continuous(position = "right") +
    theme() 

  return(g1) 
}

TimePeriodVolatility <- function( symbol ){
  stock <- 
    tq_get( symbol, 
           get="stock.prices", 
           from ="2017-01-01" ) %>% 
    tq_mutate( select=adjusted,
                mutate_fun = periodReturn, 
                period="daily", 
                type="log" ) %>% 
    group_by( year=year(date) ) %>% 
    summarize( annual.volatility = sd(daily.returns)/sqrt(1/n()) ) %>% 
    mutate( monthly.volatility = annual.volatility*sqrt(1/12) ) %>%
    mutate( weekly.volatility = annual.volatility*sqrt(1/52) ) 
  return( stock ) 
}

ChartTimePeriodVolatility <- function( periodVolTbl ){
 p1 <- 
  periodVolTbl %>% 
  gather("Time.Period", "Volatility", -year ) %>% 
     ggplot( aes(x=year, y=Volatility, fill=Time.Period ) ) +
     geom_bar( position="dodge", stat="identity") + 
     scale_fill_discrete( labels=c("Annual", "Monthly", "Weekly") ) +
     theme() 
  return(p1) 
}


MaxQtyBuy <- function( closePrice, maxAmt ){
  qtyCount <- 0
  while( maxAmt > closePrice ){
    maxAmt <- maxAmt - closePrice 
    qtyCount <- qtyCount + 1 
  }
  return(qtyCount) 
}

stopLossAnalysis <- function( stockTbbl ){
  # get Return Metric
  stockReturnMetric <- 
    stockTbbl %>% 
    stockDailyLogReturnsTbbl %>%     
    stockDistributionMetricTbbl 
  
  roE <- stockReturnMetric 

  return( roE )  
}

DailyAnnualReturnRate <- function( stockTbbl){
  #get Daily Returns 
  dailyReturns <- 
    stockTbbl %>% 
    stockDailyLogReturnsTbbl() %>% 
    stockAvgDailyReturnRate 
  
  return( dailyReturns ) 
}


## get the last row of tibble %>% slice(n()) 
