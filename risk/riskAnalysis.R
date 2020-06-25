source("data.transfer.lib.R") 
source("var/economicIndicators.R")
source("risk/riskFreeAssets.R")

daily.Returns <- function( stock ){
  stockReturn <-
    stock %>% 
    tq_mutate(  adjusted, 
                mutate_fun=periodReturn, 
                period='daily' )
    return( stockReturn ) 
}

inflation.Returns <- function( stockReturn, inflationRate ){
  inflationReturn <-
    (stockReturn+1)/(inflationRate+1) 
  return( inflationReturn-1 )
}

inflation.Adjusted.Returns <- function( stock ){
  inflationTable <-
    stock %>% 
    slice( c(1,n()) ) %>% 
    select( date ) %>% 
    pull %>% 
    as.list() %>% 
    inflation.Rate2() %>% 
    rename( month = date ) %>% 
    select( c(-symbol ) ) 
  inflationReturns <- 
    stock %>%
    daily.Returns() %>% 
    group_by( month = floor_date( date, unit="month") ) %>% 
    left_join( inflationTable, by="month") %>% 
    mutate( inflation.adjusted.returns = ((1+daily.returns)/(1+rate) - 1 )) %>% 
    ungroup()
  return( inflationReturns ) 
}

WeeklyPriceAnalysis <- function( stockTbbl ){
  price <-
    stockTbbl %>% 
    mutate( daily.returns = close-lag(close) )  %>% 
    group_by( symbol, week = week(date), year=year(date) ) %>% 
    summarize( weeks.mean = mean( daily.returns ), 
                weeks.median=median(daily.returns), 
                weeks.stdev = sd(daily.returns), 
                weeks.lowhigh.stdev=sd(high-low), 
                weeks.middle.50 = IQR(close, na.rm=TRUE), 
                weeks.returns.middle.50=IQR(daily.returns, na.rm=TRUE)  ) %>% 
    mutate( weeks.skew = 3*(weeks.mean-weeks.median)/weeks.stdev ) 
    
    return( price ) 
}

WeeklyPriceChange <- function( stockTbbl ){
# quantile, or percentile, tells you how much of your data lies below a 
  # certain value. The 50 percent quantile, for example, is the same as the median
  quant <- 
    stockTbbl %>% 
    stockDailyLogReturnsTbbl %>% 
    group_by( weeks  = week(date) ) %>% 
    summarize( week.gain = mean( daily.returns ) ) %>% 
    select( daily.returns )  %>% 
    pull() %>% 
    quantile( probs=c(0.2, 0.5, 0.8), na.rm=TRUE )

  return( quant ) 
}

stockDistributionMetric <- function( stockDailyLogReturnTbbl ){
  #probs <- c(.005, .025, .05, .25, .5, .75, .975, .995)
  dist_log_returns <- 
    stockDailyLogReturnTbbl %>% 
    select( daily.returns ) %>% 
    pull() %>% exp() %>% 
    quantile(probs = probs, na.rm = TRUE)
  return( dist_log_returns ) }


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

ExcessReturn <- function(stockTbbl){
  excessR <- 
    stockTbbl %>% 
    tq_transmute(adjusted, periodReturn, period = "daily") %>%
    tq_transmute(daily.returns, Return.clean, alpha = 0.05) %>%
    tq_transmute(daily.returns, Return.excess, Rf = RISKFREERATE, col_rename="excess.return")
  return(excessR)
}

SharpeRatio <- function( excessReturnTbbl ){
  sharpeRatio <-
    excessReturnTbbl$excess.return 

  sharpeRatio <-
    mean( sharpeRatio ) / sd( sharpeRatio ) 

  return(sharpeRatio ) 
}

stockAvgDailyReturnRate <- function( stockDailyLogReturnTbbl ){
  avgDailyReturn <- 
    stockDailyLogReturnTbbl %>% 
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

TimePeriodVolatility2 <- function( stockTbbl ){
  stockVolatility <- tryCatch({ 
    stockTbbl %>% 
      tq_mutate( select=adjusted,
                mutate_fun = periodReturn, 
                period="daily", 
                type="log" ) %>% 
      group_by( year=year(date) ) %>% 
      summarize( annual.volatility = sd(daily.returns)/sqrt(1/n()) ) %>% 
      mutate( monthly.volatility = annual.volatility*sqrt(1/12) ) %>%
      mutate( weekly.volatility = annual.volatility*sqrt(1/52) ) 
  }, error = function(err){
    return(NA) },
    finally = { 
      print("Could Not Calculate Time Period Volatility" )
    }) 
    
  return( stockVolatility ) 
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
