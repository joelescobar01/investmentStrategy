require(quantmod)
require(PerformanceAnalytics)
#source("../lib/utils.R")
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

stockDailyLogReturns <- function ( stock ){
  dailyLogReturns <- stock %>% 
    dailyReturn(type='log')  
  names( dailyLogReturns ) <- c("Log.Returns")
  return(dailyLogReturns)
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

mutateDailyLogReturnsTbbl <- function ( stockTbbl ){
  dailyLogReturnsV <- 
    stockTbbl %>% 
    tq_transmute(adjusted, 
                 mutate_fun = periodReturn, 
                 period = 'daily', 
                 type='log' ) %>%
    pull(daily.returns)
  stockTbbl <- 
    stockTbbl %>%
    add_column( Log.Returns = dailyLogReturnsV )
  return(stockTbbl )
}

stockSumVolatility <- function( stockTb, n=9 ){
  stockTb <- 
    stockTb %>% mutate( daily.returns = c(0,diff( log(close))))
  
  dailyReturns <- stockTb$daily.returns 
  
  vol <- c()
  beta <- c() 
  vol[1:(n-1)] <- NA
  beta[1:(n-1)] <- NA
  vol[n]<- sd(dailyReturns[1:n])
  for (i in (n+1):length(dailyReturns)){
    vol[i]<- sd(dailyReturns[(i-n):i])
  }
  stockTb <- 
    stockTb %>% 
    add_column(nDayVolatility = vol)
  #stockTb$nDayCumSumVol <- vol 
  return(stockTb)
}

changeRateInVolatility <- function( stockTb, n=9 ){
  deltaVol <- c()
  histVolatility <- 
    stockTb %>% 
    select(nDayVolatility) %>% 
    pull() 
  deltaVol[1:(n*2-1)] <- NA
  deltaVol[n*2]<- sd(histVolatility[n:n*2])
  for (i in (n*2+1):length(histVolatility)){
    currentDay<- histVolatility[i]
    pastDays<- sum( histVolatility[(i-n):i] )
    deltaVol[i] <- currentDay/pastDays 
  }
  stockTb$deltaVolatility <- deltaVol  
  return( stockTb )
}

myEMA <- function (stocktb,n){
  price <- 
    stocktb %>%
    select(close) %>%
    pull() 
  betaV <- 
    stocktb %>% 
    select(deltaVolatility) %>% 
    pull 
  ema <- c()
  ema[1:(n-1)] <- NA
  ema[n]<- mean(price[1:n])
  for (i in (n+1):length(price)){
    beta <- betaV[i] 
    ema[i]<-beta * price[i] + 
      (1-beta) * ema[i-1]
  }
  ema <- reclass(ema,price)
  return(ema)
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



tbill3MDailyRate <- function(  ){
  tbillLogRate <- 
    tq_get( "DGS3MO", 
           get="economic.data" ) %>%
    drop_na() 
  #tbillLogRate <- tbillLogRate[!is.infinite(rowSums(tbillLogRate)),] 
  #tbillLogRate <- na.omit( tbillLogRate )
  
  #tbillLogRate <- 
  #  tail( tbillLogRate, n=92 )
  #colnames(tbillLogRate) <- c( "Rate")
  #tbillDF <- zooToDataFrame(tbillLogRate)
  #
  #return(tbillLogRate)
}

tbill3MDailyLogtRate <- function( ){
  tbillLogRate <- 
    getSymbols( "DGS3MO", 
                src='FRED', 
                auto.assign = FALSE ) %>% 
    dailyReturn(type='log')
  
  tbillLogRate <- tbillLogRate[!is.infinite(rowSums(tbillLogRate)),] 
  tbillLogRate <- na.omit( tbillLogRate )
  
  tbillLogRate <- 
    tail( tbillLogRate, n=92 )
  colnames(tbillLogRate) <- c( "Log.Returns")
  tbillDF <- convertStockToDataFrame(tbillLogRate)
  
  return(tbillLogRate)
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

getStockTbbl <- function( stockSymbolChar, ... ){
  stockTbbl <- tq_get( toupper(stockSymbolChar),
                        get="stock.prices",
                        ... )
  return(stockTbbl) 
}

week13Rates <- function( dailyRate ){
  dailyRate <- 
    tail( dailyRate, n=92 )
  colnames(dailyRate) <- c( "Rate")
  dailyRateDF <- zooToDataFrame( dailyRate)
  return(dailyRate) 
}

dailyRiskFreeRates <- function( tbillDailyRates ){
  #ep <- endpoints(prices, "months")
  dailyYield <- (1+(tbill/100))^(1/252) - 1
 # threeMoPrice <- cumprod(1+dailyYield)
  return(dailyYield)
#  threeMoPrice <- threeMoPrice["2019-12-16::"]
#  threeMoPrice <- threeMoPrice[endpoints(threeMoPrice, "days"),]
#  return(threeMoPrice)  
}

priceSimulationMC <- function( stock ){
  # Parameters
  stock <- 
    stock %>% 
    adjustOHLC(use.Adjusted=TRUE )
  
  price_init <- last(stock) 
  dailyLogReturns <- stockDailyLogReturns(stock)  
  mean_log_returns <- mean(dailyLogReturns, na.rm = TRUE)
  sd_log_returns <- sd(dailyLogReturns, na.rm = TRUE)
  
  N     <- 252 # Number of Stock Price Simulations
  M     <- 250  # Number of Monte Carlo Simulations
  mu    <- mean_log_returns
  sigma <- sd_log_returns
  day <- 1:N
  # Simulate prices
  #price_init <- stock$CAT.Adjusted[[nrow(stock$CAT.Adjusted)]]
  set.seed(123)
  monte_carlo_mat <- matrix(nrow = N, ncol = M)
  for (j in 1:M) {
    monte_carlo_mat[[1, j]] <- price_init
    for(i in 2:N) {
      monte_carlo_mat[[i, j]] <- monte_carlo_mat[[i - 1, j]] * exp(rnorm(1, mu, sigma))
    }
  }
 
  # Format and organize data frame
  price_sim <- cbind(day, monte_carlo_mat) %>%
    as_tibble() 
  nm <- str_c("Sim.", seq(1, M))
  nm <- c("Day", nm)
  names(price_sim) <- nm
  price_sim <- price_sim %>%
    gather(key = "Simulation", value = "Stock.Price", -(Day))
  end_stock_prices <- price_sim %>% 
    filter(Day == max(Day))
  probs <- c(.005, .025, .25, .5, .75, .975, .995)
  dist_end_stock_prices <- quantile(end_stock_prices$Stock.Price, probs = probs)
  dist_end_stock_prices %>% round(2)
  
  # Inputs
  N_hist          <- nrow(stock) / 252
  p_start_hist    <- first(Ad(stock))
  p_end_hist      <-last(Ad(stock))
  N_sim           <- N / 252
  p_start_sim     <- p_end_hist
  p_end_sim       <- dist_end_stock_prices[[4]]
  # CAGR calculations
  CAGR_historical <- (p_end_hist / p_start_hist) ^ (1 / N_hist) - 1
  CAGR_sim        <- (p_end_sim / p_start_sim) ^ (1 / N_sim) - 1
   
  # Format and organize data frame
  price_sim <- cbind(day, monte_carlo_mat) %>%
    as_tibble()
  nm <- str_c("Sim.", seq(1, M))
  nm <- c("Day", nm)
  names(price_sim) <- nm
  price_sim <- price_sim %>%
    gather(key = "Simulation", value = "Stock.Price", -(Day))
  
  startDate <- paste( "Using data since", first( index(stock), "to simulate" ))
  capt <- paste("Historical Annual Growth: ", CAGR_historical, "% Simulated Annual Growth: ", CAGR_sim, "%" )
  # Visualize simulation
  p <- chartPriceSimulationMC(price_sim, caption=capt) +
    labs( caption=capt, subtitle = startDate, y="Stock Price" )
  
  plot(p)
  
  return(price_sim)   
}

sp500ListWebScrape <- function( ){
  sp_500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
    html_node("#constituents") %>%
    html_table() %>%
    select('Symbol','Security', 'GICS Sector', 'GICS Sub Industry') %>%
    as_tibble()
  names(sp_500) <- c("symbol", "security", "sector", "sub-industry") 
  #Remove periods 
  sp_500 <- 
    sp_500 %>% 
    mutate( symbol = gsub("\\.", "-", symbol) )
  sp_500 %>%
    lapply(function(x) x %>% unique() %>% length()) %>% 
    unlist() # show in condensed format
  return(sp_500)
}

getSP500Sector <- function( sp_500, Sector ){
  sector <- 
    sp_500 %>%
    filter( sector == Sector )
  stockSymbols <- sp_500 %>%
    select(symbol) 
  return(sector)
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
