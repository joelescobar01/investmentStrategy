require(quantmod)
require(PerformanceAnalytics)
source("utils.R")
source("charting.R")
require(forecast)
require(purrr)
require(tibble)
require(lubridate)
require(tidyquant)
require(dplyr)
require(rvest)
require(tidyverse)
# width="600px" border="0" cellspacing="0" cellpadding="0" bgcolor="#336699"

probs <- c(.005, .025, .05, .25, .5, .75, .975, .995)

stockDailyLogReturns <- function ( stock ){
  dailyLogReturns <- stock %>% 
    dailyReturn(type='log')  
  names( dailyLogReturns ) <- c("Log.Returns")
  return(dailyLogReturns)
}
stockDistributionMetric <- function( stockDailyLogReturn ){
  #probs <- c(.005, .025, .05, .25, .5, .75, .975, .995)
  dist_log_returns <- stockDailyReturn %>% 
    quantile(probs = probs, na.rm = TRUE)
  return( dist_log_returns )
}
stockAvgDailyReturnRate <- function( stock ){
  dailyLogReturns <- historicStockDailyReturns(stock)  
  mean_log_returns <- mean(dailyLogReturns, na.rm = TRUE)
#  sd_log_returns <- sd(dailyLogReturns, na.rm = TRUE)
  #On average, the mean daily return 
  dailyReturnP <- 
    mean_log_returns %>% 
    exp() 
  # #On average, the mean daily return
  # #but it compounds daily at an exponential rate.
  return( dailyReturnP ) #returns in percentage 
}

tbill3MDailyRate <- function( ){
  tbillLogRate <- 
    getSymbols( "DGS3MO", 
                src='FRED', 
                auto.assign = FALSE )
  
  tbillLogRate <- tbillLogRate[!is.infinite(rowSums(tbillLogRate)),] 
  tbillLogRate <- na.omit( tbillLogRate )
  
  tbillLogRate <- 
    tail( tbillLogRate, n=92 )
  colnames(tbillLogRate) <- c( "Rate")
  tbillDF <- zooToDataFrame(tbillLogRate)
  
  return(tbillLogRate)
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
  dailyLogReturns <- historicStockDailyReturns(stock)  
  mean_log_returns <- mean(dailyLogReturns, na.rm = TRUE)
  sd_log_returns <- sd(dailyLogReturns, na.rm = TRUE)
  
  N     <- 252 # Number of Stock Price Simulations
  M     <- 250  # Number of Monte Carlo Simulations
  mu    <- mean_log_returns
  sigma <- sd_log_returns
  day <- 1:N
  #price_init <- stock$CAT.Adjusted[[nrow(stock$CAT.Adjusted)]]
  # Simulate prices
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
  # Visualize simulation
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

chartSP500SectorReturnDistribution <- function( sp_500Sector ){
  sp500 <- sp500ListWebScrape()
  sp500Material <- getSP500Sector( sp500, "Materials" ) 
  symbols <-
    sp500Material %>%
    select(symbol) %>% 
    pull() 
  for(ii in seq_along(symbols)){
    stockRe <- getStock(symbols[ii])
    annualReturn <- stock %>%
    as_tibble() %>%
    rownames_to_column(var="Date") %>%
    mutate( Date = as.Date(index(stockRe)) ) %>% 
    mutate( Log.Returns = coredata( periodReturn(stockRe, period='daily', type='log') ) )
    names(annualReturn ) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adusted", "Log.Returns" ) 
    
    volatilityMeasure <- 
      annualReturn %>% 
      select(Log.Returns) %>%
      stockVolatilityDailyMeasure( )    
  }
}

