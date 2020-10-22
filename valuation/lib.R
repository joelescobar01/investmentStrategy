library(tidyverse)
library(tidyquant) 
source("analysis/marketWatchWeb/financialStatementInteface.R" )
source("data.transfer.lib.R")

futureValue <- function( currentVal, interestRate, years=1 ){
  val <- 
    currentVal*(1+interestRate)^(years)
  return( val ) 
}

presentValue <- function( futureVal, interestRate, years=1 ){
  val <-
    futureVal/( 1+interestRate )^(years) 
  return(val)
}

annualReturn <- function( symbol ){
  annReturns <- 
    tq_get(symbol, 
           get='stock.prices' ) %>% 
    tq_transmute( adjusted, periodReturn, 
                  period="yearly", col_rename="annual.returns") %>% 
    mutate( year = year(date) ) %>% 
    mutate( period = as.character(year) ) %>% 
    select( -date, -year )
  return( annReturns ) 
}
