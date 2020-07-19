source("data.transfer.lib.R")
source("economy/economy.R") 


sp500AnnualDiff <- function( symbol=c(), fromDate=ymd("2015-01-01") ) {
  stock <-
    symbol %>% 
    tq_get( get='stock.prices', from=fromDate ) %>% 
    group_by( symbol, year=year(date) ) %>% 
    filter( date==min(date)|date==max(date) ) %>% 
    select( symbol, date, close, adjusted, year ) %>% 
    summarise_at( c('adjusted'), diff ) 
 
  pp <- 
    economyIndicators( fromDate=fromDate ) %>% 
    purchasingPower() 
  
  stock <- 
    left_join( stock, pp, by="year" ) 

  return( stock ) 
}

sp500Dividends <- function( symbol=c(),fromDate=ymd("2015-01-01"), sampleSize=20 ) {
  stockDividends <- 
    symbol %>%
    yahoo.Dividend.Payout( from=fromDate ) %>% 
    group_by( symbol, year=year(date) ) %>% 
    summarize( annual.dividend=sum( dividend) ) 
  
  return(stockDividends) 
}


