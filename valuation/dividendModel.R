source("valuation/lib.R")
getQuarterlyRevenue <- function( symbol ){
  incomeStatement <- 
    incomeStatementQuarter(symbol) %>% 
    select( date, 'Sales/Revenue', Basic_Shares_Outstanding, Preferred_Dividends, Net_Income_Available_to_Common ) %>%
    rename( Revenue = 'Sales/Revenue' ) %>% 
    mutate( quarter = quarter( date ), year = year(date) ) %>%
    select( -date ) 
  return( incomeStatement ) 
}

getDividend <- function( symbol ){
  dividend <- 
    yahoo.Dividend.Payout(symbol, from='2019-01-01') %>% 
    mutate( quarter = quarter( date ), 
           year=year(date)  ) %>%
    select( -date) 
  return( dividend ) 
}

getDDMData <- function( symbol ) {
  dv <- 
    getDividend(symbol) 
  rv <- 
    getQuarterlyRevenue(symbol) 
  ddm <- 
    left_join( dv, rv, by=c("quarter", "year" ) ) 

  return( ddm ) 
} 
