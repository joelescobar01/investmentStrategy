source("analysis/marketWatchWeb/financialStatementInteface.R")

priceToProfit <- function( ticker ){
  marketCap <- 
    get.Financial.Ratios(ticker) %>% 
    select( Market.Capitilization ) %>% 
    pull

  ebitda <- 
    incomeStatementQuarter(ticker) %>% 
    select( ebitda ) %>% 
    last %>% 
    pull

  pricePerEbitda <- 
    marketCap / ebitda 

  return( pricePerEbitda ) 
}

