source("analysis/marketWatchWeb/incomeStatement.R" )


shares.Outstanding <- function( ticker ){
  shareCount <- 
    incomeStatementURL( ticker ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    yearIncomeStatementClean2() %>% 
    select( Basic_Shares_Outstanding ) 
  return( shareCount ) 
}

preferred.Dividends.Currency <- function( ticker ){ 
  pDividend <- 
    incomeStatementURL( ticker ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    yearIncomeStatementClean2() %>% 
    select( year, Preferred_Dividends ) 
  return( pDividend ) 
}

available.Common.Equity <- function( ticker ) {
  commonE <- 
    incomeStatementURL( ticker ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    yearIncomeStatementClean2() %>% 
    select( Net_Income_Available_to_Common ) 
  return( commonE )  
}

