source("analysis/marketWatchWeb/balanceSheet.R")
source("analysis/marketWatchWeb/cashFlow.R")
source("analysis/marketWatchWeb/incomeStatement.R")

incomeStatementQuarter <- function( symbol ){
  incomeTable <- 
    incomeStatementQuarterURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    quarterIncomeStatementClean()

  return( incomeTable ) 
}

incomeStatementYear <- function( symbol ){
  incomeTable <- 
    incomeStatementURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    cleanTable()  

  return( incomeTable ) 
}

balanceSheetYear <- function( symbol ){
  balanceTable <- 
    balanceSheetURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    cleanTable()  
  return( balanceTable ) 
}

balanceSheetQuarter <- function( symbol ){
  incomeTable <- 
    balanceSheetQuarterURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    quarterBalanceSheetClean()

  return( incomeTable ) 
}

cashFlowYear <- function( symbol ){
  cashFlow <- 
    cashFlowURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    cleanTable() 

  return( cashFlow ) 
}

cashFlowQuarter <- function( symbol ){
  cashFlow <- 
    cashFlowQuarterURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    quarterCashFlowClean() 

  return( cashFlow ) 
}


