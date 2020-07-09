source('data.transfer.lib.R')
source('visual.lib.R')
source("analysis/marketWatchWeb/webTable.R")
source("analysis/marketWatchWeb/marketWatchHTTP.R")

incomeStatementQuarter <- function( symbol ){
  incomeTable <- 
    incomeStatementQuarterURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>%
    combineTables() %>%
    reshapeTable() %>% 
    removeDashes() %>% 
    removePercentage() %>% 
    convertFinanceFormat() %>% 
    removeNACol() %>% 
    rename( revenue = SalesRevenue ) 
  return( incomeTable ) 
}

incomeStatementYear <- function( symbol ){
  incomeTable <- 
    incomeStatementURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    combineTables() %>%
    reshapeTable() %>% 
    removeDashes() %>% 
    removePercentage() %>% 
    convertFinanceFormat() %>% 
    removeNACol() %>% 
    rename( revenue = SalesRevenue )
  return( incomeTable ) 
}

balanceSheetYear <- function( symbol ){
  balanceTable <- 
    balanceSheetURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    combineTables() %>% 
    reshapeTable() %>% 
    removeDashes() %>% 
    removePercentage() %>% 
    convertFinanceFormat() %>% 
    removeNACol()  

  return( balanceTable ) 
}

balanceSheetQuarter <- function( symbol ){
  incomeTable <- 
    balanceSheetQuarterURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    combineTables() %>% 
    reshapeTable() %>% 
    removeDashes() %>% 
    removePercentage() %>% 
    convertFinanceFormat() %>% 
    removeNACol()  
  return( incomeTable ) 
}

cashFlowYear <- function( symbol ){
  cashFlow <- 
    cashFlowURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    combineTables() %>%
    reshapeTable() %>% 
    removeDashes() %>% 
    removePercentage() %>% 
    convertFinanceFormat() %>% 
    removeNACol()  
  return( cashFlow ) 
}

cashFlowQuarter <- function( symbol ){
  cashFlow <- 
    cashFlowQuarterURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    combineTables() %>%
    reshapeTable() %>% 
    removeDashes() %>% 
    removePercentage() %>% 
    convertFinanceFormat() %>% 
    removeNACol()  
  return( cashFlow ) 
}


