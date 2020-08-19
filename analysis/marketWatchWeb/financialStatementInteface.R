source('data.transfer.lib.R')
library(rvest)
library(httr)
source('visual.lib.R')
source("analysis/marketWatchWeb/webTable.R")
source("analysis/marketWatchWeb/marketWatchHTTP.R")

financialStatement <- function( mwURL ){
  docu <- 
    mwURL %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    combineTables() %>% 
    reshapeTable() %>% 
    removeDashes() %>% 
    removePercentage() %>% 
    convertFinanceFormat() %>% 
    cleanTable()
  return(docu)
}

incomeStatementYear <- function( symbol ){
  statement <- 
    symbol %>% 
    incomeStatementURL() %>% 
    financialStatement() 
  return( statement ) 
}

incomeStatementQuarter <- function( symbol ){
  statement <- 
    symbol %>% 
    incomeStatementQuarterURL() %>% 
    financialStatement() 
  return( statement ) 
}

balanceSheetYear <- function( symbol ){
  statement <- 
    symbol %>% 
    balanceSheetURL() %>% 
    financialStatement() 
  return( statement ) 
}

balanceSheetQuarter <- function( symbol ){
  statement <- 
    symbol %>% 
    balanceSheetQuarterURL() %>% 
    financialStatement() 
  return( statement ) 
}

balanceSheetQuarter2 <- function( symbol=NA ){
  if( is.na(symbol) )
    return(NA) 
  statement <- 
    symbol %>% 
    balanceSheetQuarterURL() %>% 
    financialStatement() 
  return( statement ) 
}
cashFlowYear <- function( symbol ){
  statement <- 
    symbol %>% 
    cashFlowURL() %>% 
    financialStatement() 
  return( statement ) 
}

cashFlowQuarter <- function( symbol ){
  statement <- 
    symbol %>% 
    cashFlowQuarterURL() %>% 
    financialStatement() 
  return( statement ) 
}

companyFinancialDocumentsYearly <- function( company ){
  incomeS <- 
    company %>% 
    incomeStatementYear()
  balanceS <- 
    company %>% 
    balanceSheetYear()   
  cashFlowS <- 
    company %>% 
    cashFlowYear()

 document <- 
   list(  company = company, 
          income.statement = incomeS,
          balance.sheet = balanceS, 
          cash.flow = cashFlowS ) 

  return( document )
}


