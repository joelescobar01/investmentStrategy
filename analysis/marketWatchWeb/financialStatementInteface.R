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
    cleanTable() %>% 
    select_if( ~ !is.numeric(.) || sum(.) != 0 )
  return(docu)
}

financialStatementSeperated <- function( mwURL ){
  docu <- 
    mwURL %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    map( ~ rename(.x, defaultName=names(.)[1] ) %>%  
          select_if( ~sum(!is.na(.)) > 0 ) %>%  
          reshapeTable() %>% 
          removeDashes() %>% 
          removePercentage() %>% 
          convertFinanceFormat() %>% 
          cleanTable() %>% 
          select_if( ~ !is.numeric(.) || sum(.) != 0  ) )
    names(docu) <- 
      c("current.assets", "fixed.assets", "liabilities.shareholders.equity" )
  return(docu)
}


transposeFinancialStatement <- function( statement ){
  tranpo <- 
    statement %>% 
    pivot_longer( -period, names_to="item", values_to="usd") %>% 
    pivot_wider( names_from="period", values_from="usd") 
  return(tranpo ) 
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


