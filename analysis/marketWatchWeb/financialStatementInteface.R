source('data.transfer.lib.R')
library(rvest)
library(httr)
source('visual.lib.R')
source("analysis/marketWatchWeb/webTable.R")
source("analysis/marketWatchWeb/marketWatchHTTP.R")

wikiSP500URL <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies" 

wikiSP500 <-
  wikiSP500URL %>% 
  createHTMLSession() %>% 
  fetchTable2() %>% 
  first %>% 
  flatten_dfr() %>% 
  rename_all( ~ str_replace_all(., "\\s+|-", "." ) %>% tolower  ) %>% 
  rename( company = security ) %>% 
  rename_at( vars( starts_with("gics.")), ~ str_replace_all(., "gics.", ""))



financialStatement <- function( mwURL ){
  docu <- 
    mwURL %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    combineTables() %>% 
    reshapeTable() %>% 
    replaceDashes() %>% 
    removePercentage() %>% 
    convertFinanceFormat() %>% 
    cleanTable() %>% 
    select_if( ~ !is.numeric(.) || sum(.) != 0 )
  return(docu)
}

financialStatement2 <- function( mwURL ){
  docu <- 
    mwURL %>% 
    createHTMLSession() %>% 
    fetchTable2() %>% 
    combineTables() %>% 
    removeDashes("defaultName") %>% 
    removePercentage("defaultName") %>% 
    convertFinanceFormat("defaultName") %>%
    cleanRowItem() %>% 
    cleanTable2("defaultName")  
  return(docu)
}

financialStatementRatioLess <- function( mwURL) {
  docu <-
    mwURL %>% 
    createHTMLSession() %>% 
    fetchTable2() %>% 
    combineTables2() %>% 
    replaceDashes("defaultName") %>% 
    removePercentage("defaultName") %>% 
    convertFinanceFormat2("defaultName") %>% 
    cleanRowItem() %>% 
    removeRatios
  return( docu ) 
}

financialStatementRatioLess2 <- function( mwURL) {
  docu <-
    mwURL %>% 
    createHTMLSession() %>% 
    fetchTable2() %>% 
    combineTables2() %>% 
    replaceDashes("defaultName") %>% 
    removePercentage("defaultName") %>% 
    convertFinanceFormat2("defaultName") %>% 
    cleanRowItem() %>% 
    removeRatios %>% 
    rename_at( -1 ,~ c("year1", "year2", "year3", "year4", "year5" )  )
  return( docu ) 
}

financialStatementCashFlow  <- function( mwURL ){
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
  
  statement <- 
    docu[[1]] %>% 
    rename_at( vars(-period), ~ paste0( .x, ".operating.activities") ) 
  statement <- 
    docu[[2]] %>% 
    rename_at( vars(-period), ~ paste0( .x, ".investing.activities") ) %>% 
    right_join( statement, by='period') 
  statement <- 
    docu[[3]] %>% 
    rename_at( vars(-period), ~ paste0( .x, ".financing.activities") ) %>% 
    right_join( statement, by='period' ) 

  return( statement )
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

fullyCleanTable <- function( fTable ){ 
  cTable <- 
    fTable %>% 
    reshapeTable() %>% 
    removeDashes() %>% 
    removePercentage() %>% 
    convertFinanceFormat() %>% 
    cleanTable() %>% 
    select_if( ~ !is.numeric(.) || sum(.) != 0 )
  return( cTable )
}

transposeFinancialStatement <- function( statement ){
  tranpo <- 
    statement %>% 
    pivot_longer( -period, names_to="item", values_to="usd") %>% 
    pivot_wider( names_from="period", values_from="usd") 
  return(tranpo ) 
}


