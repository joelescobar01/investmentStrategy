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


