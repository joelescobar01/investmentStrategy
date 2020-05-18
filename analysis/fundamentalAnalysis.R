library(rvest)
library(dplyr)
library( stringr ) 
library(tidyverse)
library(httr)
prevDir <- getwd() 
setwd("/home/joel/Documents/stocks/analysis")
source("financialRatios.R")
source("sp500.R") 
setwd( prevDir )

# incomeStatementURL(ticker) %>% createHTMLSession() %>%
# marketWatchTableClass() %>% marketWatchTableClean2(n=2) %>%
# marketWatchNetIncomeTable() 


#balanceSheetURL("AA") %>% createHTMLSession() %>% marketWatchTableClass() %>%
#marketWatchTableClean2(n=1) 

marketWatchWebPageURL <- "https://www.marketwatch.com" 
marketWatchWebPagePath <- "investing/stock" 
marketWatchWebPageQuery <-"financials" 

marketWatchWebPageTableClass <- ".crDataTable"
marketWatchWebPageBalanceSheet <- "balance-sheet" 
marketWatchWebPageCashFlowSheet <- "cash-flow"

replaceBillion <- function( columnVector ){
  columnVector <- 
    columnVector %>%  
    str_replace( "([0-9]{1,2}).([0-9]{2})B", "\\1\\20000000") %>% 
    str_replace( "([0-9]{1,2}).([0-9]{1})B", "\\1\\200000000") %>% 
    str_replace( "([0-9]{1,3})B", "\\100000000")
  return( columnVector ) 
}

replaceMillion <- function( columnVector ){
  #order MATTERS 
  columnVector <- columnVector %>%  
    str_replace( "([0-9]{1,3}).([0-9]{2})M", "\\1\\20000") %>% 
    str_replace( "([0-9]{1,3}).([0-9]{1})M", "\\1\\200000") %>% 
    str_replace( "([0-9]{1,3})M", "\\1000000")
  return( columnVector ) 
}

incomeStatementURL <- function( symbol ){
  urlPath <- 
    paste( marketWatchWebPageURL, 
           marketWatchWebPagePath, 
           tolower(symbol), 
           marketWatchWebPageQuery, 
           sep="/")
  return(urlPath)
}

balanceSheetURL <- function( symbol ){
  urlPath <- 
    paste( marketWatchWebPageURL, 
           marketWatchWebPagePath, 
           tolower(symbol), 
           marketWatchWebPageQuery, 
           marketWatchWebPageBalanceSheet, 
           sep="/")  
  return(urlPath)
}

liabilitiesURL <- function( symbol ){
  urlPath <- 
    paste( marketWatchWebPageURL, 
          marketWatchWebPagePath, 
          tolower(symbol), 
          marketWatchWebPageQuery, 
          marketWatchWebPageBalanceSheet, 
          sep="/")
  return(urlPath)
}

createHTMLSession <- function( url ){
  session <-
    html_session( url ) 
  if( http_status(session)$reason != "OK" ){
    print("Error connection to url")
    return(NA)
  }    
  return( session ) 
}

marketWatchTableClass <- function ( htmlSession ) {
  incomeStatement <-
    htmlSession %>%  #status_code to check it  
    read_html()%>% 
    html_table( ".crDataTable", header=TRUE )
  return( incomeStatement )
}

marketWatchTableClean <- function( tableStatementList, n=1 ){
  tableDF <-
    tableStatementList[[n]] %>% 
    select(matches("[0-9]{3,5}")) %>% 
    t()
    
  rowValues <- 
    tableStatementList[[n]][1] %>%
    map( function(x) str_replace_all(x, "[\\s{1,}\\W]", "" ) ) 

  rowValues <-
    rowValues[[1]]

  mwTbbl <- 
    tableDF %>% 
    rownames() %>%
    as_tibble_col( column_name="Year" ) %>% 
    slice(-1) 
  
  for(ii in 1:ncol( tableDF ) ){
    if( tableDF[-1,ii] %>% str_detect("-|%") %>% any() ){ #remove the rowname 
      next() 
    } else {
      newCol <- 
        tableDF[-1,ii] %>%  #-1 is market watch row name
        str_replace_all( "\\(|\\)", "" ) %>% 
        replaceBillion() %>% 
        replaceMillion() %>%
        as.numeric() %>%  
        as_tibble_col( column_name=rowValues[ii] ) 
      mwTbbl <- 
        bind_cols( mwTbbl, newCol )
    }
  }
  return( mwTbbl ) 
}

marketWatchGrossIncomeTable <- function( incomeTbl ){
  names( incomeTbl ) <- 
    c("year", "revenue", "COGS", 
      "COGS.xDA", "depreciation.amorization", 
      "depreciation", "amortization", "gross.income" ) 
  return(incomeTbl) 
}

marketWatchNetIncomeTable <- function( incomeTbl ){
  names( incomeTbl ) <- c("year", 
                          "SGA.expense", "research.development", "other.SGA", 
                          "unusual.expenses", "EBIT.after.unusual.expenses", 
                          "non.operating.income.expense", "equity.affiliates.pretax",
                          "interest.expenses", "gross.interest.expenses", "interest.capitalized",
                          "pretax.income", "income.tax", "income.tax.current.domestic", 
                          "income.tax.current.foreign", "income.tax.deferred.foreign",
                          "consolidated.net.income", "minority.interest.expenses", 
                          "net.income", "net.income.after.extraordinaries", 
                          "net.income.available.to.common", "eps.basic", "basic.shares.outstanding",
                          "eps.diluted", "diluted.shares.outstanding", "EBITDA" ) 

  return(incomeTbl) 
}

marketWatchCurrentAssetTable <- function( balanceSheetTbl ){
  names( balanceSheetTbl ) <- 
    c( "year", "cash.short.term.investment", "cash.only", 
      "total.account.receivable", "account.receivable.net", "account.receivable.gross", 
      "other.receivables", "accounts.receivable.turnover", "inventories", "finished.goods",
      "working.progress", "raw.materials", "other.current.assets", "misc.current.assets",
      "total.current.assets" ) 
  return( balanceSheetTbl ) 
}

marketWatchTotalAssetTable <- function( balanceSheetTbl ){
  names( balanceSheetTbl ) <- 
    c( "year", "net.property.plant.equip", "property.plant", "land.improvements",  
      "other.property", "accumulated.depreciation", "total.investment.advances", 
      "other.long.term.investment", "intangible.asset", "net.goodwill", "net.other.intangibles",
      "other.assets", "tangible.other.assets", "total.assets" ) 
  return( balanceSheetTbl ) 
}
