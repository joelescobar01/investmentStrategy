marketWatchWebPageURL <- "https://www.marketwatch.com" 
marketWatchWebPagePath <- "investing/stock" 
marketWatchWebPageQuery <-"financials"

marketWatchWebPageIncomeStatement <- "financials"
marketWatchWebPageIncomeStatementQuarter <- "financials/income/quarter"
marketWatchWebPageTableClass <- ".crDataTable"
marketWatchWebPageBalanceSheet <- "balance-sheet" 
marketWatchWebPageQuarterBalanceSheet <- "balance-sheet/quarter" 
marketWatchWebPageCashFlow <- "cash-flow"
marketWatchWebPageCashFlowQuarter <- "cash-flow/quarter"

incomeStatementURL <- function( symbol ){
  urlPath <- 
    paste( marketWatchWebPageURL, 
           marketWatchWebPagePath, 
           tolower(symbol), 
           marketWatchWebPageIncomeStatement, 
           sep="/")
  return(urlPath)
}

incomeStatementQuarterURL <- function( symbol ){
  urlPath <- 
    paste( marketWatchWebPageURL, 
           marketWatchWebPagePath, 
           tolower(symbol), 
           marketWatchWebPageIncomeStatementQuarter , 
           sep="/")
  return(urlPath)
}


cashFlowURL <- function( symbol ){
  urlPath <- 
    paste( marketWatchWebPageURL, 
           marketWatchWebPagePath, 
           tolower(symbol), 
           marketWatchWebPageQuery, 
           marketWatchWebPageCashFlow, 
           sep="/")  
  return(urlPath)
}

cashFlowQuarterURL <- function( symbol ){
  urlPath <- 
    paste( marketWatchWebPageURL, 
           marketWatchWebPagePath, 
           tolower(symbol), 
           marketWatchWebPageQuery, 
           marketWatchWebPageCashFlowQuarter, 
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

balanceSheetQuarterURL <- function( symbol ){
  urlPath <- 
    paste( marketWatchWebPageURL, 
           marketWatchWebPagePath, 
           tolower(symbol), 
           marketWatchWebPageQuery, 
           marketWatchWebPageQuarterBalanceSheet, 
           sep="/")  
  return(urlPath)
}

createHTMLSession <- function( urlPath ){
  htmlSession <-
    html_session( urlPath ) 
  if( http_status(htmlSession)$reason != "OK" ){
    print("Error connection to url")
    return(NA)
  }    
  return( htmlSession ) 
}


