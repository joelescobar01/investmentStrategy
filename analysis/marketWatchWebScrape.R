library(rvest)
library(dplyr)
library( stringr ) 
library(tidyverse)
library(httr)
source("analysis/sp500.R") 
source("analysis/utils.R")

marketWatchWebPageURL <- "https://www.marketwatch.com" 
marketWatchWebPagePath <- "investing/stock" 
marketWatchWebPageQuery <-"financials" 

marketWatchWebPageTableClass <- ".crDataTable"
marketWatchWebPageBalanceSheet <- "balance-sheet" 
marketWatchWebPageCashFlowSheet <- "cash-flow"


grossIncomeFundamental <- function( symbol ) {
  chartAnalysis <- 
    incomeStatementURL(symbol) %>% 
    createHTMLSession() %>% 
    marketWatchTableClass() %>% 
    marketWatchTableClean() %>% 
    marketWatchGrossIncomeTable()
  return( chartAnalysis ) 
}

netIncomeFundamental <- function( symbol ) {
  chartAnalysis <- 
    incomeStatementURL(symbol) %>% 
    createHTMLSession() %>% 
    marketWatchTableClass() %>% 
    marketWatchTableClean(n=2) %>% 
    marketWatchNetIncomeTable()
  return( chartAnalysis ) 
}

incomeStatement <- function( symbol ){
  chart1 <- 
    grossIncomeFundamental( symbol )
  chart2 <- 
    netIncomeFundamental( symbol ) 
  incomeTable <- 
    full_join( chart1, chart2, by="year" ) 
  return(incomeTable ) 
}

currentAssetFundamental <- function( symbol ) {
  chartAnalysis <- 
    balanceSheetURL(symbol) %>% 
    createHTMLSession() %>% 
    marketWatchTableClass() %>% 
    marketWatchTableClean() %>% 
    marketWatchCurrentAssetTable()
  return( chartAnalysis ) 
}

totalAssetFundamental <- function( symbol ) {
  chartAnalysis <- 
    balanceSheetURL(symbol) %>% 
    createHTMLSession() %>% 
    marketWatchTableClass() %>% 
    marketWatchTableClean(n=2) %>% 
    marketWatchTotalAssetTable()
  return( chartAnalysis ) 
}

liabilitiesFundamental <- function( symbol ) {
  chartAnalysis <- 
    balanceSheetURL(symbol) %>% 
    createHTMLSession() %>% 
    marketWatchTableClass() %>% 
    marketWatchTableClean(n=3) %>% 
    marketWatchLiabilityTable()
  return( chartAnalysis ) 
}

balanceSheet <- function( symbol ) {
  chart1 <- 
    currentAssetFundamental( symbol )
  chart2 <- 
    totalAssetFundamental( symbol ) 
  chart3 <- 
    liabilitiesFundamental( symbol ) 

  balanceTable <- 
    full_join( chart1, chart2, by="year" ) %>% 
    full_join( chart3, by="year" ) 

  return( balanceTable ) 
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
    if( tableDF[-1,ii] %>% str_detect("%") %>% any() ){ #remove the rowname 
      next() 
    } else {
      newCol <- 
        tableDF[-1,ii] %>%  #-1 is market watch row name
        replaceBillion() %>% 
        replaceMillion() %>%
        str_replace_all( "-", NA_character_ ) %>% 
        str_replace_all( "(\\()([0-9]*)(\\))", "-\\2" ) %>%   #looses 
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
      "COGS.DA", "depreciation.amorization", 
      "depreciation", "amortization", "gross.income" ) 
  return(incomeTbl) 
}

marketWatchNetIncomeTable <- function( incomeTbl ){
  names( incomeTbl ) <- c("year", 
                          "SGA.expense", 
                          "research.development", 
                          "other.SGA", 
                          "other.operating.expeses",
                          "unusual.expenses", 
                          "EBIT.after.unusual.expenses",
                          "non.operating.income.expense",
                          "non.operating.interest.income",
                          "equity.affiliates.pretax",
                          "interest.expenses", 
                          "gross.interest.expenses", 
                          "interest.capitalized",
                          "pretax.income", 
                          "income.tax", 
                          "income.tax.current.domestic", 
                          "income.tax.current.foreign", 
                          "income.tax.deferred.domestic",
                          "income.tax.deferred.foreign",
                          "income.tax.credits",
                          "equity.in.affiliates",
                          "other.after.tax.income.expenses",
                          "consolidated.net.income", 
                          "minority.interest.expenses", 
                          "net.income", 
                          "extraordinaries.discontinued.operations",
                          "extra.items.gain.loss.sales.of.assets",
                          "cummulative.efecs.accounting.chg",
                          "dicontinued.operations",
                          "net.income.after.extraordinaries", 
                          "preferred.dividends",
                          "net.income.available.to.common", 
                          "eps.basic", 
                          "basic.shares.outstanding",
                          "eps.diluted", 
                          "diluted.shares.outstanding", 
                          "EBITDA" ) 

  return(incomeTbl) 
}

marketWatchCurrentAssetTable <- function( balanceSheetTbl ){
  names( balanceSheetTbl ) <- 
    c( "year", 
      "cash.short.term.investment", 
      "cash.only",
      "short.term.investments", 
      "total.account.receivable",
      "account.receivable.net", 
      "account.receivable.gross", 
      "bad.debt.doubtful.accounts",
      "other.receivables", 
      "accounts.receivable.turnover", 
      "inventories", 
      "finished.goods",
      "working.progress", 
      "raw.materials", 
      "progress.payments.other",
      "other.current.assets", 
      "misc.current.assets",
      "total.current.assets" ) 
  return( balanceSheetTbl ) 
}

marketWatchTotalAssetTable <- function( balanceSheetTbl ){
  names( balanceSheetTbl ) <- 
    c( "year", 
      "net.property.plant.equip", 
      "property.plant.gross", 
      "buildings", 
      "land.improvements", 
      "computer.software.equip",
      "other.property.plant.equip", 
      "accumulated.depreciation",
      "total.investement.advances",
      "other.long.term.investments",
      "long.term.note.receivable",
      "intangible.assets",
      "net.goodwill", 
      "net.other.intangibles",
      "other.assets",
      "tangible.other.assets",
      "total.assets" ) 
  return( balanceSheetTbl ) 
}

marketWatchLiabilityTable <- function( balanceSheetTbl ){
  names( balanceSheetTbl ) <- 
  c( "year", 
      "short.term.debt.current.portion.ltdebt",
      "short.term.debt",
      "current.portionof.long.term.debt",
      "accounts.payable",
      "income.tax.payable", 
      "other.current.liabilities",
      "dividends.payable",
      "accrued.payroll",
      "miscellaneous.current.liabilities",
      "total.current.liabilities",
      "long.term.debt",
      "long.term.debt.excl.capitalized.leases",
      "non.convertible.debt",
      "convertible.debt",
      "capitalized.lease.obligations",
      "provision.for.risks.charges",
      "deferred.taxes",
      "deferred.taxes.credit",
      "deferred.taxes.debit",
      "other.liabilities",
      "other.liabilities.excl.deferred.income",
      "deferred.income", 
      "total.liabilities",
      "non.equity.reserves",
      "preferred.stock.carrying.value",
      "redeemable.preferred.stock",
      "non.redeemable.preferred.stock",
      "common.equity.total",
      "common.stock.par.carry.value",
      "retained.earnings",
      "ESOP.debt.guarantee",
      "cumulative.translation.adjustment.unrealized.for.exch.gain",
      "unrealized.gain.loss.marketable.securities",
      "revalutation.reservers",
      "treasury.stock",
      "total.shareholders.equity",
      "accumulated.minority.interest",
      "total.equity",
      "liabilities.shareholders.equity" )
  return( balanceSheetTbl ) 
}
