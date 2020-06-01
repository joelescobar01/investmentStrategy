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

grossIncomeFinanceFundamental <- function( symbol ) {
  chartAnalysis <- 
    incomeStatementURL(symbol) %>% 
    createHTMLSession() %>% 
    marketWatchTableClass() %>% 
    marketWatchTableClean() %>% 
    marketWatchFinanceIncomeStatement()
  return( chartAnalysis ) 
}

netIncomeFinanceFundamental <- function( symbol ) {
  chartAnalysis <- 
    incomeStatementURL(symbol) %>% 
    createHTMLSession() %>% 
    marketWatchTableClass() %>% 
    marketWatchTableClean(n=2) %>% 
    marketWatchFinanceNetIncomeStatement()
  return( chartAnalysis ) 
}

incomeStatement <- function( symbol ){
  chart1 <- 
    grossIncomeFundamental( symbol )
  chart2 <- 
    netIncomeFundamental( symbol ) 
  incomeTable <- 
    full_join( chart1, chart2, by="year" ) %>% 
    mutate_if( is.numeric, scales::dollar_format() ) 
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

incomeStatement <- function( symbol ){
  incomeTable <- 
    incomeStatementURL( symbol ) %>% 
    createHTMLSession() %>% 
    marketWatchTableClass2() %>% 
    marketWatchIncomeStatementClean()

  return( incomeTable ) 
}

balanceSheet <- function( symbol ){
  incomeTable <- 
    balanceSheetURL( symbol ) %>% 
    createHTMLSession() %>% 
    marketWatchTableClass2() %>% 
    marketWatchBalanceSheetClean()

  return( incomeTable ) 
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

marketWatchTableClass2 <- function ( htmlSession ) {
  webTable <- 
    htmlSession %>% 
    read_html() %>% 
    html_nodes( "table" ) %>% 
    html_table(header = TRUE, fill = TRUE, trim=TRUE) %>% 
    map(., ~ as_tibble(., .name_repair="unique" ) ) 
  
  return( webTable )
}


marketWatchIncomeStatementClean <- function( webTable ){
  revenueTable <- 
    webTable[[1]] %>%
    rename_at( vars(starts_with("fiscal")), funs(glue::glue("IncomeStatement")) ) %>% 
    rename_at( vars(starts_with("20")), funs( paste0("year.",.) ) ) %>%      
    select( -starts_with("..."), -ends_with( "trend" ) )

  expenseTable <- 
    webTable[[2]] %>%   
    rename_at( vars("...1"), funs( glue::glue("IncomeStatement") ) ) %>% 
    rename_at( vars(starts_with("20")), funs( paste0("year.",.) ) ) %>%
    select( -starts_with( "..." ), -ends_with("trend") )

  incomeTable <- 
    bind_rows( revenueTable, expenseTable ) %>%
    mutate_all(~ replace(., . == "-" , NA_character_)) %>% 
    mutate_at( vars(-"IncomeStatement"), ~ str_replace_all(., "%", NA_character_ ) ) %>% 
    map_df( ., ~billionConverter(.)) %>%  
    map_df( ., ~millionConverter(.)) %>%
    drop_na() %>%
    mutate_at( vars(-"IncomeStatement"), ~ str_replace_all( ., ",", "" ) ) %>%  
    mutate_at( vars(-"IncomeStatement"), ~str_replace_all( .,"(\\()([0-9]*)(\\))", "-\\2" )) %>% 
    mutate_at( vars(-"IncomeStatement"), ~str_replace_all( .,"\\(|\\)", "" )) %>% 
    mutate_at( vars("IncomeStatement"),~ str_replace_all( ., "\\s+|\\.+", "" ) ) %>% 
    mutate_at( vars(-"IncomeStatement"), ~ as.numeric(.) )   
   # mutate_if( is.numeric, ~ format(., scientific=F) )
  return( incomeTable ) 
}

marketWatchBalanceSheetClean <- function( webTable ){
  currentTable <- 
    webTable[[1]] %>%
    rename_at( vars(starts_with("Fiscal")), funs(glue::glue("BalanceSheet")) ) %>% 
    rename_at( vars(starts_with("20")), funs( paste0("year.",.) ) ) %>%      
    select( -starts_with("..."), -ends_with( "trend" ) )

  totalTable <- 
    webTable[[2]] %>%   
    rename_at( vars("...1"), funs( glue::glue("BalanceSheet") ) ) %>% 
    rename_at( vars(starts_with("20")), funs( paste0("year.",.) ) ) %>%
    select( -starts_with( "..." ), -ends_with("trend") )

  liabilitiesTable <- 
    webTable[[3]] %>%   
    rename_at( vars("...1"), funs( glue::glue("BalanceSheet") ) ) %>% 
    rename_at( vars(starts_with("20")), funs( paste0("year.",.) ) ) %>%
    select( -starts_with( "..." ), -ends_with("trend") )


  balanceSheetTable <- 
    bind_rows( currentTable, totalTable ) %>% 
    bind_rows( liabilitiesTable ) %>% 
    mutate_all(~ replace(., . == "-" , NA_character_)) %>%
    mutate_at( vars(-"BalanceSheet"), ~ str_replace_all(., "%", NA_character_ ) ) %>% 
    map_df( ., ~billionConverter(.)) %>%  
    map_df( ., ~millionConverter(.)) %>% 
    drop_na() %>%
    mutate_at( vars(-"BalanceSheet"), ~ str_replace_all( ., ",", "" ) ) %>% 
    mutate_at( vars(-"BalanceSheet"), ~str_replace_all( .,"(\\()([0-9]*)(\\))", "-\\2" )) %>% 
    mutate_at( vars(-"BalanceSheet"), ~str_replace_all( .,"\\(|\\)", "" )) %>% 
    mutate_at( vars("BalanceSheet"),~ str_replace_all( ., "\\s+|\\.+", "" ) ) %>% 
    mutate_at( vars(-"BalanceSheet"), ~ as.numeric(.) )
    #mutate_if( is.numeric, ~ format(., scientific=F) )
  return( balanceSheetTable ) 
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

marketWatchFinanceNetIncomeStatement <- function( incomeTbl ){
  names( incomeTbl ) <-
    c(  "interest.income", "interest.and.fees.on.loans",
        "interest.income.on.fed.funds", "interest.income.on.fed.funds", 
        "interest.income.on.fed.repos", "interest.on.bank.deposits", 
        "other.interest.or.dividend.income", "interest.income.growth", 
        "total.interest.expenses", "interest.expense.on.bank.deposits", 
        "interest.capitalized", "other.borrowed.funds", 
        "total.interest.expense.growth")
  return( incomeTbl ) 
}

marketWatchFinanceNetIncomeStatement <- function( incomeTbl ){
  names(incomeTbl ) <- 
    c( "year"
      ,"net.interest.income"
      ,"loan.loss.provision"
      ,"net.interest.income.after.provision"
      ,"non.interest.income"
      ,"securities.gain"
      ,"trading.account.income"
      ,"trust.income.commissions.fees"
      ,"commission.fee.income"
      ,"other.operating.income"
      ,"non.interest.expense"
      ,"labor.related.expense"
      ,"equipment.expense"
      ,"other.operating.expense"
      ,"operating.income"
      ,"non.operating.income.expense"
      ,"non.operating.interest.income"
      ,"miscellaneous.non.operating.expense"
      ,"equityin.affiliates.pretax"
      ,"unusual.expense"
      ,"pretax.income"
      ,"income.taxes"
      ,"income.tax.current.domestic"
      ,"income.tax.current.foreign"
      ,"income.tax.deferred.domestic"
      ,"income.tax.deferred.foreign"
      ,"income.tax.credits"
      ,"equity.in.affiliates"
      ,"other.after.tax.income.expense"
      ,"consolidated.net.income"
      ,"minority.interest.expense"
      ,"net.income"
      ,"extraordinaries.discontinued.operations"
      ,"extra.items.gain.loss.sale.of.assets"
      ,"cumulative.effect.accounting.chg"
      ,"discontinued.operations"
      ,"net.income.after.extraordinaries"
      ,"preferred.dividends"
      ,"net.income.availableto.common"
      ,"EPS.basic"
      ,"basic.shares.outstanding"
      ,"EPS.diluted"
      ,"diluted.shares.outstanding" )
  return( incomeTbl ) 
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
