library(rvest)
library(dplyr)
library( stringr ) 
library(tidyverse)
library(httr)
source("analysis/sp500.R") 
source("analysis/utils.R")


#ns <- namespace::makeNamespace("marketWatch")
#assign("test", 7, env = ns)
#base::namespaceExport(ns, ls(ns))


marketWatchWebPageURL <- "https://www.marketwatch.com" 
marketWatchWebPagePath <- "investing/stock" 
marketWatchWebPageQuery <-"financials" 

marketWatchWebPageTableClass <- ".crDataTable"
marketWatchWebPageBalanceSheet <- "balance-sheet" 
marketWatchWebPageQuarterBalanceSheet <- "balance-sheet/quarter" 
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

quaterlyBalanceSheetURL <- function( symbol ){
  urlPath <- 
    paste( marketWatchWebPageURL, 
           marketWatchWebPagePath, 
           tolower(symbol), 
           marketWatchWebPageQuery, 
           marketWatchWebPageQuarterBalanceSheet, 
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

quarterBalanceSheetClean <- function( webTable ){
  currentTable <- 
    webTable[[1]] %>%
    rename_at( vars(contains("USD")), funs(glue::glue("BalanceSheet")) ) %>% 
    rename_all( funs(str_replace(., "-", "." ) ) ) %>%  
    select( -ends_with( "trend" ) )

  totalTable <- 
    webTable[[2]] %>%   
    rename_at( vars("...1"), funs( glue::glue("BalanceSheet") ) ) %>% 
    rename_all( funs(str_replace(., "-", "." ) ) ) %>%
    select( -ends_with("trend") )

  liabilitiesTable <- 
    webTable[[3]] %>%   
    rename_at( vars("...1"), funs( glue::glue("BalanceSheet") ) ) %>% 
    rename_all( funs(str_replace(., "-", "." ) ) ) %>%
    select( -ends_with("trend") )
 
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
  return( balanceSheetTable ) 
}
