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

marketWatchWebPageIncomeStatement <- "financials"
marketWatchWebPageIncomeStatementQuarter <- "financials/income/quarter"
marketWatchWebPageTableClass <- ".crDataTable"
marketWatchWebPageBalanceSheet <- "balance-sheet" 
marketWatchWebPageQuarterBalanceSheet <- "balance-sheet/quarter" 
marketWatchWebPageCashFlow <- "cash-flow"
marketWatchWebPageCashFlowQuarter <- "cash-flow/quarter"


createHTMLSession <- function( urlPath ){
  htmlSession <-
    html_session( urlPath ) 
  if( http_status(htmlSession)$reason != "OK" ){
    print("Error connection to url")
    return(NA)
  }    
  return( htmlSession ) 
}

fetchTable <- function ( htmlSession ) {
  webTable <- 
    htmlSession %>% 
    read_html() %>% 
    html_nodes( "table" ) %>% 
    html_table(header = TRUE, fill = TRUE, trim=TRUE) %>% 
    map(., ~ as_tibble(., .name_repair="unique" ) ) 
  
  return( webTable )
}


