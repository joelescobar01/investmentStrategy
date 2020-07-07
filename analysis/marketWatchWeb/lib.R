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

cleanTable <- function( webTable ){
  mwTable <- 
    webTable %>%
    map( ~ rename(.x, defaultName=names(.)[1] ) ) %>% 
    reduce(bind_rows) %>% 
    mutate_at( vars("defaultName"),~ str_replace_all( ., ",|\'|\\&|-|\\.+", "" ) ) %>% 
    mutate_at( vars("defaultName"),~ str_replace_all( ., "\\s+", "_" ) ) %>% 
    #rename_at( vars(starts_with("20")), funs( paste0("period.",.) ) ) %>%      
    select( -starts_with("..."), -ends_with( "trend" ) ) %>% 
    pivot_longer( -defaultName, names_to="period", values_to="values" ) %>% 
    pivot_wider( names_from=defaultName, values_from="values" ) %>% 
    mutate_at( vars(-"period"), ~ billionConverter(.) )  %>% 
    mutate_at( vars(-"period"), ~ millionConverter(.) ) %>% 
    mutate_at( vars(-"period"), ~ str_replace_all(., "%", NA_character_ ) ) %>% 
    mutate_at( vars(-"period"), ~ str_replace_all( ., ",", "" ) ) %>% 
    mutate_at( vars(-"period"), ~str_replace_all( .,"(\\()([0-9]*)(\\))", "-\\2" )) %>% 
    mutate_at( vars(-"period"), ~str_replace_all( .,"\\(|\\)", "" )) %>% 
    mutate_at( vars(-"period"), ~ as.numeric(.) )

    #mutate_if( is.numeric, ~ format(., scientific=F) )
  return( mwTable ) 
}


