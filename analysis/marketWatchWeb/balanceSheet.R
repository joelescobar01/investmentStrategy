library(rvest)
library(dplyr)
library( stringr ) 
library(tidyverse)
library(httr)
source("analysis/sp500.R") 
source("analysis/utils.R")
source("analysis/marketWatchWeb/lib.R")

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

createHTMLSession <- function( url ){
  session <-
    html_session( url ) 
  if( http_status(session)$reason != "OK" ){
    print("Error connection to url")
    return(NA)
  }    
  return( session ) 
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

OLDyearBalanceSheetClean <- function( webTable ){
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
    mutate_at( vars("BalanceSheet"), ~ str_replace_all(., "&", "." ) ) %>% 
    map_df( ., ~billionConverter(.)) %>%  
    map_df( ., ~millionConverter(.)) %>% 
    mutate_at( vars(-"BalanceSheet"), ~ str_replace_all( ., ",", "" ) ) %>% 
    mutate_at( vars(-"BalanceSheet"), ~str_replace_all( .,"(\\()([0-9]*)(\\))", "-\\2" )) %>% 
    mutate_at( vars(-"BalanceSheet"), ~str_replace_all( .,"\\(|\\)", "" )) %>% 
    mutate_at( vars("BalanceSheet"),~ str_replace_all( ., "\\s+|\\.+", "" ) ) %>% 
    mutate_at( vars(-"BalanceSheet"), ~ as.numeric(.) )
    #mutate_if( is.numeric, ~ format(., scientific=F) )
  return( balanceSheetTable ) 
}

yearBalanceSheetClean <- function( webTable ){
  balanceSheetTable <- 
    webTable %>%
    map( ~ rename(.x, BalanceSheet=names(.)[1] ) ) %>% 
    reduce(bind_rows) %>% 
    mutate_at( vars("BalanceSheet"),~ str_replace_all( ., ",|\'|\\&|-|\\.+", "" ) ) %>% 
    mutate_at( vars("BalanceSheet"),~ str_replace_all( ., "\\s+", "_" ) ) %>% 
    #rename_at( vars(starts_with("20")), funs( paste0("year.",.) ) ) %>%      
    select( -starts_with("..."), -ends_with( "trend" ) ) %>% 
    pivot_longer( -BalanceSheet, names_to="year", values_to="values" ) %>% 
    pivot_wider( names_from=BalanceSheet, values_from="values" ) %>% 
    mutate_at( vars(-"year"), ~ billionConverter(.) )  %>% 
    mutate_at( vars(-"year"), ~ millionConverter(.) ) %>% 
    mutate_at( vars(-"year"), ~ str_replace_all(., "%", NA_character_ ) ) %>% 
    mutate_at( vars(-"year"), ~ str_replace_all( ., ",", "" ) ) %>% 
    mutate_at( vars(-"year"), ~str_replace_all( .,"(\\()([0-9]*)(\\))", "-\\2" )) %>% 
    mutate_at( vars(-"year"), ~str_replace_all( .,"\\(|\\)", "" )) %>% 
    mutate_at( vars(-"year"), ~ as.numeric(.) )
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
    mutate_at( vars(-"BalanceSheet"), ~ str_replace_all( ., ",", "" ) ) %>% 
    mutate_at( vars(-"BalanceSheet"), ~str_replace_all( .,"(\\()([0-9]*)(\\))", "-\\2" )) %>% 
    mutate_at( vars(-"BalanceSheet"), ~str_replace_all( .,"\\(|\\)", "" )) %>% 
    mutate_at( vars("BalanceSheet"),~ str_replace_all( ., "\\s+|\\.+", "" ) ) %>% 
    mutate_at( vars(-"BalanceSheet"), ~ as.numeric(.) )
  return( balanceSheetTable ) 
}

