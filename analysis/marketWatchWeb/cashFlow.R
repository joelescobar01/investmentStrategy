source("analysis/marketWatchWeb/lib.R")

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

cashFlowYear <- function( symbol ){
  cashFlow <- 
    cashFlowURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    yearCashFlowClean() 

  return( cashFlow ) 
}

cashFlowQuarter <- function( symbol ){
  cashFlow <- 
    cashFlowQuarterURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    quarterCashFlowClean() 

  return( cashFlow ) 
}

yearCashFlowClean <- function( webTable ){
  currentTable <- 
    webTable[[1]] %>%
    rename_at( vars(starts_with("Fiscal")), funs(glue::glue("CashFlow")) ) %>% 
    rename_at( vars(starts_with("20")), funs( paste0("year.",.) ) ) %>%      
    select( -starts_with("..."), -ends_with( "trend" ) )

  totalTable <- 
    webTable[[2]] %>%   
    rename_at( vars("...1"), funs( glue::glue("CashFlow") ) ) %>% 
    rename_at( vars(starts_with("20")), funs( paste0("year.",.) ) ) %>%
    select( -starts_with( "..." ), -ends_with("trend") )

  liabilitiesTable <- 
    webTable[[3]] %>%   
    rename_at( vars("...1"), funs( glue::glue("CashFlow") ) ) %>% 
    rename_at( vars(starts_with("20")), funs( paste0("year.",.) ) ) %>%
    select( -starts_with( "..." ), -ends_with("trend") )


  balanceSheetTable <- 
    bind_rows( currentTable, totalTable ) %>% 
    bind_rows( liabilitiesTable ) %>% 
    mutate_all(~ replace(., . == "-" , NA_character_)) %>%
    mutate_at( vars(-"CashFlow"), ~ str_replace_all(., "%", NA_character_ ) ) %>% 
    map_df( ., ~billionConverter(.)) %>%  
    map_df( ., ~millionConverter(.)) %>% 
    mutate_at( vars(-"CashFlow"), ~ str_replace_all( ., ",", "" ) ) %>% 
    mutate_at( vars(-"CashFlow"), ~str_replace_all( .,"(\\()([0-9]*)(\\))", "-\\2" )) %>% 
    mutate_at( vars(-"CashFlow"), ~str_replace_all( .,"\\(|\\)", "" )) %>% 
    mutate_at( vars("CashFlow"),~ str_replace_all( ., "\\s+|\\.+", "" ) ) %>% 
    mutate_at( vars(-"CashFlow"), ~ as.numeric(.) )
    #mutate_if( is.numeric, ~ format(., scientific=F) )
  return( balanceSheetTable ) 
}


yearCashFlowClean2 <- function( webTable ){
  cashFlowTable <- 
    webTable %>%
    map( ~ rename(.x, CashFlow=names(.)[1] ) ) %>% 
    reduce(bind_rows) %>% 
    mutate_at( vars("CashFlow"),~ str_replace_all( ., ",|\'|\\&|-|\\.+", "" ) ) %>% 
    mutate_at( vars("CashFlow"),~ str_replace_all( ., "\\s+", "_" ) ) %>% 
    #rename_at( vars(starts_with("20")), funs( paste0("year.",.) ) ) %>%      
    select( -starts_with("..."), -ends_with( "trend" ) ) %>% 
    pivot_longer( -CashFlow, names_to="year", values_to="values" ) %>% 
    pivot_wider( names_from=CashFlow, values_from="values" ) %>% 
    mutate_at( vars(-"year"), ~ billionConverter(.) )  %>% 
    mutate_at( vars(-"year"), ~ millionConverter(.) ) %>% 
    mutate_at( vars(-"year"), ~ str_replace_all(., "%", NA_character_ ) ) %>% 
    mutate_at( vars(-"year"), ~ str_replace_all( ., ",", "" ) ) %>% 
    mutate_at( vars(-"year"), ~str_replace_all( .,"(\\()([0-9]*)(\\))", "-\\2" )) %>% 
    mutate_at( vars(-"year"), ~str_replace_all( .,"\\(|\\)", "" )) %>% 
    mutate_at( vars(-"year"), ~ as.numeric(.) ) %>% 
    select( ! matches("\\(|\\/|\\)", "") ) 
    #mutate_if( is.numeric, ~ format(., scientific=F) )
  return( cashFlowTable ) 
}

quarterCashFlowClean <- function( webTable ){
  currentTable <- 
    webTable[[1]] %>%
    rename_at( vars(starts_with("All")), funs(glue::glue("CashFlow")) ) %>% 
    rename_all( funs(str_replace(., "-", "." ) ) ) %>%
    select( -starts_with("..."), -ends_with( "trend" ) )

  totalTable <- 
    webTable[[2]] %>%   
    rename_at( vars("...1"), funs( glue::glue("CashFlow") ) ) %>% 
    rename_all( funs(str_replace(., "-", "." ) ) ) %>%
    select( -starts_with( "..." ), -ends_with("trend") )

  liabilitiesTable <- 
    webTable[[3]] %>%   
    rename_at( vars("...1"), funs( glue::glue("CashFlow") ) ) %>% 
    rename_all( funs(str_replace(., "-", "." ) ) ) %>%
    select( -starts_with( "..." ), -ends_with("trend") )


  balanceSheetTable <- 
    bind_rows( currentTable, totalTable ) %>% 
    bind_rows( liabilitiesTable ) %>% 
    mutate_all(~ replace(., . == "-" , NA_character_)) %>%
    mutate_at( vars(-"CashFlow"), ~ str_replace_all(., "%", NA_character_ ) ) %>% 
    map_df( ., ~billionConverter(.)) %>%  
    map_df( ., ~millionConverter(.)) %>% 
    mutate_at( vars(-"CashFlow"), ~ str_replace_all( ., ",", "" ) ) %>% 
    mutate_at( vars(-"CashFlow"), ~str_replace_all( .,"(\\()([0-9]*)(\\))", "-\\2" )) %>% 
    mutate_at( vars(-"CashFlow"), ~str_replace_all( .,"\\(|\\)", "" )) %>% 
    mutate_at( vars("CashFlow"),~ str_replace_all( ., "\\s+|\\.+", "" ) ) %>% 
    mutate_at( vars(-"CashFlow"), ~ as.numeric(.) )
    #mutate_if( is.numeric, ~ format(., scientific=F) )
  return( balanceSheetTable ) 
}


