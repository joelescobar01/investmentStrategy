source("analysis/marketWatchWeb/lib.R")

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

incomeStatementQuarter <- function( symbol ){
  incomeTable <- 
    incomeStatementQuarterURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    quarterIncomeStatementClean()

  return( incomeTable ) 
}

incomeStatementYear <- function( symbol ){
  incomeTable <- 
    incomeStatementURL( symbol ) %>% 
    createHTMLSession() %>% 
    fetchTable() %>% 
    yearIncomeStatementClean() 

  return( incomeTable ) 
}

yearIncomeStatementClean <- function( webTable ){
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
    mutate_at( vars(-"IncomeStatement"), ~ str_replace_all( ., ",", "" ) ) %>%  
    mutate_at( vars(-"IncomeStatement"), ~str_replace_all( .,"(\\()([0-9]*)(\\))", "-\\2" )) %>% 
    mutate_at( vars(-"IncomeStatement"), ~str_replace_all( .,"\\(|\\)", "" )) %>% 
    mutate_at( vars("IncomeStatement"),~ str_replace_all( ., "\\s+|\\.+", "" ) ) %>% 
    mutate_at( vars(-"IncomeStatement"), ~ as.numeric(.) )   
   # mutate_if( is.numeric, ~ format(., scientific=F) )
  return( incomeTable ) 
}

yearIncomeStatementClean2 <- function( webTable ){
  incomeStatementTable <- 
    webTable %>%
    map( ~ rename(.x, IncomeStatement=names(.)[1] ) ) %>% 
    reduce(bind_rows) %>% 
    mutate_at( vars("IncomeStatement"),~ str_replace_all( ., ",|\'|\\&|-|\\.+", "" ) ) %>% 
    mutate_at( vars("IncomeStatement"),~ str_replace_all( ., "\\s+", "_" ) ) %>% 
    #rename_at( vars(starts_with("20")), funs( paste0("year.",.) ) ) %>%      
    select( -starts_with("..."), -ends_with( "trend" ) ) %>% 
    pivot_longer( -IncomeStatement, names_to="year", values_to="values" ) %>% 
    pivot_wider( names_from=IncomeStatement, values_from="values" ) %>% 
    mutate_at( vars(-"year"), ~ billionConverter(.) )  %>% 
    mutate_at( vars(-"year"), ~ millionConverter(.) ) %>% 
    mutate_at( vars(-"year"), ~ str_replace_all(., "%", NA_character_ ) ) %>% 
    mutate_at( vars(-"year"), ~ str_replace_all( ., ",", "" ) ) %>% 
    mutate_at( vars(-"year"), ~str_replace_all( .,"(\\()([0-9]*)(\\))", "-\\2" )) %>% 
    mutate_at( vars(-"year"), ~str_replace_all( .,"\\(|\\)", "" )) %>% 
    mutate_at( vars(-"year"), ~ as.numeric(.) ) %>% 
    select( ! matches("\\(|\\/|\\)", "") ) 
    #mutate_if( is.numeric, ~ format(., scientific=F) )
  return( incomeStatementTable ) 
}


quarterIncomeStatementClean <- function( webTable ){
  revenueTable <- 
    webTable[[1]] %>%
    rename_at( vars(starts_with("All")), funs(glue::glue("IncomeStatement")) ) %>% 
    rename_all( funs(str_replace(., "-", "." ) ) ) %>%  
    select( -starts_with("..."), -ends_with( "trend" ) )

  expenseTable <- 
    webTable[[2]] %>%   
    rename_at( vars("...1"), funs( glue::glue("IncomeStatement") ) ) %>% 
    rename_all( funs(str_replace(., "-", "." ) ) ) %>%
    select( -starts_with( "..." ), -ends_with("trend") )

  incomeTable <- 
    bind_rows( revenueTable, expenseTable ) %>%
    mutate_all(~ replace(., . == "-" , NA_character_)) %>% 
    mutate_at( vars(-"IncomeStatement"), ~ str_replace_all(., "%", NA_character_ ) ) %>% 
    map_df( ., ~billionConverter(.)) %>%  
    map_df( ., ~millionConverter(.)) %>%
    mutate_at( vars(-"IncomeStatement"), ~ str_replace_all( ., ",", "" ) ) %>%  
    mutate_at( vars(-"IncomeStatement"), ~str_replace_all( .,"(\\()([0-9]*)(\\))", "-\\2" )) %>% 
    mutate_at( vars(-"IncomeStatement"), ~str_replace_all( .,"\\(|\\)", "" )) %>% 
    mutate_at( vars("IncomeStatement"),~ str_replace_all( ., "\\s+|\\.+", "" ) ) %>% 
    mutate_at( vars(-"IncomeStatement"), ~ as.numeric(.) )   
   # mutate_if( is.numeric, ~ format(., scientific=F) )
  return( incomeTable ) 
}
