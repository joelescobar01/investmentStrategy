library(tidyverse)

options(scipen = 999)   

removeNACol <- function( tbbl ) {
  rColTable <- 
    tbbl %>% 
    select_if( ~ all(!is.na(.)) )
  return(rColTable)
}

fetchTable <- function ( htmlSession ) {
  webTable <- 
    htmlSession %>% 
    read_html() %>% 
    html_nodes( "table" ) %>% 
    html_table(header = TRUE, fill = TRUE, trim=TRUE) %>% 
    map(., ~ as_tibble(., .name_repair="unique" ) ) %>%
    map( ., ~ select_if( .x, ~sum(!is.na(.)) > 0 ) ) 
  return( webTable )
}

combineTables <- function( webTable ){
  dCol <- 
    webTable %>% 
    map( ~ rename(.x, defaultName=names(.)[1] ) ) %>% 
    reduce(bind_rows)
  return(dCol) 
}

reshapeTable <- function( dCol ) {
  rTable <- 
    dCol %>% 
    mutate_at( vars("defaultName"), ~ str_replace_all(., "[:punct:]", "" ) %>% 
                                      str_replace_all( "[:blank:]+", "_" ) 
              ) %>% 
    pivot_longer( -defaultName, names_to="period", values_to="values" ) %>% 
    pivot_wider( names_from=defaultName, values_from="values" ) 

  return(rTable ) 
}

removeDashes <- function( rTable ) {
  dTable <- 
    rTable %>%
    mutate_at( vars(-"period"), ~ str_replace_all( ., "-", NA_character_ ) ) 
  return(dTable ) 
}

removePercentage <- function( dTable ) {
  pTable <- 
    dTable %>% 
    mutate_at( vars(-"period"), ~ str_replace_all( ., "%", NA_character_ ) )
  return(pTable) 
}

convertFinanceFormat <- function( pTable ) {
  fFormat <- 
    pTable %>% 
    mutate_at( vars(-"period"), ~ str_replace_all( .,"(\\()(.*)(\\))", "-\\2" ) %>% 
                                  numerateChar()) 
  return(fFormat )
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


numericalIndicator <- c(  "O" = 10^(0),
                          "T" = 10^(3),
                          "M" = 10^(6), 
                          "B" = 10^(9) ) 

charToNumeric <- function( numberString ) {
  num <- 
    numberString %>% 
    str_remove("\\$" ) %>% 
    str_extract("[:alpha:]" ) %>% 
    replace_na("O" ) %>% 
    map_dbl( ~ numericalIndicator[[.]] ) * as.numeric( str_extract( t, "-?\\d+" ) )
    
    return( num ) 
}

numerateChar <- function( colum ){
  digMatrix <-  
    colum %>% 
    str_remove("\\$" ) %>% 
    str_split_fixed( "(?<=[0-9])(?=[A-Z])", n=2 )
  
  placeValue <- 
    digMatrix[,2] %>% 
    str_extract( "[:alpha:]") %>% 
    replace_na( "O" ) %>% 
    map_dbl( ~ numericalIndicator[[.]]) 

  digit <- 
    as.numeric( digMatrix[,1] ) 

  return( digit*placeValue ) 
}
