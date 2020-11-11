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
    map(., ~ as_tibble(., .name_repair="unique" ) ) 
  return( webTable )
}

fetchTable2 <- function ( htmlSession ) {
  webTable <- 
    htmlSession %>% 
    read_html() %>% 
    html_nodes( "table" ) %>% 
    html_table(header = TRUE, fill = TRUE, trim=TRUE) %>% 
    map(., ~ as_tibble(., .name_repair="unique" ) %>% 
          select_if( ~!all(is.na(.)))) #delete all NA columns 
  return( webTable )
}

combineTables <- function( webTable ){
  dCol <- 
    webTable %>% 
    map( ~ rename(.x, defaultName=names(.)[1] ) ) %>% 
    reduce(bind_rows) %>% 
    select_if( ~sum(!is.na(.)) > 0 ) 
  return(dCol) 
}

combineTables2 <- function( webTable ){
  if( length(webTable) == 0 )
    return(NA) 
  dCol <- 
    webTable %>% 
    map( ~ rename(.x, defaultName=names(.)[1] ) ) %>% 
    reduce(bind_rows)  
  return(dCol) 
}


reshapeTable <- function( dCol ) {
  rTable <- 
    dCol %>% 
    mutate_at( vars("defaultName"), ~  
              str_replace_all( ., "/", " " ) %>%
              str_replace_all( "&", "" ) %>% 
              str_replace_all( "[:punct:]", "" ) %>% 
              str_replace_all( "[:blank:]+", "." )
              ) %>% 
    pivot_longer( -defaultName, names_to="period", values_to="values" ) %>% 
    pivot_wider( names_from=defaultName, values_from="values", values_fn=list ) 
  return(rTable ) 
}

tidyTable <- function( dCol ) {
  rTable <- 
    dCol %>% 
    pivot_longer( -defaultName, names_to="period", values_to="values" ) %>% 
    pivot_wider( names_from=defaultName, values_from="values" ) 
  return(rTable ) 
}

excelFormatTable <- function( dCol, outputColumn="item" ) {
  rTable <- 
    dCol %>% 
    pivot_longer( -period, names_to=outputColumn, values_to="values" ) %>% 
    pivot_wider( names_from=period, values_from=values ) 
  return(rTable ) 
}

excelFormatTable2 <- function( dCol ) {
  rTable <- 
    dCol %>% 
    pivot_longer( -period, names_to="defaultName", values_to="values" ) %>% 
    pivot_wider( names_from=period, values_from=values ) 
  return(rTable ) 
}


removeDashes <- function( rTable, cName="period" ) {
  dTable <- 
    rTable %>%
    mutate_at( vars(-cName), ~ str_replace_all( ., "-", NA_character_ ) ) 
  return(dTable ) 
}

replaceDashes <- function( rTable, cName="period" ) {
  dTable <- 
    rTable %>%
    mutate_at( vars(-cName), ~ str_replace_all( ., "-", "0O" ) ) 
  return(dTable ) 
}

removePercentage <- function( dTable, cName="period" ) {
  pTable <- 
    dTable %>% 
    mutate_at( vars(-cName ), ~ str_replace_all( ., "%", NA_character_ ) )
  return(pTable) 
}

convertFinanceFormat <- function( pTable, cName="period" ) {
  fFormat <- 
    pTable %>% 
    mutate_at( vars(-cName), ~ str_replace_all( .,"(\\()(.*)(\\))", "-\\2" ) ) %>% 
    mutate_at( vars(-cName), ~ numerateChar(.)) %>% 
    mutate_at( vars(-cName), replace_na, 0 ) 
  return(fFormat )
}

convertFinanceFormat2 <- function( pTable, cName="period" ) {
  fFormat <- 
    pTable %>% 
    mutate_at( vars(-cName), ~ str_replace_all( .,"(\\()(.*)(\\))", "-\\2" ) ) %>% 
    mutate_at( vars(-cName), ~ numerateChar(.)) 
  return(fFormat )
}


cleanRowItem <- function( dCol, cName="defaultName" ) {
  rTable <- 
    dCol %>% 
    mutate_at( vars(cName), ~  
              str_replace_all( ., "/", " " ) %>%
              str_replace_all( "&", "" ) %>% 
              str_replace_all( "[:punct:]", "" ) %>% 
              str_replace_all( "[:blank:]+", "." ) %>% 
              tolower()
              ) 
  return(rTable ) 
}

removeRatios <- function( fFormat ){
  eCol <- 
    fFormat %>% 
    filter_if( is.numeric, all_vars(!is.na(.)))
  return(eCol)
}

cleanTable <- function( fFormat, cName="period" ){
  eCol <- 
    fFormat %>% 
    mutate_at( vars(-cName), replace_na, 0 ) %>% 
    rename_all( tolower )

  return(eCol)
}

cleanTable2 <- function( fFormat, cName="period" ){
  eCol <- 
    fFormat %>% 
    mutate_at( vars(-cName), replace_na, 0 ) %>% 
    filter_if( is.numeric, any_vars(!. == 0 ) )
  return(eCol)
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
