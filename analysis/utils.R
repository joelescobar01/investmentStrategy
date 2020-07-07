

options(scipen = 999)   
numericalIndicator <- c(  "O" = 10^(0),
                          "T" = 10^(3),
                          "M" = 10^(6), 
                          "B" = 10^(9) ) 

charToNumeric <- function( numberString ) {
  num <- 
    numberString %>% 
    str_remove( "\\$" ) %>% 
    str_extract( "[^\\.|\\d+]") %>%  
    replace_na( "O" ) %>% 
    map_dbl( ~ numericalIndicator[[.x]] ) * as.numeric( str_extract( numberString, "\\d+\\.*\\d*" ) )  
 
  return( num ) 
}

replaceBillion <- function( columnVector ){
  columnVector <- 
    columnVector %>%  
    str_replace( "([0-9]{1,2}).([0-9]{2})B", "\\1\\20000000") %>% 
    str_replace( "([0-9]{1,2}).([0-9]{1})B", "\\1\\200000000") %>% 
    str_replace( "([0-9]{1,3})B", "\\100000000")
  return( columnVector ) 
}

billionConverter <- function( columnVector ){
  columnVector <- 
    columnVector %>%  
    str_replace( "([0-9]{1,2}).([0-9]{2})B", "\\1\\20000000") %>% 
    str_replace( "([0-9]{1,2}).([0-9]{1})B", "\\1\\200000000") %>% 
    str_replace( "([0-9]{1,3})B", "\\100000000")
  return( columnVector ) 
}


replaceMillion <- function( columnVector ){
  #order MATTERS 
  columnVector <- columnVector %>%  
    str_replace( "([0-9]{1,3}).([0-9]{2})M", "\\1\\20000") %>% 
    str_replace( "([0-9]{1,3}).([0-9]{1})M", "\\1\\200000") %>% 
    str_replace( "([0-9]{1,3})M", "\\1000000")
  return( columnVector ) 
}

millionConverter <- function( columnVector ){
  #order MATTERS 
  columnVector <- columnVector %>%  
    str_replace( "([0-9]{1,3}).([0-9]{2})M", "\\1\\20000") %>% 
    str_replace( "([0-9]{1,3}).([0-9]{1})M", "\\1\\200000") %>% 
    str_replace( "([0-9]{1,3})M", "\\1000000")
  return( columnVector ) 
}


replaceMillion2 <- function( tibbleRow ){
  #order MATTERS
  tableRow <- 
    tibbleRow %>% 
    pmap(., function(...){ millionConverter( list(...) )} )

  return( tableRow ) 
}

replaceBillion2 <- function( tibbleRow ){
  #order MATTERS
  tableRow <- 
    tibbleRow %>% 
    pmap(., function(...){ billionConverter( list(...) )} )

  return( tableRow ) 
}

