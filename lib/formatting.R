


formatRecordsNames <- function( webTable ){
  mwTable <- 
    webTable %>%
    str_replace_all( ., ",|\'|\\&|-|\\.+", "" ) %>% 
    str_replace_all( ., "\\s+", "." ) %>% 
    str_replace_all(., "%", NA_character_ )  %>% 
    str_replace_all( ., ",", "" )  %>% 
    str_replace_all( .,"(\\()([0-9]*)(\\))", ".\\2" ) %>% 
    str_replace_all( .,"\\(|\\)", "" ) 
  
  return( mwTable ) 
}


edgarIncomeStatementParser <- function( edgarTable ) {
  edgarData <- 
    edgarTable %>%
    as_tibble() %>% 
    rename( income.statement=Metric ) %>% 
    transmute(  income.statement, 
                amount=as.numeric(Amount), 
                startDate=ymd(startDate), 
                endDate=ymd(endDate) ) %>% 
    mutate( income.statement = formatRecordsNames(income.statement) ) %>% 
    mutate( day.interval = as.numeric( endDate - startDate ) )  %>% 
    filter( day.interval >= 360 ) %>% 
    transmute(  income.statement, 
                amount, year=str_glue("{year(startDate)}-{year(endDate)}" ) ) %>% 
    pivot_wider( names_from=income.statement, values_from=amount )
  return(edgarData ) 
}

edgarBalanceSheetParser <- function( edgarTable ) {
  edgarData <- 
    edgarTable %>%
    as_tibble() %>% 
    rename( balance.sheet=Metric ) %>% 
    transmute(  balance.sheet, 
                amount=as.numeric(Amount), 
                startDate=ymd(startDate),
                endDate=ymd(endDate) ) %>% 
    mutate( balance.sheet = formatRecordsNames(balance.sheet) ) %>% 
    pivot_wider( names_from=balance.sheet, values_from=amount )
  return(edgarData ) 
}




edgarPerShareData <- function( edgarTable, interval=0 ) {
  edgarData <- 
    edgarTable %>% 
    filter( str_detect( Units, "per_shares" ) ) %>% 
    rename( per.share.data = Metric ) %>% 
    transmute(  per.share.data, 
                USD.per.share=as.numeric(Amount), 
                startDate=ymd(startDate), 
                endDate=ymd(endDate) ) %>% 
    mutate( per.share.data = formatRecordsNames(per.share.data) ) %>% 
    filter( as.numeric( endDate - startDate ) > interval ) 


  return( edgarData )
}

edgarShareData <- function( edgarTable ){
  edgarData <- 
    edgarTable %>% 
    filter( str_detect( Units, "^shares" ) ) %>% 
    rename( share.data = Metric ) %>% 
    transmute(  share.data, 
                shares=as.numeric(Amount), 
                startDate=ymd(startDate), 
                endDate=ymd(endDate) ) %>% 
    mutate( share.data = formatRecordsNames(share.data) ) 

  return( edgarData )

}
