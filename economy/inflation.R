source("var/variables.R")
source("data.transfer.lib.R") 
source("var/economicIndicators.R")

inflation <- function( indicators ){
  inflation <-
    indicators %>% 
    #filter( symbol == "CPIAUCSL" ) %>% 
    mutate( rate = ( price - lag(price))/lag(price)*100 ) %>% 
    mutate( symbol = "inflation.ROC" ) %>% 
    select( symbol, date, rate )
  return( inflation ) 
}

inflation.Returns <- function( stockReturn, inflationRate ){
  inflationReturn <-
    (stockReturn+1)/(inflationRate+1) 
  return( inflationReturn-1 )
}

treasury.inflation.rate <- function(){
  inflation <- 
    GOV.SECURITIES[c("5YEAR", "5YEARTIPS")] %>% 
    unname %>% 
    fred.Data() %>% 
    group_by(symbol) %>% 
    pivot_wider( names_from=symbol, values_from=price ) %>% 
    rename( nominal=DGS5, real=DFII5 ) %>% 
    mutate( inflation.rate = ( 1 + (nominal*0.01))/(1 + (real*0.01) ) -1 ) %>% 
    mutate( percent = inflation.rate ) %>% 
    mutate( symbol = "treasury.inflation" ) %>% 
    select( symbol, everything() ) 
  return( inflation ) 
}

consumer.price.inflation <- function( indicators=fred.Data("CPIAUCSL") ){
  inflation <-
    indicators %>% 
    mutate( inflation.rate = ( price - lag(price))/lag(price)*100 ) %>% 
    mutate( symbol = "consumer.price.index" )  
  return( inflation ) 
}

producer.price.inflation <- function( indicators=fred.Data("PPIACO") ){
  inflation <-
    indicators %>% 
    mutate( inflation.rate = ( price - lag(price))/lag(price)*100 ) %>% 
    mutate( symbol = "produce.price.index" )  
  return( inflation ) 
}

market.inflation <- function( fromD=ymd("2010-01-01"), toD=Sys.Date()){
  cpi <- 
    consumer.price.inflation(fred.Data(CONSUMER.PRICE.INDEX, from=fromD, to=toD ) ) %>% 
    rename( cpi.inflation = inflation.rate, cpi.price=price ) %>% 
    select( -symbol ) 
  ppi <- 
    producer.price.inflation(fred.Data(PRODUCER.PRICE.INDEX, from=fromD, to=toD )) %>% 
    rename( ppi.inflation = inflation.rate, ppi.price=price ) %>% 
    select( -symbol ) 

  market <- 
    left_join( cpi, ppi, by='date' ) %>% 
    mutate( symbol = "market.inflation" ) %>% 
    select( symbol, everything() ) 

  return( market ) 
}

annualInflation <- function( indicators=inflation.Rates(fromDate), fromDate=ymd("2015-01-01") ){
  annualInflation <- 
    indicators %>% 
    drop_na() %>% 
    ungroup() %>% 
    group_by( year=year(date) ) %>% 
    filter( date==min(date)|date==max(date) ) %>% 
    select( -rate ) %>% 
    pivot_wider(names_from=symbol, values_from=price) %>% 
    summarise_at( c("cpi", "ppi"), diff ) %>% 
    rename( cpi.inflation=cpi, ppi.inflation=ppi )

  return( annualInflation ) 
}

purchasingPower <- function( indicators=inflation.Rates(fromDate), fromDate=ymd("2015-01-01") ){
  purchasePower <- 
    indicators %>% 
    group_by( year=year(date) ) %>% 
    filter( date==min(date)|date==max(date) ) %>% 
    select( -rate ) %>% 
    pivot_wider(names_from=symbol, values_from=price) %>% 
    summarise_at( c("cpi", "ppi"), diff ) %>% 
    rename( cpi.inflation=cpi, ppi.inflation=ppi )

  return( purchasePower) 
}
