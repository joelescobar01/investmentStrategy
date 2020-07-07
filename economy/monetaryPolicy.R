source("data.transfer.lib.R")
source("var/economicIndicators.R")

federalFunds <- function(fromDate='2016-03-01', toDate=Sys.Date() ){
  indicators <- 
    fred.Data( c( "FEDFUNDS","EFFRVOL"), from=fromDate, to=toDate ) 

  return( indicators ) 
}

reserveRequirements <- function(fromDate='2010-01-01', toDate=Sys.Date() ){
  indicators <- 
    fred.Data( c( "FEDFUNDS","EFFRVOL", "IORR", "IOER"), from=fromDate, to=toDate ) %>% 
    mutate( rate = price/100 ) 

  return( indicators ) 
}


