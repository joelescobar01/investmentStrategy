source("data.transfer.lib.R")


MOODY.AAA.BOND.YIELD <-
  "DAAA"
MOODY.BAA.BOND.YIELD <-
  "DBAA"



bonds.AAA.Corporate.Yields <- function( fromDate='2010-01-01', toDate=Sys.Date() ) {
  rate <- 
    fred.Data( MOODY.AAA.BOND.YIELD, 
              from=fromDate, to=toDate ) %>% 
    mutate( symbol = "AAA.grade.yield" )  %>% 
    mutate( rate = price/100 ) 
  return(rate)
}

