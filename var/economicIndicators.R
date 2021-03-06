source("data.transfer.lib.R")
source("var/variables.R")

UNIT.LABOR.COST <- 
  "ULCMFG"
MARKETRETURNS <-
  "SPY"
MARKETHEALTH <- 
  "^W5000" 
UNEMPLOYMENT <- 
  "UNRATE" 
REAL.GDP <- 
  "GDPC1"
CONSUMER.PRICE.INDEX <- 
  "CPIAUCSL"
PRODUCER.PRICE.INDEX <- "PPIACO"
GDP.PRICE.DEFLATOR <- 
  "A191RI1Q225SBEA"
FREIGHT.TRANSPORT.SERVICE <- 
  "TSIFRGHT"
REAL.GDP.PER.CAPITA <- 
  "A939RX0Q048SBEA"
INVENTORY.SALE.RATIO <-
  "ISRATIO"
TRANSPORTATION <-
  c("TSIFRGHT", "TSIPSNGR", "TSITTL" )

recessionLine <- 
  geom_vline( xintercept=c(ymd("2001-03-01"), 
                           ymd("2001-11-01"), 
                           ymd("2007-12-01"), 
                           ymd("2009-06-01") ), 
             linetype="dashed", alpha=0.5 )
  
market.Return <- function( fromDate="2010-01-01", toDate=Sys.Date() ){
  stock <- 
    yahoo.Stock.Prices( c( MARKETRETURNS ), from=fromDate, to=toDate )  
  return( stock ) 
}

market.Health <- function( fromDate="2010-01-01", toDate=Sys.Date() ){
  stock <- 
    yahoo.Stock.Prices( c( MARKETHEALTH ), from=fromDate, to=toDate ) %>%
    mutate( symbol = "W5000" ) 
  return( stock ) 
}

manufacturing.PMI <- function( fromDate='2010-01-01', toDate=Sys.Date() ) {  
  pmi <- 
    tibble( code="ISM/MAN_PMI", symbol="MAN_PMI" ) %>% 
    quandl.Stock.Prices2(from=fromDate, to=toDate) %>% 
    mutate( symbol = "PMI" ) %>% 
    mutate( price = pmi ) 
  return(pmi)
}

unemployment.Rate <- function( fromDate='2010-01-01', toDate=Sys.Date() ) {
  rate <- 
    fred.Data( c(UNEMPLOYMENT), 
              from=fromDate, to=toDate ) %>% 
    mutate( price = price/100 ) 
  return(rate)
}

gdp.Price.Deflator <- function( fromDate='2010-01-01', toDate=Sys.Date() ) {
  gdp <- 
    fred.Data( GDP.PRICE.DEFLATOR, 
              from=fromDate, to=toDate ) 
  return(gdp)
}

consumer.Price.Index <- function( fromDate='2010-01-01', toDate=Sys.Date() ) { cpi <- 
    fred.Data( CONSUMER.PRICE.INDEX , 
              from=fromDate, to=toDate ) %>%
    mutate( symbol="cpi") 
  return( cpi )
}

producer.Price.Index <- function( fromDate='2010-01-01', toDate=Sys.Date() ) { 
  ppi <- 
    fred.Data( PRODUCER.PRICE.INDEX , from=fromDate, to=toDate ) %>% 
    mutate( symbol="ppi") 
  return( ppi )
}

inflation.cpi <- function( fromDate='2010-01-01', toDate=Sys.Date() ){
  inflation <- 
    consumer.Price.Index( fromDate, toDate ) %>% 
    mutate( symbol="inflation.rate", rate = (price-lag(price))/lag(price) ) 
  return(inflation)
}

inflation.Rates <- function( fromDate='2010-01-01', toDate=Sys.Date() ){
  inflation <- 
    consumer.Price.Index() %>% 
    bind_rows( producer.Price.Index() ) %>% 
    group_by(symbol) %>% 
    mutate( rate = (price-lag(price))/lag(price) )
  return(inflation)
}

inflation.Rate2 <- function( dateLimit=list( start='2010-01-01', end=Sys.Date() ) ){
  inflation <- 
    consumer.Price.Index( floor_date( dateLimit[[1]], unit="month" ), dateLimit[[2]] ) %>% 
    mutate( symbol="inflation.rate", rate = (price-lag(price))/lag(price) ) 
  return(inflation)
}
unit.Labor.Cost <- function( fromDate='2010-01-01', toDate=Sys.Date() ) {
  ulc <- 
    fred.Data( UNIT.LABOR.COST , 
              from=fromDate, to=toDate ) %>% 
  mutate( symbol="unit.labor.cost" )
  return( ulc )
}
transport.Service.Index <- function( fromDate='2010-01-01', toDate=Sys.Date() ) {
  tsi <- 
    fred.Data( FREIGHT.TRANSPORT.SERVICE , 
              from=fromDate, to=toDate ) %>% 
  mutate( symbol="TSI" ) 
  return( tsi )
}

real.GDP<- function( fromDate='2010-01-01', toDate=Sys.Date() ) {
  gdp <- 
    fred.Data( REAL.GDP , 
              from=fromDate, to=toDate ) %>% 
    mutate( symbol="real.gdp" ) 
  return( gdp )
}
real.GDP.Per.Capita<- function( fromDate='2010-01-01', toDate=Sys.Date() ) {
  gdp <- 
    fred.Data( REAL.GDP.PER.CAPITA , 
              from=fromDate, to=toDate ) %>% 
    mutate( symbol="real.gdp.per.capita" ) 
  return( gdp )
}

inventory.Sale.Ratio <- function( fromDate='2010-01-01', toDate=Sys.Date() ) {
  isr <- 
    fred.Data( INVENTORY.SALE.RATIO , 
              from=fromDate, to=toDate ) %>% 
    mutate( symbol="inventory.sale.ratio" ) 
  return( isr )
}

tsa.Screening <- function(tsaURL="https://www.tsa.gov/coronavirus/passenger-throughput" ){
  values <- 
    tsaURL %>% 
    html_session() %>% 
    html_node("table") %>% 
    html_table(fill=TRUE, trim=TRUE, header=TRUE ) %>% 
    as_tibble() %>% 
    rename_all( tolower ) %>% 
    mutate( date = mdy(date) ) 


  names( values ) <- c("date", "travelers", "old" )

  oldTravelers <- 
    values %>% 
    transmute( date = date - years(1), travelers = old  ) %>% 
    mutate( travelers = str_replace_all(travelers, ",", "" ) ) %>% 
    mutate( travelers = as.numeric( travelers ) ) %>% 
    drop_na() 

  travelers <- 
    values %>% 
    select( date, travelers ) %>% 
    drop_na() %>% 
    mutate( travelers = str_replace_all(travelers, ",", "" ) ) %>% 
    mutate( travelers = as.numeric( travelers ) ) %>% 
    arrange( by=date ) %>% 
    mutate( annual.change = (travelers-oldTravelers$travelers)/oldTravelers$travelers ) 

  return( travelers ) 
#tsa.Screening() %>% filter( date >= (Sys.Date()-days(30) ) ) %>% mutate(
  #change = (travelers-lag( travelers ))/lag(travelers) ) %>% View()
}
