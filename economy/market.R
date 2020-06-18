source("data.transfer.lib.R") 
library( broom )
library(lubridate)

CONSUMER.PRICE.INDEX <- 
  "CPIAUCSL"
GDP.PRICE.DEFLATOR <- 
  "A191RI1Q225SBEA"
UNEMPLOYMENT <- 
  "UNRATE" 

consumer.demand.indicators <- function(fromDate='2010-01-01', toDate=Sys.Date() ){
  indicators <- 
    fred.Data( c( CONSUMER.PRICE.INDEX, GDP.PRICE.DEFLATOR, 
                  UNEMPLOYMENT), 
              from=fromDate, to=toDate ) 
  pmi <- 
    tibble( code="ISM/MAN_PMI", symbol="MAN_PMI" ) %>% 
    quandl.Stock.Prices2(from=fromDate, to=toDate) %>% 
    mutate( symbol = "PMI" ) %>% 
    rename( price = pmi )

  indicators <- 
    bind_rows( indicators, pmi ) 
  return( indicators ) 
}

chart.Indicator <- function( indi ){
  p1 <- 
    indi %>% 
    group_by( symbol ) %>% 
    drop_na() %>% 
    group_by(symbol) %>% 
    ggplot( aes( x=date) ) + 
    geom_line( aes(y=price) ) + 
    scale_x_date( breaks = scales::breaks_width("1 years"), 
                 labels = scales::label_date_short() ) + 
    facet_grid( symbol ~., scales="free" ) 

  return(p1 ) 
}

chart.Indicator.ROC <- function( indi ){
  p1 <- 
    indi %>% 
    group_by( symbol ) %>% 
    mutate( rate.change = ((price-lag(price))/lag(price) )) %>% 
    drop_na() %>% 
    group_by(symbol) %>% 
    ggplot( aes( x=date) ) + 
    geom_line( aes(y=rate.change) ) + 
    scale_x_date( breaks = scales::breaks_width("1 years"), 
                 labels = scales::label_date_short() ) + 
    facet_grid(symbol ~. , scales="free" ) 

  return(p1 ) 
}


chart.Quarter.Data <- function( tibb ){
  p1 <- 
    tibb %>%
    mutate( rate.change = ((price-lag(price))/lag(price) )) %>% 
    mutate( quarters = quarters(date) ) %>% 
    ggplot( aes( x=date )) +
    geom_col( aes(y=rate.change,fill=sign(rate.change) ) ) + 
    scale_x_date( date_labels="%b %y", date_breaks="4 months" ) + 
    guides( fill="none" ) + 
    labs(y="Rate of Change", title="GDP Price Deflator" ) + 
    scale_fill_gradient(low="red", high="green")
  return(p1) 
}




