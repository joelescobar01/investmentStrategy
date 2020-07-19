source("data.transfer.lib.R") 
source("var/economicIndicators.R")
library( broom )
library(lubridate)

#pivot_wider(names_from=symbol, values_from=price) %>% 


PASSENGER.CAR.REGISTRATIONS <- 
  "USASLRTCR03MLSAM" 
GOV.SECURITIES <- 
    c("4WEEK"="DTB4WK", 
      "3MONTH"="DTB3", 
      "6MONTH"="DTB6", 
      "1YEAR"="DGS1" , 
      "2YEAR"="DGS2", 
      "5YEAR"="DGS5", 
      "7YEAR"="DGS7", 
      "10YEAR"="DGS10",
      "20YEAR"="DGS20", 
      "30YEAR"="DGS30" )

economyIndicators <- function(fromDate='2010-01-01', toDate=Sys.Date() ){
  indicators <- 
    fred.Data( c("UNRATE", "FEDFUNDS", 
                 "CPIAUCSL", "GDPC1" , 
                 "DFII5", "DGS5", 
                 "USALOCOCINOSTSAM", "PPIACO" ), from=fromDate, to=toDate ) 
  pmi <- 
    tibble( code="ISM/MAN_PMI", symbol="MAN_PMI" ) %>% 
    quandl.Stock.Prices2(from=fromDate, to=toDate) %>% 
    mutate( symbol = "pmi" ) %>% 
    rename( price = pmi )


  indicators <- 
    bind_rows( indicators, pmi ) %>% 
    mutate( caption= NA_character_ ) 

  return( indicators ) 
}

yieldCurve.Data <- function( fromDate='2010-01-01', toDate=Sys.Date() ){
  
  securities <- 
    fred.Data( unname(GOV.SECURITIES), from=fromDate, to=toDate ) %>%
    group_by(symbol) %>% 
    filter( date == max(date) ) %>% 
    add_column( weeks.to.maturity = c( 4, 13, 26, 52, 104, 260, 365, 521, 1042, 1564 ) )  %>% 
    add_column( name=names(GOV.SECURITIES) ) %>%  
    transmute( symbol, name, price, maturity.date = date + weeks( weeks.to.maturity ) )
  return( securities ) 
}

inflation <- function( indicators ){
  inflation <-
    indicators %>% 
    #filter( symbol == "CPIAUCSL" ) %>% 
    mutate( rate = ( price - lag(price))/lag(price)*100 ) %>% 
    mutate( symbol = "inflation.ROC" ) %>% 
    select( symbol, date, rate )
  return( inflation ) 
}

wholesale.Inflation <- function( indicators ){
  inflation <-
    indicators %>% 
    #filter( symbol == "CPIAUCSL" ) %>% 
    mutate( rate = ( price - lag(price))/lag(price)*100 ) %>% 
    mutate( symbol = "wholesale.inflation" ) %>% 
    select( symbol, date, rate )
  return( inflation ) 
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

oecdIndicator <- function( indicators ){
  oecd <- 
    indicators %>% 
    rename( rate = price ) %>%
    mutate( symbol = "oecd" ) %>% 
    select( symbol, date, everything() ) %>% 
    mutate( caption="Major Leading Indicators in US Economy" ) 

  return(oecd) 
}

unemploymentRate <- function( indicators ) {
  unemployment <- 
    indicators %>% 
    #filter( symbol == "UNRATE" ) %>%
    #mutate( unemployment.change = (rate-lag(rate))/lag(rate) ) %>%
    rename( rate = price ) %>%
    mutate( symbol="unemployment.rate" ) %>% 
    mutate( caption="Broke Azz homies, finger popping their assholes" ) 
    
    return( unemployment ) 
}

realGDP <- function( indicators ){
  gdp <- 
    indicators %>% 
    #filter( symbol == "GDPC1" ) %>% 
    transmute(symbol="real.gdp.ROC",date, rate = (price-lag(price))/lag(price)*100 ) %>%
    mutate( caption = "Gross Domestic Product Output Monthly Change" ) 
    #mutate( monthly.gdp.change = (price-lag(price))/3 ) %>% 
    #mutate( symbol = "real.gdp" ) %>% 
    #pivot_wider( names_from=symbol, values_from=price) %>% 
    #bind_cols( tibble(date =  unique( floor_date( indicators$date, unit="quarter") ) ) )  

  return(gdp) 
}

federalFunds <- function( indicators ) {
  fundRate <- 
    indicators %>%
    mutate( symbol = "fed.fund.rates" ) %>% 
    mutate( caption="Int.Rate banks trade federal funds (balances held at Federal Reserve Banks) with each other overnight" ) %>% 
    rename( rate = price )

  return( fundRate ) 
}

purchaseManufacturing <- function( indicator ){
  pmi <- 
    indicator %>% 
    rename( rate = price ) %>% 
    mutate( caption=" Index > 50, monthly expansion in manuf. sector. Index < 50, monthly contraction, Index = 50, equal balance" )  
  return( pmi ) 
}

economicState <- function( indicators ) {
  fedfunds <- 
    indicators %>% 
    filter( symbol == "FEDFUNDS" ) %>% 
    federalFunds() 

  gdp <- 
    indicators %>% 
    filter( symbol == "GDPC1" ) %>% 
    realGDP()  
  
  pmi <- 
    indicators %>%
    filter( symbol == "pmi" ) %>%  
    purchaseManufacturing() 

  unem <- 
    indicators %>% 
    filter( symbol == "UNRATE" ) %>%  
    unemploymentRate() 

  oecd <- 
    indicators %>% 
    filter( symbol == "USALOCOCINOSTSAM" ) %>% 
    oecdIndicator() 

  cpi <-
    indicators %>% 
    filter( symbol == "CPIAUCSL" ) %>% 
    inflation() 
  
  economy <- 
    bind_rows( fedfunds, gdp, unem, cpi, pmi, oecd ) 

  return( economy )

}

plot.Economy <- function( economy ) {

  p1 <- 
    economy %>% 
    ggplot( aes( x=date, colour=symbol) ) +
    geom_line( aes(y=rate) ) +
    facet_wrap(. ~ caption, scale="free_y" ) +
    scale_x_date( breaks=scales::breaks_width("1 years"),
                 labels=scales::label_date_short() ) +
    guides( colour="none" )

  return(p1)
}

plot.MultipleStocks <- function( stocks ){
  p2 <-
    stocks %>% 
    ggplot( aes(x=date, colour=symbol) ) +
    geom_line( aes(y=adjusted) ) +
    scale_x_date( breaks = scales::breaks_width("2 years"), labels=scales::label_date("'%y") ) +
    guides(colour="none" ) +
    facet_wrap( ~symbol, scale="free" )

  return(p2)
}


#fedsRatesResults.Plot <- 
#  mutate( nominal.rate = DGS5 - DFII5 ) %>% 
#  ggplot( aes(x=date) ) + 
#  geom_line( aes(y=DFII5, colour="Inflation Fixed" ) ) + 
#  geom_line( aes(y=DGS5, colour="Rate" ) ) + 
#  geom_line( aes(y=nominal.rate, colour="Nominal" ) ) + 
#  labs( y="5-Year Treasury Constant Maturity Rate" ) + 
#  scale_x_date(date_breaks = "year", labels = scales::label_date_short(), minor_breaks="3 months") +
#  scale_y_continuous(labels = scales::label_number())

#plot.CPI <- 
#  indicators %>% 
#  ggplot( aes(x=date) ) + 
#  geom_line( aes(y=inflation.percent ) ) + 
#  labs( y=glue::glue( "2007/12-2009/06 = 100, Index Inflation Percent" )) + 
#  scale_x_date(date_breaks = "year", labels = scales::label_date_short(), minor_breaks="3 months") +
#  scale_y_continuous(labels = scales::label_percent())

#plot.Unemployment <- 
#  indicators %>% 
#  ggplot( aes(x=date) ) + 
#  geom_line( aes(y=unemployment.rate ) ) + 
#  labs( y=glue::glue( "Unemployment Rate" )) + 
#  scale_x_date(date_breaks = "year", labels = scales::label_date_short(), minor_breaks="3 months") +
#  scale_y_continuous( minor_breaks=0:25, labels = scales::label_number() )


