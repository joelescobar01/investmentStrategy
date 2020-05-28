library( tidyverse )
library( tidyquant ) 

av_api_key("46JTYXHNWNEYZ90I")
quandl_api_key("xdVQSGYi75oJgntnQbSx") 

quandl.fetch <- 
  function( l ){ quandl.Stock.Prices( l ) } 

alphavantage.fetch <- 
  function( l ){ alphavantage.Stock.Prices( l ) } 

yahoo.fetch <- 
  function( l ){ yahoo.Stock.Prices( l ) } 

# retrieve.stock.data( yahoo.fetch, c("AMZN", "AAPL" ) ) 
retrieve.stock.data <- function( FUN, stockList ){
  stock.Data <- tryCatch({ 
    FUN( stockList ) }, 
  error=function(e){
    print( paste( "Did not retrieve data: ", stockList, collapse="" ) ) 
    return( NA ) 
  })

  return( stock.Data ) 
}


quandl.Stock.List <- function( tickers ){
  quandl.codes <- 
    paste( "WIKI", tickers, sep="/" )
  quandl.stock.list <-
    tibble(
      code= quandl.codes, 
      symbol = tickers )
  return( quandl.stock.list ) 
}

quandl.Stock.Prices <- function( stock.list, ... ){
  stockPrice <- 
    stock.list %>% 
    quandl.Stock.List %>% 
    tq_get(get          = "quandl",
           from         = "2015-01-01",
           collapse     = "daily", 
           complete_cases = TRUE, 
           ... ) %>% 
    select( - code ) 

  return( stockPrice ) 
}

alphavantage.Stock.Prices <- function( stock.list, time.interval="30min",... ){
  interval.stock.prices <- 
    stock.list %>% 
    tq_get(get = "alphavantage", 
           av_fun = "TIME_SERIES_INTRADAY", 
           interval = time.interval, 
           complete_cases = TRUE, 
           ... ) %>% 
    mutate( date = as_date( timestamp ) ) %>% 
    mutate( time = hms::as_hms( timestamp ) )

    return( interval.stock.prices ) 
}

yahoo.Stock.Prices <- function( stock.list, ... ){
  stock.prices <- 
    stock.list %>% 
    tq_get( get = "stock.prices", 
                          complete_cases=TRUE,
                          from="2019-01-01",
                   ... )
  return( stock.prices ) 
}
