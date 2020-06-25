library( tidyverse ) 
library( tidyquant ) 
library(timetk) 

av_api_key("46JTYXHNWNEYZ90I")
quandl_api_key("xdVQSGYi75oJgntnQbSx") 

fetch.latest.quote <- function( ticker ){
  quot <- 
    getQuote( ticker, 
             what=yahooQF(c(  "Symbol", "Ask", 
                              "Bid", "Last Trade (Price Only)", 
                              "Last Trade Time", "Open", 
                              "Volume", "Previous Close", 
                              "52-week Low", "52-week High", 
                              "Market Capitalization" ))) %>% 
  tk_tbl(preserve_index = FALSE) 

  colnames(quot) <- 
    names( quot ) %>% 
    tolower() %>% 
    str_replace_all( "\\s{1,}", "." ) 

  quot <-
    quot %>%
    mutate( date = as_date( trade.time ) ) %>% 
    mutate( last.trade.time = hms::as_hms( as_datetime( last.trade.time) ) ) %>% 
    mutate( market.cap = market.capitalization ) %>% 
    select( -trade.time, -market.capitalization ) %>% 
    select( symbol, date, everything() ) 

  return(quot) 
}

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
  return( quandl.stock.list ) }

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

quandl.Stock.Prices2 <- function( stock.tibb, ... ){
  stockPrice <- 
    stock.tibb %>% 
    tq_get(get          = "quandl",
           complete_cases = TRUE, 
           ... ) %>% 
    select( - code ) 

  return( stockPrice ) 
}
alphavantage.Stock.Prices.Intraday <- function( stock.list, time.interval="30min",... ){
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

alphavantage.Stock.Prices.Daily <- function( stock.list, ... ){
  interval.stock.prices <- 
    stock.list %>% 
    tq_get(get = "alphavantage", 
           av_fun = "TIME_SERIES_DAILY", 
           complete_cases = TRUE, 
           ... ) %>% 
    mutate( symbol="W5000" ) %>% 
    rename( date=timestamp ) 
    return( interval.stock.prices ) 
}
yahoo.Stock.Prices <- function( stock.list, ... ){
  stock.prices <- 
    stock.list %>% 
    tq_get( get = "stock.prices", 
                          complete_cases=TRUE,
                   ... )
  return( stock.prices ) 
}

yahoo.Dividend.Payout <- function( stock.list, ... ){
  dividen <-
    stock.list %>% 
    tq_get( get='dividends', complete_cases=TRUE, ... ) 

  return( dividen)
}

fred.Data <- function( data.list, ... ){
  data.Fred <- 
    data.list %>% 
    tq_get( get="economic.data", 
            complete_cases=TRUE, 
            ... ) 
    return( data.Fred ) 
}

