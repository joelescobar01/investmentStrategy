source('chartIndicators/macdFunction.R')
source('chartIndicators/rsiFunction.R')
source('chartIndicators/barFunction.R')
source('analysis/riskAnalysis.R')
source('chartIndicators/candlestickFunction.R')
source('analysis/momentumAnalysis.R')
library( ggplot2 )
library( ggpubr )

fromDate <- Sys.Date() - 180

directory <- paste( getwd(), "research", format(Sys.time(), "%a_%b_%d_%Y"), "BUY", sep="/" )


getLatestQuote <- function( ticker ) {
  if( ticker %>% str_detect("-") )
    return(NA) 
  quoteStock <- NULL 
  quoteStock <-tryCatch(  
    getQuote( toupper( ticker ) ), error=function(e){})
  
  if( is.null( quoteStock ) ){
    print(paste("Unable to get latest quote", ticker) )
    return(NA) 
  }

  colnames( quoteStock ) <- 
    c( "date", "close", "change", "change%", "open", "high", "low", "volume" ) 
  
  stock <- quoteStock %>% as_tibble() %>% mutate( date = today() ) %>% select( date, open, high, low, close, volume ) %>% mutate( adjusted = close ) %>% mutate( symbol = toupper(ticker ) ) %>% select( symbol, everything() ) %>% mutate( last.price = close )  

  return(stock) 
}


strategy1Report <- function(  symbols=c() ){
  for(ii in seq_along(symbols)){
    ticker <- symbols[ii]
    print(paste("Starting:", symbols[ii] )) 
    data1 <- NULL                               # NULL data1
    data1 <- tryCatch(tq_get(ticker,  
                               get = "stock.prices", 
                               from="2020-03-01",
                               complete_cases=TRUE),
                    error=function(e){})      # empty function for error handling
    if(is.null(data1)) next()
    if(!is_tibble(data1)) next()  
    
    data1 <- 
      data1 %>% 
      drop_na()
   
    
    data2 <- 
      getLatestQuote( ticker )

    if( is_tibble(data2) ){
      data1 <-
        data1 %>% 
        bind_rows( data2 )  
    }

        buySignal <- 
      data1 %>% 
      Momentum.Buy.Indicator() 
    
    if( !is_tibble( buySignal ) ) 
      next() 
      
    signalCount <- 
      buySignal %>% 
      tail(n=3) %>% 
      filter( buy.indicator == TRUE ) %>% 
      nrow()  
    
    print( ticker ) 
    print( ii ) 
    if( signalCount == 0 ) 
      next() 
    
    print("Signal Found" ) 
    p1 <- chart.BAR( data1, ticker )    
    
    fileName <- paste( ticker, "png", sep="." )
    fileName <- paste( directory, fileName, sep="/") 
    ggsave( fileName, plot=p1 ) 
  
  }

}
