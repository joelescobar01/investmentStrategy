source('chartIndicators/macdFunction.R')
source('chartIndicators/rsiFunction.R')
source('chartIndicators/barFunction.R')
source('visual.lib.R')
source('data.transfer.lib.R')
library( ggpubr )

fromDate <- Sys.Date() - 180

directory <- paste( getwd(), "research", format(Sys.time(), "%a_%b_%d_%Y"), sep="/" )

emptyTbbl <- function( stockTbbl ){
  stockTbblCount <- 
    stockTbbl %>% 
    group_by( symbol ) %>% 
    count() %>% 
    filter( n > 0 ) %>% 
    nrow()

  return( stockTbblCount == 0 ) 
}

stock.signal.init <- function( symbol.list ){
  stock.data <- 
    retrieve.stock.data( yahoo.fetch, symbol.list ) %>% 
    drop_na()

  return( stock.data )
}

stock.signal.setup <- function( stock.data.tbbl ){
  # setup table with correct equations 
  stock.indicators.tbbl <-
    stock.data.tbbl %>% 
    group_by(symbol) 

  return( stock.indicators.tbbl ) 
}

stock.signal.min.qty <- function( stock.indicators.tbbl, min.qty=10, max.limit=100.00 ){
  if( stock.indicators.tbbl %>% emptyTbbl() ){
    print("Min. Qty Passed") 
    return( stock.indicators.tbbl ) 
  }
  
  valid.stocks <-
    stock.indicators.tbbl %>% 
    group_by( symbol ) %>% 
    slice( n() ) %>% 
    filter( close*min.qty <= max.limit  ) %>% 
    select( symbol ) %>% 
    ungroup() 

  stock.indicators.tbbl <-
    stock.indicators.tbbl %>% 
    ungroup() %>%  
    right_join( valid.stocks ) %>% 
    group_by( symbol )
  
  return( stock.indicators.tbbl ) 
}

stock.signal.min.price <- function( stock.indicators.tbbl, min.price.per.stock=5.00 ){
  if( stock.indicators.tbbl %>% emptyTbbl() ){
    print("Min. Price Passed") 
    return( stock.indicators.tbbl ) 
  }
  
  valid.stocks <-
    stock.indicators.tbbl %>%
    group_by( symbol ) %>% 
    last() %>% 
    filter( close > min.price.per.stock ) %>% 
    select( symbol )

  stock.indicators.tbbl <-
    stock.indicators.tbbl %>% 
    ungroup() %>%  
    right_join( valid.stocks ) %>% 
    group_by( symbol ) 
  
  return( stock.indicators.tbbl ) 
}

stock.signal.uptrend <- function( stock.indicators.tbbl ){
  if( stock.indicators.tbbl %>% emptyTbbl() ){
    print("Uptrend Passed") 
    return( stock.indicators.tbbl ) 
  }
  stock.indicators.tbbl <-
    stock.indicators.tbbl %>% 
    group_by( symbol ) %>% 
    mutate( short.MA = TTR::SMA( close, n=20 ) ) %>% 
    mutate( long.MA = TTR::SMA( close, n=50 ) ) 

  valid.stocks <- 
    stock.indicators.tbbl %>% 
    slice( (n()-5):n() ) %>% 
    filter( short.MA > long.MA ) %>% 
    select( symbol ) %>% 
    ungroup 

  stock.indicators.tbbl <-
    stock.indicators.tbbl %>% 
    ungroup() %>%  
    right_join( valid.stocks ) %>% 
    group_by( symbol )

  return( stock.indicators.tbbl ) 
}


stock.signal.macd <- function( stock.indicators.tbbl ){
  if( stock.indicators.tbbl %>% emptyTbbl() ){
    print("MACD Passed") 
    return( stock.indicators.tbbl ) 
  }
  
  stock.indicators.tbbl <-
    stock.indicators.tbbl %>%
    group_by(symbol) %>% 
    GetMACD() 
  
  valid.stocks <- 
    stock.indicators.tbbl %>%
    group_by( symbol ) %>% 
    last(n=3) %>% 
    group_by(symbol) %>% 
    filter( macd < signal ) %>% 
    select( symbol ) 

  stock.indicators.tbbl <-
    stock.indicators.tbbl %>% 
    ungroup() %>%  
    right_join( valid.stocks ) %>% 
    group_by( symbol )  
    
  return(stock.indicators.tbbl ) 
}

stock.signal.rsi <- function( stock.indicators.tbbl ) {
  if( stock.indicators.tbbl %>% emptyTbbl() ){
    print("RSI Passed") 
    return( stock.indicators.tbbl ) 
  }
 

  stock.indicators.tbbl <-
    stock.indicators.tbbl %>%
    group_by(symbol) %>% 
    GetRSI() 
  
  valid.stocks <- 
    stock.indicators.tbbl %>% 
    group_by(symbol) %>% 
    last(n=3) %>%
    group_by(symbol) %>% 
    filter( rsi < 70 ) %>%
    group_by(symbol) %>% 
    select( symbol )  
  
  return( stock.indicators.tbbl ) 
}

stock.signal.run <- function( stockList ){
  signal.run.result <- 
    stock.signal.init( stockList ) %>% 
    group_by(symbol) %>% 
    stock.signal.min.qty() %>% 
    group_by(symbol ) %>% 
    stock.signal.min.price() %>% 
    group_by(symbol ) %>%
    stock.signal.macd() %>% 
    group_by(symbol ) %>%
    stock.signal.rsi() 
  return( signal.run.result ) 
}


stock.plot.macd <- function( stock.signal ){
  plotTbbl <- 
    stock.signal %>% 
    do( plot.macd = chart.MACD(., .$symbol) ) 

  return( plotTbbl ) 
}

stock.plot <- function( stock.signal ){
  plotTbbl <- 
    stock.signal %>% 
    do( 
       plot.macd = chart.MACD(., plotTitle = .$symbol, zoomDays=300),
       plot.rsi = chart.RSI(., plotTitle = .$symbol, zoomDays=300),
       plot.bar = chart.BAR(., plotTitle = .$symbol, zoomDays=100 ) ) 

  return( plotTbbl ) 
}

stock.plot.save <- function( stock.plot ){
  
  stock.plot %>% 
    pmap_dfr( function(...){   
              fileName <- paste( directory, ..1, "_Bar.png", sep="", collapse="" ) 
              fileName2 <- paste( directory, ..1,"_Indicator.png", sep="", collapse="") 
              gp1 <- ggarrange( ..2, ..3, nrow=2, ncol=1 )
              ggsave( fileName , plot=..4, 
                      width=19, height=10, units=c("in") )
              ggsave( fileName2, plot=gp1, 
                      width=19, height=10, units=c("in") )
       })
  return()
}


search.stock.market <- function( tickers ){
  stockMarketTickers <-
    tickers  
  stockSymbols <- 
    split( stockMarketTickers, ceiling( seq_along( stockMarketTickers)/20) ) 
  stock.data <-
    NULL
  for(ii in seq_along(stockSymbols) ){
    stock.data <-
      stock.data %>% 
      bind_rows( stock.signal.run( stockSymbols[[ii]] ) ) 
  }
  return( stock.data )
} 

stock.Chart <- function( stock ){ 

    p1 <- chart.BAR( data1, ticker )    
    
    p2 <- 
      data1 %>% 
      chart.MACD( plotTitle = ticker ) 
    p3 <- 
      data1 %>% 
      chart.RSI( plotTitle = ticker ) 

    fileName <- paste( ticker, "_Bar.png", sep="" )
    fileName2 <- paste( ticker,"_Indicator.png", sep="") 
    fileName3 <- paste( ticker,"_Analysis.png", sep="") 
    fileName <- paste( directory, fileName, sep="/") 
    fileName2 <- paste( directory, fileName2, sep="/") 
    fileName3 <- paste( directory, fileName3, sep="/") 

    closePrice <- paste( "Latest Close Price:", closePrice ) 
    gp1 <- 
      ggarrange(  p2, p3, 
                align=c("v"), nrow=2, ncol=1  ) %>% 
      annotate_figure( bottom=text_grob( closePrice, color="green", face="italic", size=10), 
                        fig.lab = ticker, fig.lab.face = "bold")
    ggsave( fileName, plot=p1, width=20, height=9, units=c("in") ) 
    ggsave( fileName2, plot=gp1, width=20, height=9, units=c("in")) 
}
  


# c() %>% generateBuyReportBatch() %>% group_by( symbol ) %>% filter(
# !is.na(macd.indictor) & !is.na(rsi.indicator)) %>% do( plot=printTibblePlot(.) ) 

generateBuyReportBatch <- function(  symbols=c(), purchaseLimit=150.00 ){
    data1 <- NULL                               # NULL data1
    data1 <- tryCatch(tq_get( symbols,  
                               get = "stock.prices", 
                               from=fromDate,
                               complete_cases=TRUE) %>% 
              group_by( symbol ),
              error=function(e){})      # empty function for error handling

    data1 <- 
      data1 %>% 
      group_by( symbol ) %>% 
      GetMACD() %>%
      GetRSI() %>% 
      mutate( rsi.buy.signal = rsi < 50 ) %>% 
      mutate( macd.buy.signal = macd < signal ) %>%
      mutate( buy.signal = rsi.buy.signal & macd.buy.signal ) 

    buySymbols <- 
      data1 %>% 
      filter( row_number() >= (n()-5) & buy.signal ) %>% 
      select( symbol ) %>% unique %>% pull 
    
    data1 <- 
      data1 %>% 
      ungroup %>% 
      filter( symbol == buySymbols )    
    
    return( data1 ) 
}


generateBuyReportBatch2 <- function(  symbols=c(), purchaseLimit=150.00 ){
    data1 <- NULL                               # NULL data1
    data1 <- tryCatch(tq_get( symbols,  
                               get = "stock.prices", 
                               from=fromDate,
                               complete_cases=TRUE) %>% 
              group_by( symbol ),
              error=function(e){})      # empty function for error handling
  data1.macd <- 
      data1 %>% 
      group_by( symbol ) %>% 
      do( macd.indicator = signal.Buy.MACD( . ) ) %>% 
      mutate( macd.indicator = unlist( macd.indicator ) ) 

  data1.rsi <- 
      data1 %>% 
      group_by( symbol ) %>% 
      do( rsi.indicator = signal.Buy.RSI( . )  ) %>% 
      mutate( rsi.indicator = unlist( rsi.indicator ) ) 

  data1.momentum <- 
      data1 %>% 
      group_by( symbol ) %>% 
      do( momentum.indicator = signal.Buy.Momentum( . )  ) %>% 
      mutate( momentum.indicator = unlist( momentum.indicator ) ) 


  dataSignal <-
      full_join( data1.macd, data1.rsi )
  dataSignal <- 
    full_join( dataSignal, data1.momentum ) 


  return( dataSignal ) 
}

printTibblePlot <- function( tibblePlot ){
  fileName <- 
    paste( tibblePlot$symbol, "_MACD", ".png", sep="", collapse="" ) 
  fileName <- paste( directory, fileName, sep="/") 
  p1 <- 
    tibblePlot$macd.indicator[[1]] 
  if( !is.na(p1) ) 
    ggsave( fileName, plot=chart.MACD(p1)  ) 

  fileName <- 
    paste( tibblePlot$symbol, "_RSI", ".png", sep="", collapse="" ) 
  fileName <- paste( directory, fileName, sep="/") 
  p1 <- 
    tibblePlot$rsi.indicator[[1]] 
  if( !is.na(p1) ) 
    ggsave( fileName, plot=chart.RSI(p1)  ) 
}
