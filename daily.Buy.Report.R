source('chartIndicators/macdFunction.R')
source('chartIndicators/rsiFunction.R')
source('chartIndicators/barFunction.R')
source('data.transfer.lib.R')
library( ggpubr )

fromDate <- Sys.Date() - 180

directory <- paste( getwd(), "research", format(Sys.time(), "%a_%b_%d_%Y"), sep="/" )

stock.signal.init <- function( symbol.list ){
  stock.data <- 
    retrieve.stock.data( yahoo.fetch, symbol.list ) 
  return( stock.data )
}

stock.signal.setup <- function( stock.data.tbbl ){
  # setup table with correct equations 
  #
  stock.indicators.tbbl <-
    stock.data.tbbl %>% 
    group_by(symbol) %>% 
    GetMACD() %>% 
    GetRSI()

  return( stock.indicators.tbbl ) 
}

stock.signal.min.qty <- function( stock.indicators.tbbl, max.price.per.stock=1000.00 ){
  valid.stocks <-
    stock.indicators.tbbl %>% 
    slice( n() ) %>% 
    filter( close < max.price.per.stock ) %>% 
    select( symbol ) %>% 
    left_join( stock.indicators.tbbl ) 
  return( valid.stocks ) 
}

stock.signal.min.price <- function( stock.indicators.tbbl, min.price.per.stock=5.00 ){
  valid.stocks <-
    stock.indicators.tbbl %>% 
    slice( n() ) %>% 
    filter( close > min.price.per.stock ) %>% 
    select( symbol ) %>% 
    left_join( stock.indicators.tbbl ) 
  return( valid.stocks ) 

}

stock.signal.macd <- function( stock.indicators.tbbl ){
  valid.stocks <-
    stock.indicators.tbbl %>% 
    tail( n=4 ) %>% 
    filter( macd < signal ) %>% 
    select( symbol ) %>% 
    left_join( stock.indicators.tbbl )
  return( valid.stocks ) 
}

stock.signal.rsi <- function( stock.indicators.tbbl ) {
  valid.stocks <-
    stock.indicators.tbbl %>% 
    tail( n=4 ) %>% 
    filter( rsi < 50 ) %>% 
    select( symbol ) %>% 
    left_join( stock.indicators.tbbl )
  return( valid.stocks ) 
}

stock.signal.run <- function( stockList ){
  signal.run.result <- 
    stock.signal.init( stockList ) %>% 
    group_by( symbol ) %>%
    stock.signal.setup %>% 
    stock.signal.min.qty %>% 
    stock.signal.min.price %>% 
    stock.signal.macd %>% 
    stock.signal.rsi 
  return( signal.run.result ) 
}

search.stock.market <- function( ){
  load("stockMarketTickers.Rds")
  stockMarketTickers <-
    stockMarketTickers$Symbol
  stockSymbols <- 
    split( stockMarketTickers, ceiling( seq_along( stockMarketTickers)/20) ) 
  
  stock.data <-
      stock.signal.run( stockSymbols[[1]] ) 

  for(ii in 2 ){
    stock.data <-
      stock.data %>% 
      bind_rows( stock.signal.run( stockSymbols[[ii]] ) )
  }
  return( stock.data )
} 

generateBuyReport <- function(  symbols=c(), purchaseLimit=100.00 ){ 
    

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
