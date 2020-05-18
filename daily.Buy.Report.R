source('chartIndicators/macdFunction.R')
source('chartIndicators/rsiFunction.R')
source('chartIndicators/barFunction.R')
source('analysis/riskAnalysis.R')
source('chartIndicators/candlestickFunction.R')
library( ggplot2 )
library( ggpubr )

fromDate <- Sys.Date() - 180

directory <- paste( getwd(), "research", format(Sys.time(), "%a_%b_%d_%Y"), sep="/" )

buyReportMaterials <- function(){
  directory <- paste( directory, "materials", sep="/" )
  dir.create(directory, showWarnings = FALSE, recursive = FALSE, mode = "0777" ) 
  source('var/materialSectorList.R')
  generateBuyReport(symbols=basic_symbols() )
}

generateBuyReport <- function(  symbols=c() ){
  for(ii in seq_along(symbols)){
    ticker <- symbols[ii]
    data1 <- NULL                               # NULL data1
    data1 <- tryCatch(tq_get(ticker,  
                               get = "stock.prices", 
                               from=fromDate,
                               complete_cases=TRUE),
                    error=function(e){})      # empty function for error handling
    
    if(is.null(data1)) next()
    
    print(ticker) 

    macd.Signal <- 
      signal.Buy.MACD( data1, symbol=ticker )
    rsi.Signal <- 
      signal.Buy.RSI( data1, symbol=ticker )
    
    filePath <- paste( directory, ticker, sep="/" )
    
    if( is.na(macd.Signal) )
      next() 
    
    if( is.na(rsi.Signal) )
      next()
    
    risk <- 
      data1 %>%  
      riskAnalysis  

    print(filePath)
    
    fileName <- paste( filePath, "MACD.png", sep="_")
    ggsave( fileName, plot=macd.Signal )
    
    fileName <- paste( filePath, "RSI.png", sep="_" )
    ggsave( fileName, plot=rsi.Signal ) 
        
    fileName <- paste( directory, "BAR.png", sep="_") 
    p1 <- chart.BAR( data1, ticker )    
    ggsave( fileName, plot=p1 ) 
  }
}

generateBuyReport2 <- function(  symbols=c(), purchaseLimit=150.00 ){
  validTickers <- c() 
  for(ii in seq_along(symbols)){
    ticker <- symbols[ii]
    data1 <- NULL                               # NULL data1
    data1 <- tryCatch(tq_get(ticker,  
                               get = "stock.prices", 
                               from=fromDate,
                               complete_cases=TRUE),
                    error=function(e){})      # empty function for error handling
    if(is.null(data1)) next()
    if(!is_tibble(data1)) next()  
    
    data1 <- 
      data1 %>% 
      drop_na() 

    print(ticker) 
    
    macd.Signal <- 
      signal.Buy.MACD( data1, symbol=ticker )
    rsi.Signal <- 
      signal.Buy.RSI( data1, symbol=ticker )
    
    print(ii)

    if( is.na(macd.Signal) )
      next() 
    
    if( is.na(rsi.Signal) )
      next()
  
    returnRate <- 
      data1 %>%
      filter( date >= "2020-03-20" ) %>% #adjust for the coronavirus  
      DailyAnnualReturnRate() 


    if( returnRate < 1 )
      next() 
    
    closePrice <- data1 %>% select( close ) %>% last %>% pull() 
    
    securityQty <- 
      MaxQtyBuy( closePrice, purchaseLimit ) 
    
    if( securityQty < 10 )
      next() 

    roE <- 
      stopLossAnalysis( data1 )

    maxLoss <-
      roE %>% 
      select( "1.65%" ) %>% 
      pull * securityQty
    
    if( maxLoss > 1500*0.02  )
      next() 
    
    dailyAvgYield <-
      roE %>% 
      map( function(x) x*(0.5*securityQty) ) %>% 
      as_tibble_row() 
		
		dailyAvgYield <-
			roE %>% 
		  map( function(x) x*(securityQty) ) %>% 
      as_tibble_row() 
		
    dailyAvgYield <-
      roE %>% 
      add_row( dailyAvgYield ) %>%
			mutate_all( funs(round(., 4))) %>%
			select( qty, everything() ) 
    
		validTickers <-  
      validTickers %>% 
      append( ticker ) 

    p1 <- chart.BAR( data1, ticker )    
    fileName <- paste( ticker, "png", sep="." )
    fileName2 <- paste( ticker,"ROE.png", sep="_") 
    fileName3 <- paste( ticker,"Analysis.png", sep="_") 
    fileName <- paste( directory, fileName, sep="/") 
    fileName2 <- paste( directory, fileName2, sep="/") 
    fileName3 <- paste( directory, fileName3, sep="/") 

    returnRate <- paste( "Daily Return Rate:", returnRate ) 
    closePrice <- paste( "Latest Close Price:", closePrice ) 
    gp1 <- 
      ggarrange(  macd.Signal, rsi.Signal, 
                align=c("v"), nrow=2, ncol=1  ) %>% 
      annotate_figure( bottom=text_grob( closePrice, color="green", face="italic", size=10), 
                        fig.lab = ticker, fig.lab.face = "bold")
    
    ggsave( fileName, plot=gp1 ) 
  
    g1 <- 
      data1 %>% 
      tail(n=7) %>% 
      PastCumulativeReturns() %>% 
      ChartPastCummulativeReturns()
    p2 <- 
      data1 %>% 
      tail(n=7) %>% 
      chart.Price.Daily( ) 

    gp2 <- 
      ggarrange( p1, ggarrange( p2, g1, ncol=2 ),   
                  nrow=2, ncol=1  ) %>% 
      annotate_figure( top = text_grob( returnRate, 
                                       color = "red", 
                                       face = "bold", 
                                       size = 14),
                        bottom=text_grob("7 Trading Days Returns", color='green', face='italic', size=10), 
                        fig.lab = ticker, fig.lab.face = "bold")
   ggsave( fileName2, plot=gp2 )  
    
   p3 <- 
      TimePeriodVolatility( ticker )  %>% 
      ChartTimePeriodVolatility()       
   p4 <-
     ggtexttable( dailyAvgYield, 
                  rows=NULL, cols = colnames(dailyAvgYield )) + theme_void() 
    gp3 <-
      ggarrange( p3, p4, 
                nrow=2, ncol=1, heights=c(2,1) ) %>% 
    annotate_figure( bottom=text_grob( "Average Daily Return", color='green', face="bold", size=10), 
											fig.lab = ticker, fig.lab.face = "bold" )  
    
    ggsave(fileName3, plot=gp3) 
  }

  return( validTickers ) 
}


