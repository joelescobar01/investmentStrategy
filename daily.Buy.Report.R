source('chartIndicators/macdFunction.R')
source('chartIndicators/rsiFunction.R')
source('chartIndicators/barFunction.R')
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
    data1 <- NULL                               # NULL data1
    data1 <- tryCatch(tq_get(symbols[ii],  
                               get = "stock.prices", 
                               from=fromDate,
                               complete_cases=TRUE),
                    error=function(e){})      # empty function for error handling
    
    if(is.null(data1)) next() 
    if( data1 %>% tally() < 30 )
      next()
    
    macd.Signal <- 
      signal.Buy.MACD( data1 )
    rsi.Signal <- 
      signal.Buy.RSI( data1 )
    
    if( is.na(rsi.Signal) )
      next()
  
    if( is.na(macd.Signal) )
      next() 
    
    print( paste("Starting Report on:", symbols[ii]) )
    generate.MACD.Report( macd.Signal, symbol=symbols[ii] )  
    generate.RSI.Report( rsi.Signal, symbol=symbols[ii] ) 
    generate.BAR.Report( data1, symbol=symbols[ii] ) 
      
  }
}

generate.MACD.Report <- function( plotMACD, symbol){
  #macdTbbl <- 
  #  macd.Interface( stockTbbl ) 
  filename <- paste( symbol, "MACD.jpg", sep="_" )
  filename <- paste( directory, filename, sep="/" )
  print("Generating MACD Report")
  jpeg(filename, width = 1080, height = 720)
  print( plotMACD )      
  dev.off(  )
}

generate.BAR.Report <- function( stockTbbl, symbol){
  filename <- paste( symbol, "BAR.jpg", sep="_" )
  filename <- paste( directory, filename, sep="/" )
  print("Generating BAR Report")
  jpeg(filename, width = 1080, height = 720)
  plot( chart.BAR( stockTbbl, symbol ))      
  dev.off(  )
}

