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
   
    
    #tempData <- 
    #  data1 
    
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



generate.RSI.Report <- function( plotRSI, symbol){ 
  #rsiTbbl <- 
  #  rsi.Interface( stockTbbl )
  filename <- paste( symbol, "RSI.jpg", sep="_" )
  filename <- paste( directory, filename, sep="/" )
  print("Generating RSI Report")
  jpeg(filename, width = 1080, height = 720)
  print(plotRSI)      
  dev.off(  )
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

directory <- paste( getwd(), "research", format(Sys.time(), "%a_%b_%d_%Y"), sep="/" )

daily.Buy.Report <- function( stockTbbl, dir=directory, symbol="DailyBuyReport" ){
  subdirectory <- paste(dir, symbol, "", sep="/" )

    #filename <- paste( subdirectory,"Bar.jpg", sep="" )
    #if(!file.exists(filename) ){
    #print("Generating Bar Report")
    #jpeg(filename, width = 1080, height = 720)
    #plot( chart.BAR( stocktTb ))
    #dev.off()
    #if(file.exists(filename))
    #  print("Finished Report")    
    #else {
    #  print("Had problem saving to file ")
    #}    
  #}#

    filename <- paste( subdirectory,"RSI.jpg", sep="" )
    if(!file.exists(filename) ){
      print("Generating RSI Report")
      jpeg(filename, width = 1080, height = 720)
      plot(chart.RSI( rsiAlarm ))      
      dev.off(  )
      if(file.exists(filename))
        print("Finished Report")    
      else {
        print("Had problem saving to file ")
      }
    }

    filename <- paste( subdirectory,"MACD.jpg", sep="" )
    if(!file.exists(filename) ){
      jpeg(filename, width = 1080, height = 720)
      #plot(generateMACDReport(stockDF,symbol))
      plot(chart.MACD(macdAlarm))
      dev.off()
      if(file.exists(filename))
        print("Finished Report")    
      else {
        print("Had problem saving to file ")
      }
    }
}


