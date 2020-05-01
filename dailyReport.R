source('chartIndicators/charts.R')
source("lib/utils.R")
source("analysis/riskAnalysis.R")

generateCandlestickReport <- function (stock, symbol){
  return( chartCandlesticks( stock, plotTitle=symbol) )
}

generateRSIReport <- function( stock, symbol ){
  return( chartRSI(stock, plotTitle=symbol) )
}

generateMomentumReport <- function( stock, symbol ){
  return( chartMomentum(stock, plotTitle=symbol) ) 
}


generateBBandsReport <- function( stock, symbol  ){
  return( chartBBands2(stock, plotTitle=symbol)  ) 
}

generateMACDReport <- function( stock, symbol ){
  if( nrow(stock) < 26 ) 
    return() 
  return( chartMACD(stock, plotTitle=symbol)  ) 
}

generateADXReport <- function( stock, symbol  ){
  attempt(return( chartADX(stock, plotTitle=symbol) ), msg = "Nop !")
}

generateWPRReport <- function( stock, symbol  ){
  return( chartWPR(stock, plotTitle=symbol) )
}

generateVolumeReport <- function( stock, symbol ){
  return( chartVolume(stock,plotTitle=symbol) )
}

generateVolatilityReport <- function( stock, symbol ){
  return( chartVolatilityCloseToClose(stock,plotTitle=symbol) )
}

generateSeasonReport <- function (stock, symbol ){
  return( chartSeasons( stock, plotTitle=symbol) )
}

saveToFile <- function( filename, FUN , args){
  print("Starting Report")
  jpeg(filename, width = 1080, height = 720)
  do.call(FUN, args)
  dev.off()
}

directory <- paste( getwd(), "research", format(Sys.time(), "%a_%b_%d_%Y"), sep="/" )


generateTest <- function( symbol, dir=directory ){
  symbol <- toupper(symbol)
  
  subdirectory <- paste(dir, symbol, "", sep="/" )
  dir.create(subdirectory, showWarnings = FALSE)
  
  stock <- getStock( symbol)
  stockDF <- convertStockToDataFrame(stock, symbol)
  
  filename <- paste( subdirectory,"RSI.jpg", sep="" )
  print(filename)
  if(!file.exists(filename) ){
    print("Generating RSI Report")
    jpeg(filename, width = 1080, height = 720)
    plot(generateRSIReport(stock, symbol))
    dev.off()
    if(file.exists(filename))
      print("Finished RIS Report")    
    else {
      print("Had problem saving to file ")
    }
  }
}

generate3MReport <- function( symbol, dir=directory ){
  symbol <- toupper(symbol)
  
  subdirectory <- paste(dir, symbol, "", sep="/" )
  dir.create(subdirectory, showWarnings = FALSE)
  
  stock <- getStock( symbol, from="2019-08-04" )
  stockDF <- convertStockToDataFrame(stock, symbol)
  
  
  if( nrow(stockDF) < 26 )
    return(NA)

  
  #subdirectory <- paste(dir, symbol, "", sep="/" )
   

  filename <- paste( subdirectory,"Candlesticks.jpg", sep="" )
  if(!file.exists(filename) ){
    print("Generating Candlestick Report")
    jpeg(filename, width = 1080, height = 720)
    plot(generateCandlestickReport(stockDF, symbol))
    dev.off()
    if(file.exists(filename))
      print("Finished Report")    
    else {
      print("Had problem saving to file ")
    }    
  }

  
  filename <- paste( subdirectory,"RSI.jpg", sep="" )
  if(!file.exists(filename) ){
    print("Generating RSI Report")
    jpeg(filename, width = 1080, height = 720)
    plot(generateRSIReport(stock, symbol))
    dev.off(  )
    if(file.exists(filename))
      print("Finished Report")    
    else {
      print("Had problem saving to file ")
    }
  }
  
  if(dev.cur() > 1)
    dev.off()
  
  filename <- paste( subdirectory,"Momentum.jpg", sep="" )
  if(!file.exists(filename) ){
    jpeg(filename, width = 1080, height = 720)
    plot(generateMomentumReport(stock,symbol))
    dev.off()
    if(file.exists(filename))
      print("Finished Report")    
    else {
      print("Had problem saving to file ")
    }
  }
   
  filename <- paste( subdirectory,"BBands.jpg", sep="" )
  if(!file.exists(filename) ){ 
    jpeg(filename, width = 1080, height = 720)
    plot(generateBBandsReport(stock,symbol))
    dev.off()
    if(file.exists(filename))
      print("Finished Report")    
    else {
      print("Had problem saving to file ")
    }
  }
       
  filename <- paste( subdirectory,"MACD.jpg", sep="" )
  if(!file.exists(filename) ){
    jpeg(filename, width = 1080, height = 720)
    plot(generateMACDReport(stockDF,symbol))
    dev.off()
    if(file.exists(filename))
      print("Finished Report")    
    else {
      print("Had problem saving to file ")
    }
  }
  
  
  filename <- paste( subdirectory,"ADX.jpg", sep="" )
  if(!file.exists(filename) ){
    print("Starting ADX Report") 
    jpeg(filename, width = 1080, height = 720)
    plot(generateADXReport(stock,symbol))
    dev.off()
    if(file.exists(filename))
      print("Finished Report")    
    else {
      print("Had problem saving to file ")
    }
  }
  
  filename <- paste( subdirectory,"WPR.jpg", sep="" )
  if(!file.exists(filename) ){
    print("Starting WPR Report") 
    jpeg(filename, width = 1080, height = 720)
    plot(generateWPRReport(stockDF,symbol)  )
    dev.off()
    if(file.exists(filename))
      print("Finished Report")    
    else {
      print("Had problem saving to file ")
    }
  }
  # 
   filename <- paste( subdirectory,"Volume.jpg", sep="" )
   if(!file.exists(filename) ){
     print("Starting Volume Report") 
     jpeg(filename, width = 1080, height = 720)
     plot(generateVolumeReport(stockDF,symbol) )
     dev.off()
     if(file.exists(filename))
       print("Finished Report")    
     else {
       print("Had problem saving to file ")
     }
   }
  # 
}

generateMaterialsReport <- function( symbols=basic_symbols() ){
  source('var/materialSectorList.R')
  dir.create( paste( directory, "/materials", sep=""), showWarnings = FALSE)
  #sym <- as.character( basic_symbols() )
  for(ii in seq_along(symbols)){
    print( paste("Starting Report on:", symbols[ii]) )
    tryCatch( 
        generate3MReport(symbols[ii], directory ), 
        error=function(e){})      # empty function for error handling
    print( paste("Completed Report on:", symbols[ii]) )
  }
}

