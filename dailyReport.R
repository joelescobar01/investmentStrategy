library("quantmod")
source('stockListConstant.R')
source('charting.R')
source("utils.R")
source("riskAnalysis.R")
require(ggplot2)

generateCandlestickReport <- function (stock, symbol){
  print( chartCandlesticks( stock, cName=symbol) )
}

generateSeasonReport <- function (stock, symbol ){
  print( chartSeasons( stock, cName=symbol) )
}

generateRSIReport <- function( stock, symbol ){
  return( chartRSI(stock, cName=symbol) )
}

generateMomentumReport <- function( stock, symbol ){
  return( chartMomentum(stock, cName=symbol) ) 
}


generateBBandsReport <- function( stock, symbol  ){
  return( chartBBands(stock, cName=symbol)  ) 
}

generateMACDReport <- function( stock, symbol ){
  return( chartMACD(stock, cName=symbol)  ) 
}

generateADXReport <- function( stock, symbol  ){
  return( chartADX(stock, cName=symbol) ) 
}

generateWPRReport <- function( stock, symbol  ){
  return( chartWPR(stock, cName=symbol) )
}

generateVolumeReport <- function( stock, symbol ){
  return( chartVolume(stock,cName=symbol) )
}

generateVolatilityReport <- function( stock, symbol ){
  return( chartVolatilityCloseToClose(stock,cName=symbol) )
}


saveToFile <- function( filename, FUN , args){
  print("Starting Report")
  jpeg(filename, width = 1250, height = 1000)
  do.call(FUN, args)
  dev.off()
}

directory <- paste( getwd(), "research", format(Sys.time(), "%a_%b_%d_%Y"), sep="/" )



generateReport <- function( symbol, dir=directory ){
  symbol <- toupper(symbol)
  stock <- getStock( symbol ) 
  if( is.na( stock ) ){
    print( paste("Unable to get stock symbol:", symbol ) )
    return(NA)
  }
   
  subdirectory <- paste(dir, symbol, "", sep="/" )
  dir.create(subdirectory, showWarnings = FALSE) 
  
  if(is.null(stock)) return(NA)                   # if data1 is still NULL go to next ticker
  
  filename <- paste( subdirectory,"Candlesticks.jpg", sep="" )
  print("Generating Candlestick Report")
  jpeg(filename, width = 1250, height = 1000)
  generateCandlestickReport(stock, symbol)
  dev.off()
  print("Completed Candlestick Report")
  
  filename <- paste( subdirectory,"RSI.jpg", sep="" )
  jpeg(filename, width = 1250, height = 1000)
  generateRSIReport(stock, symbol)
  dev.off()
  
  filename <- paste( subdirectory,"Momentum.jpg", sep="" )
  jpeg(filename, width = 1250, height = 1000)
  generateMomentumReport(stock,symbol)
  if( dev.cur() != 1L )
    dev.off()
  
  filename <- paste( subdirectory,"BBands.jpg", sep="" )
  jpeg(filename, width = 1250, height = 1000)
  generateBBandsReport(stock,symbol)
  if( dev.cur() != 1L )
    dev.off()
  
  filename <- paste( subdirectory,"MACD.jpg", sep="" )
  jpeg(filename, width = 1250, height = 1000)
  generateMACDReport(stock,symbol)
  if( dev.cur() != 1L )
    dev.off()
  
  filename <- paste( subdirectory,"ADX.jpg", sep="" )
  print("Starting ADX Report") 
  jpeg(filename, width = 1250, height = 1000)
  print( generateADXReport(stock,symbol) )
  dev.off()
  
  
  filename <- paste( subdirectory,"WPR.jpg", sep="" )
  print("Starting WPR Report") 
  jpeg(filename, width = 1250, height = 1000)
  print( generateWPRReport(stock,symbol) ) 
    dev.off()
  
  filename <- paste( subdirectory,"Volume.jpg", sep="" )
  print("Starting Volume Report") 
  jpeg(filename, width = 1250, height = 1000)
  print( generateVolumeReport(stock,symbol) )
    dev.off()
  
}

generateMaterialsReport <- function( ){
  dir.create( paste( directory, "/materials", sep=""), showWarnings = TRUE)
  source("materialSectorList.R")
  sym <- as.character( basic_symbols() )
  for(ii in seq_along(sym)){
    generateReport(ii, directory)
  }
}

