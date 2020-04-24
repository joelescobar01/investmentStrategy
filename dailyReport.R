library("quantmod")
source('stockListConstant.R')
source('charting.R')

directory <- paste( getwd(), "/", format(Sys.time(), "%a_%b_%d_%Y"), sep="" )

generateReport <- function( symbol, dir=directory ){
  data1 <- NULL                               # NULL data1
  data1 <- tryCatch(getSymbols(Symbols = symbol,  
                               src = "yahoo", 
                               auto.assign = FALSE),
                    error=function(e){})      # empty function for error handling
  if(is.null(data1)) next()                   # if data1 is still NULL go to next ticker
  
  subdirectory <- paste(dir, symbol, "/", sep="" )
  dir.create(subdirectory, showWarnings = TRUE)
  
  filename <- paste( subdirectory,"RSI.jpg", sep="" )
  jpeg(filename, width = 1250, height = 1000)
  chartRSI(data1, cName=symbol)
  dev.off()
  
  filename <- paste( subdirectory,"Momentum.jpg", sep="")
  jpeg(filename, width = 1250, height = 1000)
  chartMomentum(data1, cName=symbol) 
  dev.off()
  
  filename <- paste( subdirectory, "BBands.jpg", sep="")
  jpeg(filename, width = 1250, height = 1000)
  chartBBands(data1, cName=symbol) 
  dev.off()
  
  
  filename <- paste( subdirectory, "MACD.jpg", sep="")
  jpeg(filename, width = 1250, height = 1000)
  chartMACD(data1, cName=symbol) 
  dev.off()
  
  
  filename <- paste( subdirectory, "ADX.jpg", sep="")
  jpeg(filename, width = 1250, height = 1000)
  print( chartADX(data1, cName=symbol) )
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

