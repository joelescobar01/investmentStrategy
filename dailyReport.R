library("quantmod")
source('stockListConstant.R')
source('charting.R')

directory <- paste(format(Sys.time(), "%a_%b_%d_%Y"), "/", sep="" )
dir.create(directory, showWarnings = FALSE)

 sym <- readline(prompt="Enter stock Symbol: ")
  data1 <- NULL                               # NULL data1
  data1 <- tryCatch(getSymbols(Symbols = sym,  
                     src = "yahoo", 
                     auto.assign = FALSE),
                   error=function(e){})      # empty function for error handling
  if(is.null(data1)) next()                   # if data1 is still NULL go to next ticker
  
  filename <- paste( format(Sys.time(), "%a_%b_%d_%Y"), "/", ii, "-RSI.jpg", sep="" )
  jpeg(filename, width = 1250, height = 1000)
  chartRSI(data1, cName=ii)
  dev.off()
  
  filename <- paste( format(Sys.time(), "%a_%b_%d_%Y"), "/", ii, "-Momentum.jpg", sep="")
  jpeg(filename, width = 1250, height = 1000)
  chartMomentum(data1, cName=ii) 
  dev.off()
  
  filename <- paste( format(Sys.time(), "%a_%b_%d_%Y"), "/", ii, "-BBands.jpg", sep="")
  jpeg(filename, width = 1250, height = 1000)
  chartBBands(data1, cName=ii) 
  dev.off()
  
    
  filename <- paste( format(Sys.time(), "%a_%b_%d_%Y"), "/", ii, "-MACD.jpg", sep="")
  jpeg(filename, width = 1250, height = 1000)
  chartMACD(data1, cName=ii) 
  dev.off()

  
  filename <- paste( format(Sys.time(), "%a_%b_%d_%Y"), "/", ii, "-ADX.jpg", sep="")
  jpeg(filename, width = 1250, height = 1000)
  print( chartADX(data1, cName=ii) )
  dev.off()



