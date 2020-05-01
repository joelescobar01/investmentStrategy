library(TTR)
source("lib/utils.R")
source("var/settings.R")
#Sys.setnev(TZ="UTC")

stockADX <- function( stock, dxCal=14  ){
  #where it is greater 1 if it is above the upper band, and less than 0 when it is below the lower band
  adxData <- cbind( Hi(stock), Cl(stock), Lo(stock) )
  adx <- ADX(adxData, n = dxCal )
  return( na.omit(adx)) 
}

uptrendStrengthXTS <- function( adxDF ){
  # +DMI is above the -DMI, prices are moving up, and ADX measures the strength of the uptrend.
  uptrend <- adxDF[ adxDF$DIp > adxDF$DIn, c("Date", "ADX")]
  uptrendXTS <- dfToTimeSeries2( uptrend[,'ADX'], uptrend[,'Date'])
  return( uptrendXTS)
}

downtrendStrength <- function( adxDF ){
  # When the -DMI is above the +DMI, prices are moving down, and ADX measures the strength of the downtrend.
  
}

adxSignals <- function( adxDF ){
  uptrendDates <- coredata( adxDF[ which( adxDF$DIp > adxDF$DIn ), 'Date' ] ) 
  downtrendDates <- coredata( adxDF[ which( adxDF$DIp < adxDF$DIn ), 'Date' ] )
  
  class(uptrendDates) <- "Date"
  class(downtrendDates) <- "Date" 
  crossDate <- list( "uptrend" = uptrendDates, "downtrend" = downtrendDates )   
  return(crossDate)  
}

signalXValues <- function( adxList, startDate ){
  if( length(adxList) == 0 )
    return(c() )
  xValues <- c()
  xValueIndex = 1 
  for(i in 1:length(adxList) ){
    xValues[xValueIndex] <- businessDayCounter(startDate, adxList[i] )   
    xValueIndex = 1 + xValueIndex
  }
  return( xValues )
}
