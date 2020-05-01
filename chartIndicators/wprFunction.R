library(TTR)
source("lib/utils.R")
source("var/settings.R")

stockWPR <- function( stock, lookbackPeriod =  14 ){
  #where it is greater 1 if it is above the upper band, and less than 0 when it is below the lower band
  wpr <- WPR( cbind(  coredata( Hi(stock)), 
                      coredata(Lo(stock)), 
                      coredata(Cl(stock)) ), n=lookbackPeriod ) 
  return(wpr)
}
