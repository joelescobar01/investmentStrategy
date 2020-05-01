library(TTR)
source("lib/utils.R")
source("var/settings.R")

getMACD <- function( stockDF ){
  macd <- 
    MACD(Cl(stockDF), nFast=12, nSlow=26,
        nSig=9, maType=EMA)
  return( macd)
}
