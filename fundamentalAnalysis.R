library( TTR ) 
source("utils.R") 


getAllStock <- function(name){
  stock <-
    getSymbols(name, auto.assign = F ) #access CLose and Open with stock$MOTS.Open
  return( stock )
}

getStockFrom <- function( name, startDate=as.Date(Sys.Date()-90) ){
   stock <-
    getSymbols(name, auto.assign = F, from=startDate ) #access CLose and Open with stock$MOTS.Open
  return( stock )
 
}

getAdjustedStock <- function( stock ){
    return( adjustOHLC( stock ) ) 
}


stocksClosingPrice <- function( stock ){
    stocksCl <- Cl( stock )
    return( stocksCl )  
}

stocksReturns <- function( stockCl ){
    stock_return = as.xts( apply( stock, 1, function(x) {x/stock[1,]}) )
    return( stock_return ) 
}

stocksLogDifference <- function( stockCl ){
    stockChange <- diff( log(stockCl), lag=1 )
    plot(as.zoo(stockChange), screens = 1, lty = 1:3, xlab = "Date", ylab = "Log Difference")
    return( stockChange ) 
}


